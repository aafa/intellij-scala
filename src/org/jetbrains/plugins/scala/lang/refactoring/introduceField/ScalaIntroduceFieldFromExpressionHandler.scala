package org.jetbrains.plugins.scala
package lang
package refactoring
package introduceField

import com.intellij.codeInsight.navigation.NavigationUtil
import com.intellij.ide.util.PsiClassListCellRenderer
import com.intellij.internal.statistic.UsageTrigger
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.markup.RangeHighlighter
import com.intellij.openapi.editor.{Document, Editor}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi._
import com.intellij.psi.search.PsiElementProcessor
import com.intellij.psi.util.PsiTreeUtil.isAncestor
import com.intellij.refactoring.HelpID
import org.jetbrains.plugins.scala.extensions.{PsiElementExt, childOf}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScEarlyDefinitions
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.{ScExtendsBlock, ScTemplateParents}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory._
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.util.ScalaUtils


/**
  * Nikolay.Tropin
  * 6/27/13
  */
class ScalaIntroduceFieldFromExpressionHandler extends ScalaRefactoringActionHandler {

  val REFACTORING_NAME: String = ScalaBundle.message("introduce.field.title")

  private var occurrenceHighlighters = Seq.empty[RangeHighlighter]

  def invoke(file: PsiFile, startOffset: Int, endOffset: Int)
            (implicit project: Project, editor: Editor): Unit = {
    try {
      UsageTrigger.trigger(ScalaBundle.message("introduce.field.id"))

      trimSpacesAndComments(editor, file)
      PsiDocumentManager.getInstance(project).commitAllDocuments()
      writableScalaFile(file, REFACTORING_NAME)

      val (expr, types) = getExpressionWithTypes(file, startOffset, endOffset).getOrElse {
        showErrorHintImpl(ScalaBundle.message("cannot.refactor.not.expression"))
        return
      }

      afterClassChoosing(expr, types, file, "Choose class for Introduce Field")
    }
    catch {
      case _: IntroduceException =>
    }
  }


  override def invoke(file: PsiFile)
                     (implicit project: Project, editor: Editor, dataContext: DataContext): Unit = {
    afterExpressionChoosing(file, REFACTORING_NAME) {
      invoke(file, editor.getSelectionModel.getSelectionStart, editor.getSelectionModel.getSelectionEnd)
    }
  }

  private def afterClassChoosing(expression: ScExpression,
                                 types: Array[ScType], file: PsiFile, title: String)
                                (implicit project: Project, editor: Editor): Unit =
    ScalaPsiUtil.getParents(expression, file).collect {
      case t: ScTemplateDefinition => t
    } match {
      case Nil =>
      case head :: Nil => convertExpressionToField(new IntroduceFieldContext(file, expression, types, head))
      case classes =>
        val processor = new PsiElementProcessor[PsiClass] {
          def execute(aClass: PsiClass): Boolean = {
            convertExpressionToField(new IntroduceFieldContext(file, expression, types, aClass.asInstanceOf[ScTemplateDefinition]))
            false
          }
        }
        val renderer = new PsiClassListCellRenderer() {
          override def getElementText(element: PsiClass): String =
            super.getElementText(element).replace("$", "")
        }

        NavigationUtil.getPsiElementPopup(classes.toArray[PsiClass], renderer, title, processor, classes.head).showInBestPositionFor(editor)
    }

  private def convertExpressionToField(ifc: IntroduceFieldContext): Unit = {
    implicit val project: Project = ifc.project
    implicit val editor: Editor = ifc.editor

    checkCanBeIntroduced(ifc.expression) match {
      case Some(message) => showErrorHintImpl(message)
      case _ =>
        val settings = new IntroduceFieldSettings(ifc)
        if (settings.canBeInitInDeclaration || settings.canBeInitLocally) {
          if (getDialog(ifc, settings).isOK) {
            runRefactoring(ifc, settings)
          }
        } else {
          showErrorHintImpl("Cannot create field from this expression")
        }
    }
  }

  private def runRefactoringInside(ifc: IntroduceFieldContext, settings: IntroduceFieldSettings) {
    implicit val project: Project = ifc.project
    implicit val editor: Editor = ifc.editor

    import ScalaIntroduceFieldFromExpressionHandler._

    val expression = expressionToIntroduce(ifc.expression)
    val mainOcc = ifc.occurrences.filter(_.getStartOffset == editor.getSelectionModel.getSelectionStart)
    val occurrencesToReplace = if (settings.replaceAll) ifc.occurrences else mainOcc
    val extendsBlock = ifc.extendsBlock

    anchorForNewDeclaration(expression, occurrencesToReplace, extendsBlock) match {
      case null =>
        showErrorHintImpl("Cannot find place for the new field")
        return
      case _ =>
    }

    val name = settings.name
    val typeName = Option(settings.scType).map(_.canonicalText).getOrElse("")
    val replacedOccurrences = replaceOccurences(occurrencesToReplace, name, ifc.file)

    val anchor = anchorForNewDeclaration(expression, replacedOccurrences, extendsBlock)
    val initInDecl = settings.initInDeclaration
    var createdDeclaration: PsiElement = null
    if (initInDecl) {
      createdDeclaration = createDeclaration(name, typeName, settings.defineVar, expression)
    } else {
      val underscore = createExpressionFromText("_")
      createdDeclaration = createDeclaration(name, typeName, settings.defineVar, underscore)

      anchorForInitializer(replacedOccurrences, ifc.file) match {
        case Some(anchorForInit) =>
          val parent = anchorForInit.getParent
          val assignStmt = createExpressionFromText(s"$name = ${expression.getText}")
          parent.addBefore(assignStmt, anchorForInit)
          parent.addBefore(createNewLine(), anchorForInit)
        case None => throw new IntroduceException
      }
    }

    settings.visibilityLevel match {
      case "" =>
      case other =>
        val modifier = createModifierFromText(other)
        createdDeclaration.asInstanceOf[ScMember].getModifierList.add(modifier)
    }

    lazy val document: Document = editor.getDocument

    anchor match {
      case (_: ScTemplateParents) childOf (extBl: ScExtendsBlock) =>
        val earlyDef = extBl.addEarlyDefinitions()
        createdDeclaration = earlyDef.addAfter(createdDeclaration, earlyDef.getFirstChild)
      case _ childOf (ed: ScEarlyDefinitions) if onOneLine(document, ed.getTextRange) =>
        def isBlockStmtOrMember(elem: PsiElement) = elem != null && (elem.isInstanceOf[ScBlockStatement] || elem.isInstanceOf[ScMember])

        var declaration = createdDeclaration.getText
        if (isBlockStmtOrMember(anchor)) declaration += "; "
        if (isBlockStmtOrMember(anchor.getPrevSibling)) declaration = "; " + declaration
        document.insertString(anchor.getTextRange.getStartOffset, declaration)
        PsiDocumentManager.getInstance(project).commitDocument(document)
      case _ childOf parent =>
        createdDeclaration = parent.addBefore(createdDeclaration, anchor)
        parent.addBefore(createNewLine(), anchor)
    }

    ScalaPsiUtil.adjustTypes(createdDeclaration)
  }

  def runRefactoring(ifc: IntroduceFieldContext, settings: IntroduceFieldSettings) {
    val runnable = new Runnable {
      def run(): Unit = runRefactoringInside(ifc, settings)
    }
    ScalaUtils.runWriteAction(runnable, ifc.project, REFACTORING_NAME)
    ifc.editor.getSelectionModel.removeSelection()
  }

  protected def getDialog(ifc: IntroduceFieldContext, settings: IntroduceFieldSettings): ScalaIntroduceFieldDialog = {
    val occCount = ifc.occurrences.length
    // Add occurrences highlighting
    if (occCount > 1)
      occurrenceHighlighters = highlightOccurrences(ifc.project, ifc.occurrences, ifc.editor)

    val dialog = new ScalaIntroduceFieldDialog(ifc, settings)
    dialog.show()
    if (!dialog.isOK) {
      if (occCount > 1) {
        occurrenceHighlighters.foreach(_.dispose())
        occurrenceHighlighters = Seq.empty
      }
    }
    dialog
  }

  private def onOneLine(document: Document, range: TextRange): Boolean = {
    document.getLineNumber(range.getStartOffset) == document.getLineNumber(range.getEndOffset)
  }

  private def showErrorHintImpl(message: String)
                               (implicit project: Project, editor: Editor): Unit = {
    showErrorHint(message, REFACTORING_NAME, HelpID.INTRODUCE_FIELD)
  }

}

object ScalaIntroduceFieldFromExpressionHandler {

  protected def anchorForNewDeclaration(expr: ScExpression, occurrences: Seq[TextRange], extendsBlock: ScExtendsBlock): PsiElement = {
    val firstOccOffset = occurrences.map(_.getStartOffset).min
    val anchor = statementsAndMembersInClass(extendsBlock).find(_.getTextRange.getEndOffset >= firstOccOffset)
    anchor.orElse {
      if (isAncestor(extendsBlock.templateBody.orNull, commonParent(extendsBlock.getContainingFile, occurrences), false)) None
      else extendsBlock match {
        case ScExtendsBlock.EarlyDefinitions(earlyDef) => earlyDef.lastChild
        case extBl => extBl.templateParents
      }
    }.orNull
  }

  private def anchorForInitializer(occurrences: Seq[TextRange], file: PsiFile)
                                  (implicit context: ProjectContext = file.getManager): Option[PsiElement] = {
    Option(findParentExpr(commonParent(file, occurrences))).map { parent =>
      val firstRange = occurrences.head
      if (!parent.isInstanceOf[ScBlock] && needBraces(parent, nextParent(parent, file))) {
        val replacement = createExpressionFromText(s"{${parent.getText}}")
        (parent.replaceExpression(replacement, removeParenthesis = false), firstRange.shiftRight(1))
      } else (container(parent, file), firstRange)
    }.flatMap {
      case (e, firstRange) => e.getChildren.find(_.getTextRange.contains(firstRange))
    }
  }
}
