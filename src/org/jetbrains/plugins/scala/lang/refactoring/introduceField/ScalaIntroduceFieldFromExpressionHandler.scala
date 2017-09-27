package org.jetbrains.plugins.scala
package lang
package refactoring
package introduceField

import com.intellij.codeInsight.navigation.NavigationUtil
import com.intellij.ide.util.PsiClassListCellRenderer
import com.intellij.openapi.actionSystem.DataContext
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
import org.jetbrains.plugins.scala.lang.refactoring.introduceVariable.IntroduceElement
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._
import org.jetbrains.plugins.scala.project.ProjectContext

/**
  * Nikolay.Tropin
  * 6/27/13
  */
class ScalaIntroduceFieldFromExpressionHandler extends ScalaRefactoringActionHandler with IntroduceElement {

  override private[refactoring] val refactoringName: String = ScalaBundle.message("introduce.field.title")
  override protected val triggerName: String = ScalaBundle.message("introduce.field.id")

  import ScalaIntroduceFieldFromExpressionHandler._

  def invoke(file: PsiFile, startOffset: Int, endOffset: Int)
            (implicit project: Project, editor: Editor): Unit = {
    try {
      trimSpacesAndComments(editor, file)
      trigger(file)

      val (expression, types) = getExpressionWithTypes(file, startOffset, endOffset).getOrElse {
        showErrorHintImpl(ScalaBundle.message("cannot.refactor.not.expression"))
        return
      }

      checkCanBeIntroduced(expression).foreach { message =>
        showErrorHintImpl(message)
        return
      }

      val parents = ScalaPsiUtil.getParents(expression, file).collect {
        case t: ScTemplateDefinition => t
      }
      afterClassChoosing(parents) { clazz =>
        convertExpressionToField(new IntroduceFieldContext(file, expression, clazz), types)
      }
    }
    catch {
      case _: IntroduceException =>
    }
  }


  override def invoke(file: PsiFile)
                     (implicit project: Project, editor: Editor, dataContext: DataContext): Unit = {
    afterExpressionChoosing(file, refactoringName) {
      invoke(file, editor.getSelectionModel.getSelectionStart, editor.getSelectionModel.getSelectionEnd)
    }
  }

  private def afterClassChoosing(parents: List[ScTemplateDefinition])
                                (action: ScTemplateDefinition => Unit)
                                (implicit project: Project, editor: Editor): Unit =
    parents match {
      case Nil =>
      case head :: Nil => action(head)
      case classes =>
        val processor = new PsiElementProcessor[PsiClass] {
          def execute(clazz: PsiClass): Boolean = {
            action(clazz.asInstanceOf[ScTemplateDefinition])
            false
          }
        }
        val renderer = new PsiClassListCellRenderer() {
          override def getElementText(element: PsiClass): String =
            super.getElementText(element).replace("$", "")
        }

        NavigationUtil.getPsiElementPopup(classes.toArray[PsiClass], renderer, "Choose class for Introduce Field", processor, classes.head).showInBestPositionFor(editor)
    }

  private def convertExpressionToField(ifc: IntroduceFieldContext, types: Array[ScType])
                                      (implicit project: Project, editor: Editor): Unit = {
    val settings = new IntroduceFieldSettings(ifc, types.head)
    if (settings.canBeInitInDeclaration || settings.canBeInitLocally) {
      val dialog = new ScalaIntroduceFieldDialog(ifc, settings, types)
      showDialog(dialog, ifc.occurrences).foreach { _ =>
        runRefactoring(ifc, settings)
      }
    } else {
      showErrorHintImpl("Cannot create field from this expression")
    }
  }

  def runRefactoring(ifc: IntroduceFieldContext, settings: IntroduceFieldSettings)
                    (implicit project: Project = ifc.project,
                     editor: Editor = ifc.editor): Unit =
    runRefactoringWithSelection {
      val occurrencesToReplace = ifc.occurrences.filter {
        case _ if settings.replaceAll => true
        case range => range.getStartOffset == editor.getSelectionModel.getSelectionStart
      }

      anchorForNewDeclaration(occurrencesToReplace, ifc.extendsBlock) match {
        case None => showErrorHintImpl("Cannot find place for the new field")
        case _ => runRefactoringInside(ifc, occurrencesToReplace, settings)
      }
    }

  private def showErrorHintImpl(message: String)
                               (implicit project: Project, editor: Editor): Unit = {
    showErrorHint(message, refactoringName, HelpID.INTRODUCE_FIELD)
  }
}

object ScalaIntroduceFieldFromExpressionHandler {

  private def runRefactoringInside(ifc: IntroduceFieldContext,
                                   occurrencesToReplace: Seq[TextRange],
                                   settings: IntroduceFieldSettings)
                                  (implicit project: Project, editor: Editor): Unit = {
    val name = settings.name
    val typeName = Option(settings.scType).map(_.canonicalText).getOrElse("")

    val file = ifc.file
    val replacedOccurrences = replaceOccurences(occurrencesToReplace, name, file)

    val anchor = anchorForNewDeclaration(replacedOccurrences, ifc.extendsBlock).orNull
    val initInDecl = settings.initInDeclaration
    var createdDeclaration: PsiElement = null

    val expression = ifc.expression
    if (initInDecl) {
      createdDeclaration = createDeclaration(name, typeName, settings.defineVar, expression)
    } else {
      val underscore = createExpressionFromText("_")
      createdDeclaration = createDeclaration(name, typeName, settings.defineVar, underscore)

      anchorForInitializer(replacedOccurrences, file) match {
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

  private def onOneLine(document: Document, range: TextRange): Boolean =
    document.getLineNumber(range.getStartOffset) == document.getLineNumber(range.getEndOffset)

  private def anchorForNewDeclaration(occurrences: Seq[TextRange], extendsBlock: ScExtendsBlock): Option[PsiElement] = {
    val firstOccOffset = occurrences.map(_.getStartOffset).min
    val anchor = statementsAndMembersInClass(extendsBlock).find(_.getTextRange.getEndOffset >= firstOccOffset)
    anchor.orElse {
      if (isAncestor(extendsBlock.templateBody.orNull, commonParent(extendsBlock.getContainingFile, occurrences), false)) None
      else extendsBlock match {
        case ScExtendsBlock.EarlyDefinitions(earlyDef) => earlyDef.lastChild
        case extBl => extBl.templateParents
      }
    }
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
