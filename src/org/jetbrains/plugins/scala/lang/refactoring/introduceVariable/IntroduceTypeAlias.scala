package org.jetbrains.plugins.scala
package lang
package refactoring
package introduceVariable

import java.awt.Component
import java.util
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import com.intellij.codeInsight.template.impl.{TemplateManagerImpl, TemplateState}
import com.intellij.codeInsight.unwrap.ScopeHighlighter
import com.intellij.openapi.command.impl.StartMarkAction
import com.intellij.openapi.editor.colors.{EditorColors, EditorColorsScheme}
import com.intellij.openapi.editor.markup._
import com.intellij.openapi.editor.{Editor, SelectionModel}
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.popup.{JBPopupAdapter, JBPopupFactory, LightweightWindowEvent}
import com.intellij.openapi.util.{Key, TextRange}
import com.intellij.psi._
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageUtil
import com.intellij.psi.util.PsiTreeUtil.{findElementOfClassAtRange, getChildOfType, getParentOfType}
import org.jetbrains.plugins.scala.extensions.{inTransactionLater, inWriteAction}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil.{addTypeAliasBefore, adjustTypes}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScStableCodeReferenceElement
import org.jetbrains.plugins.scala.lang.psi.api.base.types._
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.{ScExtendsBlock, ScTemplateBody}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createTypeElementFromText
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._
import org.jetbrains.plugins.scala.lang.refactoring.util.{ConflictsReporter, DefaultListCellRendererAdapter, ScalaDirectoryService, ScalaRefactoringUtil}
import org.jetbrains.plugins.scala.util.JListCompatibility

class IntroduceTypeAlias(protected val conflictsReporter: ConflictsReporter)
  extends IntroduceElement[ScTypeAlias] {

  override protected val refactoringKey: String = "introduce.type.alias.title"
  override protected val triggerKey: String = "introduce.type.alias.id"

  def invokeTypeElement(file: PsiFile, inTypeElement: ScTypeElement)
                       (implicit project: Project, editor: Editor): Unit = {
    try {
      trigger(file)

      if (isInvalid(inTypeElement)) {
        showErrorHintWithException(ScalaBundle.message("cannot.refactor.not.valid.type"), refactoringName)
      }

      val currentDataObject = editor.getUserData(IntroduceTypeAlias.REVERT_TYPE_ALIAS_INFO)

      if (currentDataObject.possibleScopes == null) {
        currentDataObject.setPossibleScopes(ScopeSuggester.suggestScopes(conflictsReporter, project, editor, file, inTypeElement))
      }

      if (currentDataObject.possibleScopes.isEmpty) {
        showErrorHintWithException(ScalaBundle.message("cannot.refactor.scope.not.found"), refactoringName)
      }

      def runWithDialog(fromInplace: Boolean, mainScope: ScopeItem, enteredName: String = ""): Unit = {
        val possibleScopes = currentDataObject.possibleScopes

        val (updatedMainScope, updatedTypeElement) = mainScope match {
          case simpleScope: SimpleScopeItem if fromInplace =>
            val newScope = simpleScope.revalidate(enteredName)
            possibleScopes(possibleScopes.indexOf(mainScope)) = newScope

            val range = currentDataObject.initialTypeElement
            val newTypeElement = findElementOfClassAtRange(file, range.getStartOffset, range.getEndOffset, classOf[ScTypeElement]) match {
              case simpleType: ScSimpleTypeElement if isInvalid(simpleType) => getParentOfType(simpleType, classOf[ScParameterizedTypeElement])
              case typeElement => typeElement
            }

            (newScope, newTypeElement)
          case _: SimpleScopeItem | _: PackageScopeItem => (mainScope, inTypeElement)
          case _ => (possibleScopes(0), inTypeElement)
        }

        this.runWithDialog(updatedTypeElement, possibleScopes, file, updatedMainScope)
      }

      // replace all occurrences, don't replace occurences available from companion object or inheritors
      // suggest to choose scope
      def runInplace(): Unit = {
        def handleScope(scopeItem: SimpleScopeItem): Unit =
          runRefactoring {
            val suggestedNames = scopeItem.availableNames

            val (namedElementReference, typeElementReference) = inWriteAction {
              val allOccurrences = OccurrenceData(scopeItem)
              runRefactoringForTypeInside(file, inTypeElement, suggestedNames.iterator().next(), allOccurrences, scopeItem)
            }

            val maybeTypeAlias = Option(namedElementReference.getElement).collect {
              case typeAlias: ScTypeAliasDefinition => typeAlias
            }

            val maybeTypeElement = Option(typeElementReference.getElement)

            IntroduceElement.withElement(maybeTypeElement) { _ =>
              maybeTypeAlias.foreach { typeAlias =>
                editor.getUserData(IntroduceTypeAlias.REVERT_TYPE_ALIAS_INFO).addScopeElement(scopeItem)

                new ScalaInplaceTypeAliasIntroducer(typeAlias)
                  .performInplaceRefactoring(new util.LinkedHashSet[String](suggestedNames))
              }
            }
          }

        val currentScope = currentDataObject.currentScope

        //need open modal dialog in inplace mode
        if ((StartMarkAction.canStart(project) != null) && (currentScope != null)) {
          currentDataObject.isCallModalDialogInProgress = true
          val templateState: TemplateState = TemplateManagerImpl.getTemplateState(InjectedLanguageUtil.getTopLevelEditor(editor))

          if (templateState != null) {
            templateState.gotoEnd()
          }

          val enteredName = currentDataObject.getNamedElement.getName
          ScalaInplaceTypeAliasIntroducer.revertState(editor, currentDataObject.currentScope, currentDataObject.getNamedElement)

          runWithDialog(fromInplace = true, currentDataObject.currentScope, enteredName)
          //          editor.getUserData(IntroduceTypeAlias.REVERT_TYPE_ALIAS_INFO).clearData()
        } else {
          currentDataObject.setInintialInfo(inTypeElement.getTextRange)

          IntroduceTypeAlias.showTypeAliasChooser(currentDataObject.possibleScopes, ScalaBundle.message("choose.scope.for", refactoringName)) {
            case simpleScope: SimpleScopeItem if simpleScope.usualOccurrences.nonEmpty =>
              handleScope(simpleScope)
            case packageScope: PackageScopeItem =>
              runWithDialog(fromInplace = true, packageScope)
          }
        }
      }

      if (isInplaceAvailable(editor)) runInplace()
      else runWithDialog(fromInplace = false, null)
    }

    catch {
      case _: IntroduceException =>
    }
  }

  private def runRefactoringForTypeInside(file: PsiFile,
                                          typeElement: ScTypeElement,
                                          typeName: String,
                                          occurrences: OccurrenceData,
                                          scope: ScopeItem)
                                         (implicit editor: Editor): (SmartPsiElementPointer[ScTypeAlias], SmartPsiElementPointer[ScTypeElement]) = {
    def addTypeAliasDefinition(typeElement: ScTypeElement, parent: PsiElement) = {
      val text = typeElement.calcType.canonicalText
      val definition = ScalaPsiElementFactory
        .createTypeAliasDefinitionFromText(s"type $typeName = $text", typeElement.getContext, typeElement)

      val anchor = parent.getChildren
        .find(_.getTextRange.contains(typeElement.getTextRange))
        .getOrElse(parent.getLastChild)

      val resultTypeAlias = addTypeAliasBefore(definition, parent, Some(anchor))
      adjustTypes(resultTypeAlias, useTypeAliases = false)
      resultTypeAlias
    }

    val revertInfo = ScalaRefactoringUtil.RevertInfo(file.getText, editor.getCaretModel.getOffset)
    editor.putUserData(ScalaIntroduceVariableHandler.REVERT_INFO, revertInfo)

    val parent = scope match {
      case simpleScope: SimpleScopeItem => simpleScope.fileEncloser
      case packageScope: PackageScopeItem =>
        packageScope.fileEncloser match {
          case suggestedDirectory: PsiDirectory =>
            createAndGetPackageObjectBody(typeElement, suggestedDirectory, packageScope.needDirectoryCreating, scope.name)
          case encloser => encloser
        }
    }

    val typeAlias = addTypeAliasDefinition(occurrences.allOccurrences.head, parent)
    Option(editor.getUserData(IntroduceTypeAlias.REVERT_TYPE_ALIAS_INFO))
      .foreach(_.setTypeAlias(typeAlias))

    def replaceWith(typeElement: ScTypeElement, name: String = typeName): ScTypeElement = {
      //remove parenthesis around typeElement
      if (typeElement.getParent.isInstanceOf[ScParenthesisedTypeElement]) {
        typeElement.getNextSibling.delete()
        typeElement.getPrevSibling.delete()
      }

      //avoid replacing typeElement that was replaced
      val resultTypeElement = if (typeElement.calcType.presentableText == name) typeElement
      else {
        typeElement.replace {
          createTypeElementFromText(name, typeElement.getContext, typeElement)
        }.asInstanceOf[ScTypeElement]
      }

      resultTypeElement.getFirstChild match {
        case element: ScStableCodeReferenceElement => element.bindToElement(typeAlias)
      }

      resultTypeElement
    }

    occurrences.extendedClassOccurrences
      .foreach(replaceWith(_))

    val className = Option(getParentOfType(parent, classOf[ScObject]))
      .map(o => o.name + ".")
      .getOrElse("")

    occurrences.companionObjectOccurrences
      .foreach(replaceWith(_, s"$className$typeName"))

    val usualOccurrences = occurrences.usualOccurrences
      .map(replaceWith(_))

    val resultTypeElement = occurrences.usualOccurrences.indexWhere(_ == typeElement) match {
      case -1 => replaceWith(typeElement)
      case i => usualOccurrences(i)
    }

    val manager = SmartPointerManager.getInstance(file.getProject)
    (manager.createSmartPsiElementPointer(typeAlias), manager.createSmartPsiElementPointer(resultTypeElement))
  }

  def runRefactoringForTypes(file: PsiFile, typeElement: ScTypeElement, typeName: String,
                             occurrences_ : OccurrenceData, scope: ScopeItem)
                            (implicit editor: Editor): Unit =
    runRefactoringWithSelection {
      runRefactoringForTypeInside(file, typeElement, typeName, occurrences_, scope)
    }

  protected def createAndGetPackageObjectBody(typeElement: ScTypeElement,
                                              suggestedDirectory: PsiDirectory,
                                              needCreateDirectory: Boolean,
                                              inNewDirectoryName: String): ScTemplateBody = {
    val newDirectoryName = if (needCreateDirectory) {
      inNewDirectoryName
    } else {
      "package"
    }

    val currentDirectory = suggestedDirectory
    val newDir = if (needCreateDirectory) {
      currentDirectory.createSubdirectory(newDirectoryName)
    }
    else {
      currentDirectory
    }

    val packageObject: ScTypeDefinition =
      ScalaDirectoryService.createClassFromTemplate(newDir, newDirectoryName, "Package Object", askToDefineVariables = false)
        .asInstanceOf[ScTypeDefinition]

    getChildOfType(getChildOfType(packageObject, classOf[ScExtendsBlock]), classOf[ScTemplateBody])
  }

  private def runWithDialog(typeElement: ScTypeElement,
                            possibleScopes: Array[ScopeItem],
                            file: PsiFile,
                            mainScope: ScopeItem)
                           (implicit project: Project, editor: Editor): Unit = {
    val occurrences = mainScope match {
      case simpleScope: SimpleScopeItem => simpleScope.usualOccurrences.toSeq.map(_.getTextRange)
      case _: PackageScopeItem => Seq.empty[TextRange]
    }

    val dialog = new ScalaIntroduceTypeAliasDialog(project, typeElement, possibleScopes, mainScope, conflictsReporter, editor)

    showDialog(dialog, occurrences).foreach { dialog =>
      val occurrences = OccurrenceData(
        dialog.getSelectedScope,
        dialog.isReplaceAllOccurrences,
        dialog.isReplaceOccurrenceIncompanionObject,
        dialog.isReplaceOccurrenceInInheritors
      )

      runRefactoringForTypes(file,
        typeElement,
        dialog.getEnteredName,
        occurrences,
        dialog.getSelectedScope
      )
    }
  }
}

object IntroduceTypeAlias {
  val REVERT_TYPE_ALIAS_INFO: Key[IntroduceTypeAliasData] = new Key("RevertTypeAliasInfo")

  private def showTypeAliasChooser(elements: Array[ScopeItem], title: String)
                                  (pass: ScopeItem => Unit)
                                  (implicit editor: Editor): Unit = {
    class Selection {
      val selectionModel: SelectionModel = editor.getSelectionModel
      val (start, end) = (selectionModel.getSelectionStart, selectionModel.getSelectionEnd)
      val scheme: EditorColorsScheme = editor.getColorsScheme
      val textAttributes = new TextAttributes
      textAttributes.setForegroundColor(scheme.getColor(EditorColors.SELECTION_FOREGROUND_COLOR))
      textAttributes.setBackgroundColor(scheme.getColor(EditorColors.SELECTION_BACKGROUND_COLOR))
      var selectionHighlighter: RangeHighlighter = _
      val markupModel: MarkupModel = editor.getMarkupModel

      def addHighlighter(): Unit = if (selectionHighlighter == null) {
        selectionHighlighter = markupModel.addRangeHighlighter(start, end, HighlighterLayer.SELECTION + 1,
          textAttributes, HighlighterTargetArea.EXACT_RANGE)
      }

      def removeHighlighter(): Unit = if (selectionHighlighter != null) markupModel.removeHighlighter(selectionHighlighter)
    }

    val selection = new Selection
    val highlighter: ScopeHighlighter = new ScopeHighlighter(editor)
    val model = JListCompatibility.createDefaultListModel()
    for (element <- elements) {
      JListCompatibility.addElement(model, element)
    }
    val list = JListCompatibility.createJListFromModel(model)
    JListCompatibility.setCellRenderer(list, new DefaultListCellRendererAdapter {
      def getListCellRendererComponentAdapter(container: JListCompatibility.JListContainer,
                                              value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = {
        val rendererComponent: Component = getSuperListCellRendererComponent(container.getList, value, index, isSelected, cellHasFocus)
        val element = value.asInstanceOf[ScopeItem]
        setText(element.toString)
        rendererComponent
      }
    })
    list.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: ListSelectionEvent) {
        highlighter.dropHighlight()
        val index: Int = list.getSelectedIndex
        if (index < 0) return
      }
    })

    val highlightingListener = new JBPopupAdapter {
      override def beforeShown(event: LightweightWindowEvent): Unit = {
        selection.addHighlighter()
      }

      override def onClosed(event: LightweightWindowEvent) {
        highlighter.dropHighlight()
        selection.removeHighlighter()
      }
    }

    val callback: Runnable = inTransactionLater(editor.getProject) {
      pass(list.getSelectedValue.asInstanceOf[ScopeItem])
    }

    JBPopupFactory.getInstance.createListPopupBuilder(list)
      .setTitle(title)
      .setMovable(false)
      .setResizable(false)
      .setRequestFocus(true)
      .setItemChoosenCallback(callback)
      .addListener(highlightingListener)
      .createPopup
      .showInBestPositionFor(editor)
  }
}
