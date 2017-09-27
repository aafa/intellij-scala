package org.jetbrains.plugins.scala
package lang.refactoring.rename.inplace

import java.util

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.lang.{Language, LanguageNamesValidation}
import com.intellij.openapi.editor.{Editor, ScrollType}
import com.intellij.psi._
import com.intellij.psi.search.SearchScope
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.refactoring.rename.RenamePsiElementProcessor
import com.intellij.refactoring.rename.inplace.{MemberInplaceRenamer, VariableInplaceRenamer}
import com.intellij.refactoring.{RefactoringActionHandler, RefactoringBundle}
import org.jetbrains.plugins.scala.extensions.{inWriteAction, startCommand}
import org.jetbrains.plugins.scala.lang.refactoring.rename.ScalaRenameUtil
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil.RevertInfo

/**
 * Nikolay.Tropin
 * 6/20/13
 */
class ScalaMemberInplaceRenamer(elementToRename: PsiNamedElement,
                                substituted: PsiElement,
                                editor: Editor,
                                initialName: String,
                                oldName: String)
        extends MemberInplaceRenamer(elementToRename, substituted, editor, initialName, oldName) {

  private def this(t: (PsiNamedElement, PsiElement, Editor, String, String)) = this(t._1, t._2, t._3, t._4, t._5)

  def this(elementToRename: PsiNamedElement, substituted: PsiElement, editor: Editor) {
    this {
      val name = ScalaNamesUtil.scalaName(substituted)
      (elementToRename, substituted, editor, name, name)
    }
  }

  def this(elementToRename: PsiNamedElement, substituted: PsiNamedElement, editor: Editor, additionalToRename: Seq[PsiElement]) {
    this(elementToRename, substituted, editor)
  }

  protected override def getCommandName: String = {
    if (myInitialName != null) RefactoringBundle.message("renaming.command.name", myInitialName)
    else "Rename"
  }

  override def collectRefs(referencesSearchScope: SearchScope): util.Collection[PsiReference] =
    ScalaRenameUtil.filterAliasedReferences {
      super.collectRefs(referencesSearchScope)
    }

  override def restoreCaretOffset(offset: Int): Int = {
    offset.max(myCaretRangeMarker.getStartOffset).min(myCaretRangeMarker.getEndOffset)
  }

  override def acceptReference(reference: PsiReference): Boolean = true

  override def beforeTemplateStart() {
    super.beforeTemplateStart()

    val document = myEditor.getDocument
    RevertInfo.put(document.getText)(myEditor)

    val file = PsiDocumentManager.getInstance(myProject).getPsiFile(document)
    val offset = TargetElementUtil.adjustOffset(file, document, myEditor.getCaretModel.getOffset)
    val range = file.findElementAt(offset).getTextRange
    myCaretRangeMarker = document.createRangeMarker(range)
    myCaretRangeMarker.setGreedyToLeft(true)
    myCaretRangeMarker.setGreedyToRight(true)
  }

  override def revertState() {
    if (myOldName == null) return

    startCommand(myProject, () => {
      val document = myEditor.getDocument
      RevertInfo.find(myEditor).foreach {
        case RevertInfo(fileText, caretOffset) =>
          inWriteAction {
            document.replaceString(0, document.getTextLength, fileText)
            PsiDocumentManager.getInstance(myProject).commitDocument(document)
          }

          myEditor.getCaretModel.moveToOffset(caretOffset)
          myEditor.getScrollingModel.scrollToCaret(ScrollType.MAKE_VISIBLE)
          PsiDocumentManager.getInstance(myEditor.getProject).commitDocument(document)
          val clazz = myElementToRename.getClass
          val element = TargetElementUtil.findTargetElement(myEditor,
            TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED | TargetElementUtil.ELEMENT_NAME_ACCEPTED)
          myElementToRename = element match {
            case null => null
            case named: PsiNamedElement if named.getClass == clazz => named
            case _ =>
              RenamePsiElementProcessor.forElement(element).substituteElementToRename(element, myEditor) match {
                case named: PsiNamedElement if named.getClass == clazz => named
                case _ => null
              }
          }
      }
      if (!myProject.isDisposed && myProject.isOpen) {
        PsiDocumentManager.getInstance(myProject).commitDocument(document)
      }
    }, getCommandName)

  }

  override def getVariable: PsiNamedElement = {
    Option(super.getVariable).getOrElse {
      if (myElementToRename != null && myElementToRename.isValid && oldName == ScalaNamesUtil.scalaName(myElementToRename))
        myElementToRename
      else null
    }
  }

  private val substitutorOffset = substituted.getTextRange.getStartOffset

  override def getSubstituted: PsiElement = {
    val subst = super.getSubstituted
    if (subst != null && subst.getText == substituted.getText) subst
    else {
      val psiFile: PsiFile = PsiDocumentManager.getInstance(myProject).getPsiFile(myEditor.getDocument)
      if (psiFile != null) PsiTreeUtil.getParentOfType(psiFile.findElementAt(substitutorOffset), classOf[PsiNameIdentifierOwner])
      else null
    }

  }

  override def isIdentifier(newName: String, language: Language): Boolean =
    LanguageNamesValidation.INSTANCE.forLanguage(language).isIdentifier(newName, myProject)

  override def createInplaceRenamerToRestart(variable: PsiNamedElement, editor: Editor, initialName: String): VariableInplaceRenamer =
    new ScalaMemberInplaceRenamer(variable, getSubstituted, editor, initialName, oldName)

  override def performInplaceRename(): Boolean = {
    val names = new util.LinkedHashSet[String]()
    names.add(initialName)
    try performInplaceRefactoring(names)
    catch {
      case t: Throwable =>
        val element = getVariable
        val subst = getSubstituted
        val offset = myEditor.getCaretModel.getOffset
        val text = myEditor.getDocument.getText
        val aroundCaret = text.substring(offset - 50, offset) + "<caret>" + text.substring(offset, offset + 50)
        val message =
          s"""Could not perform inplace rename:
             |element to rename: $element ${element.getName}
             |substituted: $subst
             |around caret: $aroundCaret""".stripMargin
        throw new Throwable(message, t)
    }
  }

  override def getNameIdentifier: PsiElement = myElementToRename match {
    case lightPsi: PsiNamedElement if !lightPsi.isPhysical => null
    case nameIdentifierOwner: PsiNameIdentifierOwner if myElementToRename.getContainingFile.getViewProvider.getAllFiles.size() > 1 => nameIdentifierOwner.getNameIdentifier
    case _ => super.getNameIdentifier
  }

  override def startsOnTheSameElement(handler: RefactoringActionHandler, element: PsiElement): Boolean = {
    handler match {
      case _: ScalaMemberInplaceRenameHandler =>
        val caretOffset = myEditor.getCaretModel.getOffset
        myCaretRangeMarker != null && myCaretRangeMarker.isValid &&
          myCaretRangeMarker.getStartOffset <= caretOffset && myCaretRangeMarker.getEndOffset >= caretOffset
      case _ => false
    }
  }
}
