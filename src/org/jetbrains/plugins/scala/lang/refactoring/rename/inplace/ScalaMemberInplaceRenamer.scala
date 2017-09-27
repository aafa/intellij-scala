package org.jetbrains.plugins.scala
package lang.refactoring.rename.inplace

import java.util

import com.intellij.codeInsight.TargetElementUtil
import com.intellij.lang.{Language, LanguageNamesValidation}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi._
import com.intellij.psi.search.SearchScope
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.refactoring.rename.RenamePsiElementProcessor
import com.intellij.refactoring.rename.inplace.{MemberInplaceRenamer, VariableInplaceRenamer}
import com.intellij.refactoring.{RefactoringActionHandler, RefactoringBundle}
import org.jetbrains.plugins.scala.lang.refactoring.rename.ScalaRenameUtil
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil.scalaName
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil.RevertInfo

/**
 * Nikolay.Tropin
 * 6/20/13
 */
class ScalaMemberInplaceRenamer(elementToRename: PsiNamedElement,
                                substituted: PsiElement)
                               (initialName: String = scalaName(substituted),
                                oldName: String = scalaName(substituted))
                               (private implicit val editor: Editor)
        extends MemberInplaceRenamer(elementToRename, substituted, editor, initialName, oldName) {

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

  private implicit def project: Project = myProject

  override def beforeTemplateStart() {
    super.beforeTemplateStart()

    val document = myEditor.getDocument
    RevertInfo.put(document.getText)

    val file = PsiDocumentManager.getInstance(myProject).getPsiFile(document)
    val offset = TargetElementUtil.adjustOffset(file, document, myEditor.getCaretModel.getOffset)
    val range = file.findElementAt(offset).getTextRange
    myCaretRangeMarker = document.createRangeMarker(range)
    myCaretRangeMarker.setGreedyToLeft(true)
    myCaretRangeMarker.setGreedyToRight(true)
  }

  override def revertState() {
    if (myOldName == null) return

    RevertInfo.revertStateCommand(getCommandName) {
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
  }

  override def getVariable: PsiNamedElement = {
    Option(super.getVariable).getOrElse {
      if (myElementToRename != null && myElementToRename.isValid && oldName == scalaName(myElementToRename))
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
    new ScalaMemberInplaceRenamer(variable, getSubstituted)(initialName, oldName)(editor)

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
