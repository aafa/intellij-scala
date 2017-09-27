package org.jetbrains.plugins.scala.lang.refactoring.introduceVariable

import com.intellij.openapi.command.undo.UndoManager
import com.intellij.openapi.editor.Editor
import com.intellij.psi._
import com.intellij.refactoring.RefactoringActionHandler
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.refactoring.rename.inplace.ScalaMemberInplaceRenamer
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil.RevertInfo

/**
 * Created by Kate Ustyuzhanina
 * on 8/10/15
 */

class ScalaInplaceTypeAliasIntroducer(element: ScNamedElement)
                                     (implicit editor: Editor)
  extends ScalaMemberInplaceRenamer(element, element)(element.getName, element.getName) {

  override def setAdvertisementText(text: String): Unit = {
    myAdvertisementText = "Press ctrl + alt + v" + " to show dialog with more options"
  }

  override def startsOnTheSameElement(handler: RefactoringActionHandler, element: PsiElement): Boolean =
    handler.isInstanceOf[ScalaIntroduceVariableHandler] && (element match {
      case typeAlias: ScTypeAliasDefinition => IntroduceTypeAliasData.find(editor)
        .map(_.typeAlias).contains(typeAlias)
      case _ => false
    })

  override def revertState(): Unit = {
    //do nothing. we don't need to revert state
  }

  protected override def moveOffsetAfter(success: Boolean): Unit = {
    if (success) {
      // don't know about element to refactor place
    }
    else if (myInsertedName != null && !UndoManager.getInstance(myProject).isUndoInProgress
      && !IntroduceTypeAliasData.find.exists(_.modalDialogInProgress)) {
      RevertInfo.revert()(myEditor)
    }
  }
}