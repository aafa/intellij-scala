package org.jetbrains.plugins.scala
package lang
package refactoring
package introduceVariable

import com.intellij.openapi.util.TextRange
import com.intellij.psi.util.PsiTreeUtil.findElementOfClassAtOffset
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAlias

class IntroduceTypeAliasData {

  private[this] var typeAlias_ : ScTypeAlias = null
  private[this] var currentScope_ : ScopeItem = null
  private[this] var initialRange_ : TextRange = null
  private[this] var possibleScopes_ : Array[ScopeItem] = null
  private[this] var modalDialogInProgress_ : Boolean = false

  def typeAlias: ScTypeAlias =
    findElementOfClassAtOffset(typeAlias_.getContainingFile, typeAlias_.getTextRange.getStartOffset, classOf[ScTypeAlias], false)

  def typeAlias_=(typeAlias: ScTypeAlias): Unit = {
    typeAlias_ = typeAlias
  }

  def currentScope: ScopeItem = currentScope_

  def currentScope_=(currentScope: ScopeItem): Unit = {
    currentScope_ = currentScope
  }

  def initialRange: TextRange = initialRange_

  def initialRange_=(initialRange: TextRange): Unit = {
    if (initialRange_ == null) {
      initialRange_ = initialRange
    }
  }

  def possibleScopes: Array[ScopeItem] = possibleScopes_

  def possibleScopes_=(possibleScopes: Array[ScopeItem]): Unit = {
    possibleScopes_ = possibleScopes
  }

  def modalDialogInProgress: Boolean = modalDialogInProgress_

  def modalDialogInProgress_=(modalDialogInProgress: Boolean): Unit = {
    modalDialogInProgress_ = modalDialogInProgress
  }

  def isEmpty: Boolean =
    typeAlias_ == null &&
      currentScope_ == null &&
      initialRange_ == null &&
      possibleScopes_ == null
}
