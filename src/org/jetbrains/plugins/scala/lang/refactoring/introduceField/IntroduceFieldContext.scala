package org.jetbrains.plugins.scala
package lang.refactoring.introduceField

import java.{util => ju}

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.ScalaRecursiveElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScExtendsBlock
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.refactoring.namesSuggester.NameSuggester.suggestNames
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._
import org.jetbrains.plugins.scala.lang.refactoring.util.{DialogConflictsReporter, ScalaVariableValidator, ValidationReporter}

import scala.collection.JavaConverters._

/**
  * Nikolay.Tropin
  * 7/15/13
  */
case class IntroduceFieldContext(file: PsiFile,
                                 expression: ScExpression,
                                 types: Array[ScType],
                                 extendsBlock: ScExtendsBlock)
                                (implicit val project: Project, val editor: Editor) {

  def this(file: PsiFile,
           expression: ScExpression,
           types: Array[ScType],
           clazz: ScTemplateDefinition)
          (implicit project: Project, editor: Editor) =
    this(file, expression, types, clazz.extendsBlock)

  val occurrences: Seq[TextRange] = getOccurrenceRanges(expression, extendsBlock)

  private implicit val validator: ScalaVariableValidator = ScalaVariableValidator(file, expression, occurrences)

  val reporter: ValidationReporter = new ValidationReporter(project, new DialogConflictsReporter {})

  private val statementsAndMembers: Seq[PsiElement] = statementsAndMembersInClass(extendsBlock)

  val canBeInitInDecl: Boolean = find(expression).forall(checkForwardReferences)

  val possibleNames: ju.Set[String] = suggestNames(expression).toSet[String].asJava

  def canBeInitLocally(replaceAll: Boolean): Boolean = {
    val occurrences = if (replaceAll) this.occurrences else Seq(expression.getTextRange)

    val parExpr = findParentExpr(commonParent(file, occurrences))
    val parents = container(parExpr, file).withParentsInFile

    if (parents.exists(statementsAndMembers.contains(_))) checkForwardReferences(parExpr)
    else false
  }

  private def find(element: PsiElement): Option[PsiElement] =
    element.withParentsInFile
      .find(statementsAndMembers.contains(_))

  private def checkForwardReferences(position: PsiElement): Boolean =
    IntroduceFieldContext.checkForwardReferences(expression, position)
}

object IntroduceFieldContext {

  private def checkForwardReferences(expression: ScExpression, position: PsiElement): Boolean = {
    var result = true
    val visitor = new ScalaRecursiveElementVisitor() {
      override def visitReferenceExpression(ref: ScReferenceExpression): Unit = {
        ref.getParent match {
          case ScInfixExpr(_, `ref`, _) =>
          case ScPostfixExpr(_, `ref`) =>
          case ScPrefixExpr(`ref`, _) =>
          case _ =>
            val newRef = createExpressionFromText(ref.getText, position).asInstanceOf[ScReferenceExpression]
            result &= ref.resolve() == newRef.resolve()
        }
        super.visitReferenceExpression(ref)
      }
    }
    expression.accept(visitor)
    result
  }
}
