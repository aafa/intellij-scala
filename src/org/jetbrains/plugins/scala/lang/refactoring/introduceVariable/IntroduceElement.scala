package org.jetbrains.plugins.scala
package lang
package refactoring
package introduceVariable

import com.intellij.internal.statistic.UsageTrigger
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.markup.RangeHighlighter
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.DialogWrapper
import com.intellij.openapi.util.TextRange
import com.intellij.openapi.wm.WindowManager
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile}
import org.jetbrains.plugins.scala.extensions.startCommand
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil.{highlightOccurrences, isInplaceAvailable, writableScalaFile}

trait IntroduceElement {

  private[refactoring] val refactoringName: String
  protected val triggerName: String

  private var occurrenceHighlighters = Seq.empty[RangeHighlighter]

  protected def runRefactoring(action: => Unit)
                              (implicit project: Project, editor: Editor): Unit =
    startCommand(project, refactoringName)(action)

  protected def runRefactoringWithSelection(action: => Unit)
                                           (implicit editor: Editor): Unit = {
    implicit val project: Project = editor.getProject
    runRefactoring(action)
    editor.getSelectionModel.removeSelection()
  }

  protected def trigger(file: PsiFile)
                       (implicit project: Project, editor: Editor): Unit = {
    UsageTrigger.trigger(triggerName)

    PsiDocumentManager.getInstance(project).commitAllDocuments()
    writableScalaFile(file, refactoringName)
  }

  protected def showDialog[D <: DialogWrapper](dialog: D,
                                               occurrences: Seq[TextRange])
                                              (implicit project: Project, editor: Editor): Option[D] = {
    val multipleOccurrences = occurrences.length > 1
    if (multipleOccurrences) {
      occurrenceHighlighters = highlightOccurrences(project, occurrences, editor)
    }

    if (dialog.showAndGet()) Some(dialog) else {
      if (multipleOccurrences) {
        WindowManager.getInstance
          .getStatusBar(project)
          .setInfo(ScalaBundle.message("press.escape.to.remove.the.highlighting"))
      }

      occurrenceHighlighters.foreach(_.dispose())
      occurrenceHighlighters = Seq.empty

      None
    }
  }

}

object IntroduceElement {

  private[introduceVariable] def withElement[E <: PsiElement](maybeElement: Option[E])
                                                             (action: E => Unit)
                                                             (implicit project: Project, editor: Editor): Unit =
    maybeElement.filter(_.isValid).foreach { element =>
      editor.getCaretModel.moveToOffset(element.getTextOffset)
      editor.getSelectionModel.removeSelection()

      if (isInplaceAvailable(editor)) {
        (editor.getDocument, PsiDocumentManager.getInstance(project)) match {
          case (document, manager) =>
            manager.commitDocument(document)
            manager.doPostponedOperationsAndUnblockDocument(document)
        }

        action(element)
      }
    }
}
