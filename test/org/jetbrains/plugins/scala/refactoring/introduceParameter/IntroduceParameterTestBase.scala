package org.jetbrains.plugins.scala.refactoring.introduceParameter

import java.io.File

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.fileEditor.{FileEditorManager, OpenFileDescriptor}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.{CharsetToolkit, LocalFileSystem}
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.ScMethodLike
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.refactoring.changeSignature.ScalaChangeSignatureProcessor
import org.jetbrains.plugins.scala.lang.refactoring.changeSignature.changeInfo.ScalaChangeInfo
import org.jetbrains.plugins.scala.lang.refactoring.introduceParameter.ScalaIntroduceParameterHandler
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil.{afterExpressionChoosing, trimSpacesAndComments}
import org.jetbrains.plugins.scala.util.ScalaUtils
import org.junit.Assert._

/**
  * @author Alexander Podkhalyuzin
  */

abstract class IntroduceParameterTestBase extends ScalaLightPlatformCodeInsightTestCaseAdapter {
  protected def folderPath = baseRootPath() + "introduceParameter/"

  private val startMarker = "/*start*/"
  private val endMarker = "/*end*/"
  private val allMarker = "//all = "
  private val nameMarker = "//name = "
  private val defaultMarker = "//default = "
  private val constructorMarker = "//constructor = "

  protected def doTest() {
    implicit val project: Project = getProjectAdapter
    val filePath = folderPath + getTestName(false) + ".scala"
    val file = LocalFileSystem.getInstance.findFileByPath(filePath.replace(File.separatorChar, '/'))
    assertNotNull("file " + filePath + " not found", file)

    val fileText = StringUtil.convertLineSeparators(FileUtil.loadFile(new File(file.getCanonicalPath), CharsetToolkit.UTF8))
    configureFromFileTextAdapter(getTestName(false) + ".scala", fileText)
    val scalaFile = getFileAdapter.asInstanceOf[ScalaFile]
    val startOffset = fileText.indexOf(startMarker) + startMarker.length
    assertNotEquals("Not specified start marker in test case. Use /*start*/ in scala file for this.",
      startOffset, -1 + startMarker.length)

    val endOffset = fileText.indexOf(endMarker)
    assertNotEquals("Not specified end marker in test case. Use /*end*/ in scala file for this.",
      endOffset, -1)

    val fileEditorManager = FileEditorManager.getInstance(project)
    implicit val editor: Editor = fileEditorManager
      .openTextEditor(new OpenFileDescriptor(project, file, startOffset), false)

    var res: String = null

    val lastPsi = scalaFile.findElementAt(scalaFile.getText.length - 1)

    //getting settings
    def getSetting(marker: String, default: String): String = {
      val offset = fileText.indexOf(marker)
      if (offset == -1) default
      else {
        val comment = scalaFile.findElementAt(offset)
        comment.getText.substring(marker.length)
      }
    }

    val replaceAllOccurrences = getSetting(allMarker, "true").toBoolean
    val paramName = getSetting(nameMarker, "param")
    val isDefaultParam = getSetting(defaultMarker, "false").toBoolean
    val toPrimaryConstructor = getSetting(constructorMarker, "false").toBoolean

    //start to inline
    try {
      ScalaUtils.runWriteActionDoNotRequestConfirmation(new Runnable {
        def run() {
          editor.getSelectionModel.setSelection(startOffset, endOffset)
          afterExpressionChoosing(scalaFile, "Introduce Variable", filterExpressions = false) {
            trimSpacesAndComments(editor, scalaFile)
            PsiDocumentManager.getInstance(project).commitAllDocuments()
            val handler = new ScalaIntroduceParameterHandler()
            val (exprWithTypes, elems) = handler.selectedElementsInFile(scalaFile).getOrElse(return)

            val (methodLike: ScMethodLike, returnType) =
              if (toPrimaryConstructor)
                (PsiTreeUtil.getContextOfType(elems.head, true, classOf[ScClass]).constructor.get, Any)
              else {
                val fun = PsiTreeUtil.getContextOfType(elems.head, true, classOf[ScFunctionDefinition])
                (fun, fun.returnType.getOrAny)
              }

            val data = handler.collectData(exprWithTypes, elems, methodLike, editor)
              .map(_.copy(paramName = paramName, replaceAll = replaceAllOccurrences))
              .getOrElse {
                fail("Could not collect data for introduce parameter")
                null
              }

            val descriptor = ScalaIntroduceParameterHandler.createMethodDescriptor(data)
            val changeInfo = ScalaChangeInfo(descriptor.getVisibility, data.methodToSearchFor, descriptor.getName, returnType,
              descriptor.parameters, isDefaultParam)

            changeInfo.introducedParameterData = Some(data)
            new ScalaChangeSignatureProcessor(project, changeInfo).run()
          }
        }
      }, project, "Test")
      res = scalaFile.getText.substring(0, lastPsi.getTextOffset).trim
    }
    catch {
      case e: Exception => fail(e.getMessage + "\n" + e.getStackTrace)
    }

    val text = lastPsi.getText
    val output = lastPsi.getNode.getElementType match {
      case ScalaTokenTypes.tLINE_COMMENT => text.substring(2).trim
      case ScalaTokenTypes.tBLOCK_COMMENT | ScalaTokenTypes.tDOC_COMMENT =>
        text.substring(2, text.length - 2).trim
      case _ =>
        fail("Test result must be in last comment statement.")
        null
    }

    assertEquals(output, res.trim)
  }
}