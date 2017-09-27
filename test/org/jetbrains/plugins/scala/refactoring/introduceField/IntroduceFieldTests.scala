package org.jetbrains.plugins.scala
package refactoring.introduceField

import java.io.File

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.{CharsetToolkit, LocalFileSystem}
import com.intellij.testFramework.UsefulTestCase
import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.api.Int
import org.jetbrains.plugins.scala.lang.refactoring.introduceField.{IntroduceFieldContext, IntroduceFieldSettings, ScalaIntroduceFieldFromExpressionHandler}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil.getExpressionWithTypes
import org.jetbrains.plugins.scala.util.ScalaUtils
import org.junit.Assert.{assertEquals, assertNotEquals, assertNotNull, fail}

/**
  * Nikolay.Tropin
  * 7/17/13
  */
class IntroduceFieldTests extends ScalaLightPlatformCodeInsightTestCaseAdapter {

  private val startMarker = "/*start*/"
  private val endMarker = "/*end*/"
  private val replaceAllMarker = "/*replaceAll*/"
  private val initInDeclarationMarker = "/*initInDeclaration*/"
  private val initLocallyMarker = "/*initLocally*/"
  private val selectedClassNumberMarker = "/*selectedClassNumber = "

  def folderPath: String = baseRootPath() + "introduceField/"

  def testSimple(): Unit = doTest()

  def testSimpleReplaceAll(): Unit = doTest()

  def testSimpleReplaceOne(): Unit = doTest()

  def testSimpleFromMethodInitInDecl(): Unit = doTest()

  def testSimpleFromMethodInitLocally(): Unit = doTest()

  def testSimpleInInner(): Unit = doTest()

  def testTwoMethodsInitInDecl(): Unit = doTest()

  def testFromAnonymousLocally(): Unit = doTest()

  def testFromAnonymousInDeclaration(): Unit = doTest(intType = false)

  def testReplaceAllInOuter(): Unit = doTest()

  def testFromBaseConstructorAddEarlyDefs(): Unit = doTest()

  def testFromBaseConstructorToEarlyDefs(): Unit = doTest()

  def testFromBaseConstructorToEarlyDefs2(): Unit = doTest()

  protected def doTest(intType: Boolean = true) {
    val filePath = folderPath + getTestName(false) + ".scala"
    val file = LocalFileSystem.getInstance.findFileByPath(filePath.replace(File.separatorChar, '/'))
    assertNotNull("file " + filePath + " not found", file)

    var fileText = StringUtil.convertLineSeparators(FileUtil.loadFile(new File(file.getCanonicalPath), CharsetToolkit.UTF8))
    val startOffset = fileText.indexOf(startMarker)
    assertNotEquals("Not specified start marker in test case. Use /*start*/ in scala file for this.", startOffset, -1)

    fileText = fileText.replace(startMarker, "")
    val endOffset = fileText.indexOf(endMarker)
    assertNotEquals("Not specified end marker in test case. Use /*end*/ in scala file for this.", endOffset, -1)

    fileText = fileText.replace(endMarker, "")

    configureFromFileTextAdapter(getTestName(false) + ".scala", fileText)
    val scalaFile = getFileAdapter.asInstanceOf[ScalaFile]

    implicit val editor: Editor = getEditorAdapter
    editor.getSelectionModel.setSelection(startOffset, endOffset)

    var res: String = null

    val lastPsi = scalaFile.findElementAt(scalaFile.getText.length - 1)
    val replaceAll = fileText.contains(replaceAllMarker)
    val initInDecl = if (fileText.contains(initInDeclarationMarker)) Some(true)
    else if (fileText.contains(initLocallyMarker)) Some(false)
    else None

    val selectedClassNumber = fileText.indexOf(selectedClassNumberMarker) match {
      case -1 => 0
      case idx: Int => fileText.charAt(idx + selectedClassNumberMarker.length).toString.toInt
    }

    implicit val project: Project = getProjectAdapter
    //start to inline
    try {
      val Some((expr, types)) = getExpressionWithTypes(scalaFile, startOffset, endOffset)
      val aClass = expr.parents.toList.collect {
        case clazz: ScTemplateDefinition => clazz
      }.apply(selectedClassNumber)

      val ifc = new IntroduceFieldContext(scalaFile, expr, aClass)
      val settings = new IntroduceFieldSettings(ifc, types.head)
      settings.replaceAll = replaceAll
      initInDecl.foreach(settings.initInDeclaration = _)
      settings.defineVar = true
      settings.name = "i"
      settings.scType = if (intType) Int(project) else null
      ScalaUtils.runWriteActionDoNotRequestConfirmation(() => {
        (new ScalaIntroduceFieldFromExpressionHandler).runRefactoring(ifc, settings)
        UsefulTestCase.doPostponedFormatting(project)
      }, project, "Test")
      res = scalaFile.getText.substring(0, lastPsi.getTextOffset).trim
    }
    catch {
      case e: Exception => fail(e.getMessage + "\n" + e.getStackTrace.map(_.toString).mkString("  \n"))
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

    assertEquals(output, res)
  }
}
