package org.jetbrains.plugins.scala.refactoring.introduceVariable

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.fileEditor.{FileEditorManager, OpenFileDescriptor}
import com.intellij.openapi.module.{Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.SlowTests
import org.jetbrains.plugins.scala.base.libraryLoaders.{JdkLoader, ScalaLibraryLoader}
import org.jetbrains.plugins.scala.debugger.{ScalaSdkOwner, ScalaVersion, Scala_2_10}
import org.jetbrains.plugins.scala.lang.actions.ActionTestBase
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.SyntheticClasses
import org.jetbrains.plugins.scala.lang.refactoring.introduceVariable._
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil.{getExpressionWithTypes, getOccurrenceRanges, getTypeElement}
import org.jetbrains.plugins.scala.util.TestUtils.{createPseudoPhysicalScalaFile, getTestDataPath, removeBeginMarker, removeEndMarker}
import org.jetbrains.plugins.scala.util.{TestUtils, TypeAnnotationSettings}
import org.junit.Assert.{assertNotNull, assertTrue, fail}
import org.junit.experimental.categories.Category
import org.junit.runner.RunWith
import org.junit.runners.AllTests

/**
  * Nikolay.Tropin
  * 25-Sep-17
  */
@RunWith(classOf[AllTests])
@Category(Array(classOf[SlowTests]))
class IntroduceVariableTest extends ActionTestBase(Option(System.getProperty("path")).getOrElse {
  s"$getTestDataPath/introduceVariable/data"
}) with ScalaSdkOwner {

  override implicit val version: ScalaVersion = Scala_2_10

  override implicit def project: Project = getProject

  override implicit def module: Module = ModuleManager.getInstance(project).getModules()(0)

  override protected def librariesLoaders = Seq(JdkLoader(), ScalaLibraryLoader())

  private val path: Boolean = System.getProperty("path").toBoolean

  private var replaceAllOccurrences = System.getProperty("replaceAll") != null && path

  private var replaceCompanionObjOccurrences = System.getProperty("replaceCompanion") != null && path

  private var replaceOccurrencesFromInheritors = System.getProperty("replaceInheritors") != null && path

  import IntroduceVariableTest._
  import TestUtils.{BEGIN_MARKER, END_MARKER}

  private def processFile(fileText: String,
                          startOffset: Int, endOffset: Int): String = {
    val settings = ScalaCodeStyleSettings.getInstance(project)
    val oldSettings = settings.clone
    TypeAnnotationSettings.set(project, TypeAnnotationSettings.alwaysAddType(settings))

    val syntheticClasses = project.getComponent(classOf[SyntheticClasses])
    if (!syntheticClasses.isClassesRegistered) syntheticClasses.registerClasses()

    val psiFile = createPseudoPhysicalScalaFile(project, fileText)
    assertTrue(psiFile.isInstanceOf[ScalaFile])

    val virtualFile = psiFile.getVirtualFile
    assertNotNull(virtualFile)

    val fileEditorManager = FileEditorManager.getInstance(project)
    implicit val editor: Editor = fileEditorManager
      .openTextEditor(new OpenFileDescriptor(project, virtualFile, 0), false)
    assertNotNull(editor)

    val result = try {
      editor.getSelectionModel.setSelection(startOffset, endOffset)
      // gathering data for introduce variable
      val introduceVariableHandler = new ScalaIntroduceVariableHandler

      PsiTreeUtil.getParentOfType(psiFile.findElementAt(startOffset), classOf[ScExpression], classOf[ScTypeElement]) match {
        case _: ScExpression =>
          val (selectedExpr, types) = getExpressionWithTypes(psiFile, startOffset, endOffset).getOrElse {
            fail("Selected expression reference points to null")
            null
          }

          val occurrencesInFile = IntroduceExpressions.OccurrencesInFile(psiFile, new TextRange(startOffset, endOffset), getOccurrenceRanges(selectedExpr, psiFile))
          introduceVariableHandler.runRefactoring(occurrencesInFile, selectedExpr, "value", types.head, replaceAllOccurrences, isVariable = false)

          editor.getDocument.getText
        case _: ScTypeElement =>
          val typeElement = getTypeElement(psiFile, startOffset, endOffset).getOrElse {
            fail("Selected block should be presented as type element")
            null
          }

          val typeName = getName(fileText)
          val scope = ScopeSuggester.suggestScopes(introduceVariableHandler, project, editor, psiFile, typeElement)(0)
          val occurrences = OccurrenceData(scope, replaceAllOccurrences, replaceCompanionObjOccurrences, replaceOccurrencesFromInheritors)
          introduceVariableHandler.runRefactoringForTypes(psiFile, typeElement, typeName, occurrences, scope)

          removeTypenameComment(editor.getDocument.getText)
        case _ =>
          fail("Element should be typeElement or Expression")
          null
      }
    } finally {
      fileEditorManager.closeFile(virtualFile)
    }

    TypeAnnotationSettings.set(project, oldSettings.asInstanceOf[ScalaCodeStyleSettings])
    result
  }

  private def getName(fileText: String): String = fileText.indexOf("//") match {
    case 0 =>
      fileText.substring(2, fileText.indexOf("\n")).replaceAll("\\W", "")
    case _ =>
      fail("Typename to validator should be in first comment statement.")
      null
  }

  private def removeTypenameComment(fileText: String) = {
    val begin = fileText.indexOf("//")
    fileText.substring(0, begin) + fileText.substring(fileText.indexOf("\n", begin) + 1)
  }

  override def transform(testName: String, data: Array[String]): String = {
    setSettings()

    val file = createPseudoPhysicalScalaFile(project, data.head)

    var fileText = file.getText
    var startOffset = fileText.indexOf(BEGIN_MARKER)
    if (startOffset >= 0) {
      replaceAllOccurrences = false
      fileText = removeBeginMarker(fileText)
    } else {
      startOffset = fileText.indexOf(COMPANION_MARKER)
      if (startOffset >= 0) {
        replaceCompanionObjOccurrences = true
        fileText = removeMarker(fileText, COMPANION_MARKER)
      }
      else {
        startOffset = fileText.indexOf(INHERITORS_MARKER)
        if (startOffset >= 0) {
          replaceOccurrencesFromInheritors = true
          fileText = removeMarker(fileText, INHERITORS_MARKER)
        }
        else {
          startOffset = fileText.indexOf(ALL_MARKER)
          if (startOffset >= 0) {
            replaceAllOccurrences = true
            fileText = removeMarker(fileText, ALL_MARKER)
          }
        }
      }
    }

    val endOffset = fileText.indexOf(END_MARKER)
    fileText = removeEndMarker(fileText)

    processFile(fileText, startOffset, endOffset)
  }

  private def removeMarker(text: String, marker: String) = {
    val index = text.indexOf(marker)
    myOffset = index - 1
    text.substring(0, index) + text.substring(index + marker.length)
  }
}

object IntroduceVariableTest {
  private val ALL_MARKER = "<all>"
  private val COMPANION_MARKER = "<companion>"
  private val INHERITORS_MARKER = "<inheritors>"
}
