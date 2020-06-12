package fpp.compiler.codegen

import fpp.compiler.codegen._
import fpp.compiler.util._

// Write out F Prime XML as FPP source
object XmlFppWriter extends LineUtils {

  type Result = Result.Result[List[Line]]

  // An F Prime XML file
  case class File(
    // The file name
    name: String,
    // The XML element
    elem: scala.xml.Elem
  ) {

    def error(e: (String) => Error) = e(name)

  }

  def writeFile(file: File): Result = {
    Right(List(Line.blank, line(s"# ${file.name}"), Line.blank))
  }

  def writeFileList(fileList: List[File]) = {
    val writeHeader = lines(
      s"""|# ====================================================================== 
          |# Generated by xml-to-fpp
          |# ======================================================================"""
    )
    for (files <- Result.map(fileList, writeFile))
      yield writeHeader ++ files.flatten
  }


}
