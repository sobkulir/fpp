package fpp.compiler.codegen

import fpp.compiler.analysis._
import fpp.compiler.ast._
import fpp.compiler.util._

/** Writes out C++ for topology component instances */
case class TopComponentInstances(
  s: CppWriterState,
  aNode: Ast.Annotated[AstNode[Ast.DefTopology]]
) extends TopologyCppWriterUtils(s, aNode) {

  private val bannerComment = "Component instances"

  def getHppLines: List[Line] = addBannerComment(
    bannerComment,
    getDeclLines
  )

  def getCppLines: List[Line] = addBannerComment(
    bannerComment,
    getDefLines
  )

  private def getDeclLines = {
    def getCode(ci: ComponentInstance): List[Line] = {
      val implType = getImplType(ci)
      val instanceName = getNameAsIdent(ci.qualifiedName)
      Line.addPrefixLine (line(s"//! $instanceName")) (
        lines(
          s"extern $implType $instanceName;"
        )
      )
    }
    flattenWithBlankPrefix(instances.map(getCode))
  }

  private def getDefLines = {
    def getCode(ci: ComponentInstance): List[Line] = {
      val implType = getImplType(ci)
      val instanceName = getNameAsIdent(ci.qualifiedName)

      getCodeLinesForPhase (CppWriter.Phases.instances) (ci).getOrElse{
        def getStackDefCode(ci: ComponentInstance): List[Line] = {
          val stackSize = ci.stackSize.getOrElse(Nil)
          List(
            line("#ifdef TGT_OS_TYPE_ZEPHYR"),
            line(s"K_THREAD_STACK_DEFINE(${instanceName}_stack, $stackSize);"),
            line(s"NATIVE_UINT_TYPE ${instanceName}_stack_size_real = K_THREAD_STACK_SIZEOF(${instanceName}_stack);"),
            line("#endif")
          )
        }  

        val result = lines(
          s"$implType $instanceName(FW_OPTIONAL_NAME($q$instanceName$q));"
        )

        if (isActive(ci)) {
          result ::: getStackDefCode(ci)
        } else {
          result
        }
      }
    }
    
    flattenWithBlankPrefix(instances.map(getCode))
  }

  private def getImplType(ci: ComponentInstance) = {
    val implType = ci.aNode._2.data.implType.map(_.data)
    implType.getOrElse(getComponentNameAsQualIdent(ci))
  }

}
