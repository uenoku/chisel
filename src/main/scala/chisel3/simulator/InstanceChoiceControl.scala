// SPDX-License-Identifier: Apache-2.0

package chisel3.simulator

import chisel3.RawModule
import java.io.File

/** Utilities for controlling instance choice selections */
object InstanceChoiceControl {

  /** Enum representing when instance choice specialization occurs */
  sealed trait SpecializationTime
  object SpecializationTime {
    case object FirtoolCompilationTime extends SpecializationTime
    case object VerilogElaborationTime extends SpecializationTime
  }

  /** The type of all instance choice control variations */
  sealed trait Type {

    /** Return a partial function that will return true if a file should be included
      * in the build to enable an instance choice. This partial function is not defined
      * if the file is not an instance choice header file.
      *
      * @param module an elaborated Chisel module
      * @return a partial function to test if instance choice files should be included
      */
    final def shouldIncludeFile(
      module: ElaboratedModule[_ <: RawModule]
    ): PartialFunction[File, Boolean] = {
      val selectedChoices = getVerilogElaborationTimeChoices.toSet

      {
        case a if a.getName().startsWith("targets-") && a.getName().endsWith(".svh") =>
          val fileName = a.getName()
          val withoutPrefix = fileName.stripPrefix("targets-").stripSuffix(".svh")
          val parts = withoutPrefix.split("-")

          if (parts.length >= 3) {
            val optionValue = parts.last
            val optionName = parts(parts.length - 2)
            selectedChoices.contains((optionName, optionValue))
          } else {
            true
          }
      }
    }

    /** Return the (option, value) pairs for VerilogElaborationTime choices */
    protected def getVerilogElaborationTimeChoices: Seq[(String, String)]

    /** Convert instance choices to firtool command-line options */
    def toFirtoolOptions: Seq[String]
  }

  /** Instance choice control implementation
    *
    * @param choices sequence of (specializationTime, option, case) tuples
    */
  case class Choices(choices: Seq[(SpecializationTime, String, String)]) extends Type {

    override protected def getVerilogElaborationTimeChoices: Seq[(String, String)] = {
      choices.collect { case (SpecializationTime.VerilogElaborationTime, option, value) =>
        (option, value)
      }
    }

    override def toFirtoolOptions: Seq[String] = {
      choices.collect { case (SpecializationTime.FirtoolCompilationTime, option, caseValue) =>
        Seq("--select-instance-choice", s"$option=$caseValue")
      }.flatten
    }
  }

  /** Create instance choices from a sequence */
  def apply(choices: Seq[(SpecializationTime, String, String)]): Type = Choices(choices)

  /** Return the list of additional header files that should be included for
    * instance choice support.
    *
    * '''Verilog Elaboration Time:''' Instance choice generates `targets-*.svh` header
    * files during Verilog elaboration. These files need to be available during
    * compilation but should not be compiled as source files.
    *
    * @param primarySourcesPath the path to the primary sources directory
    * @return a sequence of header file paths
    */
  def getAdditionalHeaders(primarySourcesPath: String): Seq[String] = {
    val primarySourcesDir = new File(primarySourcesPath)
    if (primarySourcesDir.exists() && primarySourcesDir.isDirectory()) {
      primarySourcesDir
        .listFiles()
        .filter(f => f.getName().startsWith("targets-") && f.getName().endsWith(".svh"))
        .map(_.getParent())
        .distinct
        .toSeq
    } else {
      Seq.empty
    }
  }

}
