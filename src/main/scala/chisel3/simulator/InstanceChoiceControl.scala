// SPDX-License-Identifier: Apache-2.0

package chisel3.simulator

import chisel3.RawModule
import java.io.File

/** Utilities for controlling instance choice selections in ChiselSim
  *
  * Instance choice allows selecting different module implementations based on
  * options (e.g., FPGA vs ASIC implementations). This is similar to layer
  * control but for instance_choice operations in FIRRTL.
  *
  * '''FIRRTL Compilation Time:''' Instance choice selections are applied during
  * FIRRTL compilation (via firtool) when converting FIRRTL to Verilog. The
  * selected implementation is baked into the generated Verilog code.
  *
  * '''Verilog Elaboration Time:''' The choice affects which module implementation
  * gets instantiated in the generated SystemVerilog. CIRCT generates `targets_*.svh`
  * header files that define preprocessor macros controlling the selection.
  */
object InstanceChoiceControl {

  /** Enum representing when instance choice specialization occurs */
  sealed trait SpecializationTime
  object SpecializationTime {

    /** Specialize during FIRRTL compilation (via firtool options) */
    case object FirtoolCompilationTime extends SpecializationTime

    /** Specialize during Verilog elaboration (via preprocessor defines) */
    case object VerilogElaborationTime extends SpecializationTime
  }

  /** The type of all instance choice control variations */
  sealed trait Type {

    /** Return a partial function that will return true for files that should be
      * included in the build when using instance choices.
      *
      * '''Verilog Elaboration Time:''' Instance choice generates `targets-*.svh` header
      * files during Verilog elaboration. Only the header files for selected options
      * should be included; others should be excluded.
      *
      * File naming pattern: `targets-<module_name>-<option-name>-<option-value>.svh`
      *
      * @param module an elaborated Chisel module
      * @return a partial function to test if files should be included
      */
    final def shouldIncludeFile(
      module: ElaboratedModule[_ <: RawModule]
    ): PartialFunction[File, Boolean] = {
      // Get the set of (option, value) pairs that are selected for VerilogElaborationTime
      val selectedChoices = getVerilogElaborationTimeChoices.toSet

      {
        case a if a.getName().startsWith("targets-") && a.getName().endsWith(".svh") =>
          // Extract option and value from filename: targets-<module_name>-<option-name>-<option-value>.svh
          val fileName = a.getName()
          val withoutPrefix = fileName.stripPrefix("targets-").stripSuffix(".svh")
          val parts = withoutPrefix.split("-")

          // Need at least 3 parts: module_name, option-name, option-value
          if (parts.length >= 3) {
            // Last part is option-value, second-to-last is option-name
            val optionValue = parts.last
            val optionName = parts(parts.length - 2)
            // Include if this (option, value) pair IS selected for VerilogElaborationTime
            selectedChoices.contains((optionName, optionValue))
          } else {
            // If we can't parse it, include it (safer default)
            true
          }
      }
    }

    /** Get the list of (option, value) pairs for VerilogElaborationTime choices.
      *
      * @return sequence of (option, value) tuples
      */
    protected def getVerilogElaborationTimeChoices: Seq[(String, String)]

    /** Convert instance choices to firtool command-line options.
      *
      * Only choices with `SpecializationTime.FirtoolCompilationTime` are converted to firtool options.
      * Choices with `SpecializationTime.VerilogElaborationTime` are handled via preprocessor defines
      * and are not passed to firtool.
      *
      * @return firtool command line options for compile-time specialization
      */
    def toFirtoolOptions: Seq[String]
  }

  /** Instance choice control implementation that holds a sequence of choices.
    *
    * Instance choices are represented as a sequence of (specializationTime, option, case) tuples:
    * - `specializationTime`: When the choice is specialized (FirtoolCompilationTime or VerilogElaborationTime)
    * - `option`: The name of the instance choice group (e.g., "Platform")
    * - `case`: The selected case for this option (e.g., "FPGA", "ASIC")
    *
    * '''FIRRTL Compilation Time:''' Choices with `SpecializationTime.FirtoolCompilationTime` are passed
    * to firtool and applied during FIRRTL-to-Verilog conversion.
    *
    * '''Verilog Elaboration Time:''' Choices with `SpecializationTime.VerilogElaborationTime` are handled
    * via preprocessor defines in the generated SystemVerilog.
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

  /** Helper to create instance choices from a sequence.
    *
    * @param choices the instance choice selections
    * @return an InstanceChoiceControl.Type
    */
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
