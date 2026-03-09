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

  /** The type representing instance choice selections
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
  type Type = Seq[(SpecializationTime, String, String)]

  /** Convert instance choices to firtool command-line options.
    *
    * Only choices with `SpecializationTime.FirtoolCompilationTime` are converted to firtool options.
    * Choices with `SpecializationTime.VerilogElaborationTime` are handled via preprocessor defines
    * and are not passed to firtool.
    *
    * @param choices the instance choice selections
    * @return firtool command line options for compile-time specialization
    */
  def toFirtoolOptions(choices: Type): Seq[String] = {
    choices.collect {
      case (SpecializationTime.FirtoolCompilationTime, option, caseValue) =>
        Seq("--select-instance-choice", s"$option=$caseValue")
    }.flatten
  }

  /** Return a partial function that will return false for files that should be
    * excluded from the build when using instance choices.
    *
    * '''Verilog Elaboration Time:''' Instance choice generates `targets-*.svh` header
    * files during Verilog elaboration. Only the header files for selected options
    * should be included; others should be excluded.
    *
    * @param choices the instance choice selections
    * @return a partial function to test if files should be excluded
    */
  def shouldExcludeFile(choices: Type): PartialFunction[File, Boolean] = {
    // Get the set of option names that are selected for VerilogElaborationTime
    val selectedOptions = choices.collect {
      case (SpecializationTime.VerilogElaborationTime, option, _) => option
    }.toSet

    {
      case a if a.getName().startsWith("targets-") && a.getName().endsWith(".svh") =>
        // Extract the option name from the filename: targets-<OptionName>.svh
        val fileName = a.getName()
        val optionName = fileName.stripPrefix("targets-").stripSuffix(".svh")
        // Exclude if this option is NOT selected for VerilogElaborationTime
        !selectedOptions.contains(optionName)
    }
  }

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

