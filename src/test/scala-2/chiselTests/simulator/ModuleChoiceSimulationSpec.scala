// SPDX-License-Identifier: Apache-2.0

package chiselTests.simulator

import chisel3._
import chisel3.choice.{Case, Group, ModuleChoice}
import chisel3.simulator.{InstanceChoiceControl, Settings}
import chisel3.simulator.InstanceChoiceControl.SpecializationTime
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/** Test Group for ModuleChoice unit test */
object Platform extends Group {
  object FPGA extends Case
}

/** IO bundle for the ModuleChoice output modules */
class TargetIO extends Bundle {
  val out = Output(UInt(8.W))
}

/** Simple module that outputs a different constant value based on ModuleChoice selection.
  * This verifies that ModuleChoice correctly selects between different module implementations.
  */
class ModuleChoiceTestModule extends Module {
  val out = IO(Output(UInt(8.W)))

  // Each case outputs a different constant value
  // Must use FixedIORawModule for ModuleChoice compatibility
  class FPGATarget extends FixedIORawModule[TargetIO](new TargetIO) {
    io.out := 1.U
  }
  class VerificationTarget extends FixedIORawModule[TargetIO](new TargetIO) {
    io.out := 0.U
  }

  // Use ModuleChoice to select the appropriate output module based on the Platform group
  // Default is VerificationTarget (value 3)
  val choiceOut = ModuleChoice(new VerificationTarget)(
    Seq(
      Platform.FPGA -> new FPGATarget
    )
  )

  out := choiceOut.out
}

/** ScalaTest specification for ModuleChoice functionality with FPGA platform.
  *
  * This test demonstrates how to use instance choice with ChiselSim.
  * The instance choice is selected by passing the appropriate option to firtool
  * via the Settings.instanceChoices field.
  */
class ModuleChoiceFPGASpec extends AnyFunSpec with ChiselSim with Matchers {

  describe("ModuleChoice mechanism with FPGA platform") {
    it("should output 1 when FPGA is selected at compile time") {
      // Configure instance choice to select FPGA implementation at FIRRTL compilation time
      val settings = Settings
        .default[ModuleChoiceTestModule]
        .copy(
          instanceChoices = InstanceChoiceControl(List((SpecializationTime.FirtoolCompilationTime, "Platform", "FPGA")))
        )

      simulate(new ModuleChoiceTestModule, settings = settings) { dut =>
        dut.out.peek().litValue shouldBe 1
      }
    }
  }
}

/** ScalaTest specification for ModuleChoice functionality with FPGA platform at Verilog elaboration time. */
class ModuleChoiceFPGAVerilogElaborationSpec extends AnyFunSpec with ChiselSim with Matchers {

  describe("ModuleChoice mechanism with FPGA platform at Verilog elaboration time") {
    it("should output 1 when FPGA is selected at Verilog elaboration time") {
      // Configure instance choice to select FPGA implementation at Verilog elaboration time
      val settings = Settings
        .default[ModuleChoiceTestModule]
        .copy(
          instanceChoices = InstanceChoiceControl(List((SpecializationTime.VerilogElaborationTime, "Platform", "FPGA")))
        )

      simulate(new ModuleChoiceTestModule, settings = settings) { dut =>
        dut.out.peek().litValue shouldBe 1
      }
    }
  }
}

/** ScalaTest specification for ModuleChoice functionality with FPGA platform at Verilog elaboration time. */
class ModuleChoiceDefaultVerilogElaborationSpec extends AnyFunSpec with ChiselSim with Matchers {

  describe("ModuleChoice mechanism with FPGA platform at Verilog elaboration time") {
    it("should output 0 when no choice is selected at Verilog elaboration time") {
      simulate(new ModuleChoiceTestModule) { dut =>
        dut.out.peek().litValue shouldBe 0
      }
    }
  }
}
