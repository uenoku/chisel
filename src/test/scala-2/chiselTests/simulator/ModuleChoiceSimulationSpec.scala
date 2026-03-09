// SPDX-License-Identifier: Apache-2.0

package chiselTests.simulator

import chisel3._
import chisel3.choice.{Case, Group, ModuleChoice}
import chisel3.simulator.{InstanceChoiceControl, Settings}
import chisel3.simulator.InstanceChoiceControl.SpecializationTime
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

object Platform extends Group {
  object FPGA extends Case
}

object Opt extends Group {
  object Fast extends Case
}

class TargetIO extends Bundle {
  val out = Output(UInt(8.W))
}

/** Test module with ModuleChoice that outputs different values based on selection */
class ModuleChoiceTestModule extends Module {
  val out1, out2 = IO(Output(UInt(8.W)))

  class Return1 extends FixedIORawModule[TargetIO](new TargetIO) {
    io.out := 1.U
  }
  class Return0 extends FixedIORawModule[TargetIO](new TargetIO) {
    io.out := 0.U
  }

  val choiceOut1 = ModuleChoice(new Return0)(
    Seq(
      Platform.FPGA -> new Return1
    )
  )

  out1 := choiceOut1.out

  val choiceOut2 = ModuleChoice(new Return0)(
    Seq(
      Opt.Fast -> new Return1
    )
  )

  out2 := choiceOut2.out
}

/** Test ModuleChoice with FirtoolCompilationTime */
class ModuleChoiceFPGASpec extends AnyFunSpec with ChiselSim with Matchers {

  describe("ModuleChoice mechanism with FPGA platform") {
    it("should output 1 when FPGA is selected at compile time") {
      val settings = Settings
        .default[ModuleChoiceTestModule]
        .copy(
          instanceChoices = InstanceChoiceControl(List((SpecializationTime.FirtoolCompilationTime, "Platform", "FPGA")))
        )

      simulate(new ModuleChoiceTestModule, settings = settings) { dut =>
        dut.out1.peek().litValue shouldBe 1
        dut.out2.peek().litValue shouldBe 0
      }
    }
  }
}

/** Test ModuleChoice with multiple choices at FirtoolCompilationTime */
class ModuleChoiceMultipleChoicesFPGASpec extends AnyFunSpec with ChiselSim with Matchers {

  describe("ModuleChoice mechanism with FPGA platform") {
    it("should output 1 when FPGA is selected at compile time") {
      val settings = Settings
        .default[ModuleChoiceTestModule]
        .copy(
          instanceChoices = InstanceChoiceControl(List((SpecializationTime.FirtoolCompilationTime, "Platform", "FPGA"), (SpecializationTime.FirtoolCompilationTime, "Opt", "Fast"))
        ))

      simulate(new ModuleChoiceTestModule, settings = settings) { dut =>
        dut.out1.peek().litValue shouldBe 1
        dut.out2.peek().litValue shouldBe 1
      }
    }
  }
}

/** Test ModuleChoice with VerilogElaborationTime */
class ModuleChoiceFPGAVerilogElaborationSpec extends AnyFunSpec with ChiselSim with Matchers {

  describe("ModuleChoice mechanism with FPGA platform at Verilog elaboration time") {
    it("should output 1 when FPGA is selected at Verilog elaboration time") {
      val settings = Settings
        .default[ModuleChoiceTestModule]
        .copy(
          instanceChoices = InstanceChoiceControl(List((SpecializationTime.VerilogElaborationTime, "Platform", "FPGA")))
        )

      simulate(new ModuleChoiceTestModule, settings = settings) { dut =>
        dut.out1.peek().litValue shouldBe 1
        dut.out2.peek().litValue shouldBe 0
      }
    }
  }
}

/** Test ModuleChoice with multiple choices at VerilogElaborationTime */
class ModuleChoiceMultipleChoicesVerilogElaborationSpec extends AnyFunSpec with ChiselSim with Matchers {

  describe("ModuleChoice mechanism with FPGA platform at Verilog elaboration time") {
    it("should output 1 when FPGA is selected at Verilog elaboration time") {
      val settings = Settings
        .default[ModuleChoiceTestModule]
        .copy(
          instanceChoices = InstanceChoiceControl(List((SpecializationTime.VerilogElaborationTime, "Platform", "FPGA"), (SpecializationTime.VerilogElaborationTime, "Opt", "Fast"))
        ))

      simulate(new ModuleChoiceTestModule, settings = settings) { dut =>
        dut.out1.peek().litValue shouldBe 1
        dut.out2.peek().litValue shouldBe 1
      }
    }
  }
}