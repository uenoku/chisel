// SPDX-License-Identifier: Apache-2.0

package chiselTests.util.circt

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.testers.BasicTester
import chisel3.util.circt._
import circt.stage.ChiselStage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

private class ChiselIfElseFatalIntrinsic extends Module {
  val predicate = IO(Input(Bool()))
  val enable = IO(Input(Bool()))
  val x = IO(Input(Bool()))
  val y = IO(Input(UInt(3.W)))
  circt_chisel_ifelsefatal(clock, predicate, enable, "X is %x and Y is %d", x, y)
}

private class ChiselAssertAssumeIntrinsic extends Module {
  val predicate = IO(Input(Bool()))
  val enable = IO(Input(Bool()))
  val x = IO(Input(Bool()))
  val y = IO(Input(UInt(3.W)))
  circt_chisel_assert_assume(
    clock,
    predicate,
    enable,
    "X is %x and Y is %d",
    "label",
    Seq("GUARD1", "GUARD2"),
    x,
    y
  )

}

private class ChiselAssumeIntrinsic extends Module {
  val predicate = IO(Input(Bool()))
  val enable = IO(Input(Bool()))
  val x = IO(Input(Bool()))
  val y = IO(Input(UInt(3.W)))
  circt_chisel_assume(
    clock,
    predicate,
    enable,
    "X is %x and Y is %d",
    "label",
    Seq("GUARD1", "GUARD2"),
    x,
    y
  )
}

private class ChiselCoverIntrinsic extends Module {
  val predicate = IO(Input(Bool()))
  val enable = IO(Input(Bool()))
  circt_chisel_cover(clock, predicate, enable, "label", Seq("GUARD1", "GUARD2"))
}

class VerificationIntrinsicSpec extends AnyFlatSpec with Matchers {
  it should "work for Chisel IfElseFatal" in {
    val fir = ChiselStage.emitCHIRRTL(new ChiselIfElseFatalIntrinsic)
    (
      (fir.split('\n').map(_.trim.takeWhile(_ != '@')) should contain).allOf(
        "intmodule Chisel_IfElseFatal : ",
        "input clock : Clock",
        "input predicate : UInt<1>",
        "input enable : UInt<1>",
        "input args_0 : UInt<1>",
        "input args_1 : UInt<3>",
        "intrinsic = circt_chisel_ifelsefatal",
        "parameter format = \"X is %x and Y is %d\"",
        "module ChiselIfElseFatalIntrinsic : ",
        "input x : UInt<1> ",
        "input y : UInt<3> ",
        "inst inst of Chisel_IfElseFatal ",
        "connect inst.clock, clock ",
        "connect inst.predicate, predicate ",
        "connect inst.enable, enable ",
        "connect inst.args_0, x ",
        "connect inst.args_1, y "
      )
    )
  }
  it should "work for Chisel AssertAssume" in {
    val fir = ChiselStage.emitCHIRRTL(new ChiselAssertAssumeIntrinsic)
    (
      (fir.split('\n').map(_.trim.takeWhile(_ != '@')) should contain).allOf(
        "intmodule Chisel_Assert_Assume : ",
        "input clock : Clock",
        "input predicate : UInt<1>",
        "input enable : UInt<1>",
        "input args_0 : UInt<1>",
        "input args_1 : UInt<3>",
        "intrinsic = circt_chisel_assert_assume",
        "parameter format = \"X is %x and Y is %d\"",
        "parameter guards = \"GUARD1;GUARD2\"",
        "parameter label = \"label\"",
        "module ChiselAssertAssumeIntrinsic : ",
        "input x : UInt<1> ",
        "input y : UInt<3> ",
        "inst inst of Chisel_Assert_Assume ",
        "connect inst.clock, clock ",
        "connect inst.predicate, predicate ",
        "connect inst.enable, enable ",
        "connect inst.args_0, x ",
        "connect inst.args_1, y "
      )
    )
  }
  it should "work for Chisel Assume" in {
    val fir = ChiselStage.emitCHIRRTL(new ChiselAssumeIntrinsic)
    (
      (fir.split('\n').map(_.trim.takeWhile(_ != '@')) should contain).allOf(
        "intmodule Chisel_Assume : ",
        "input clock : Clock",
        "input predicate : UInt<1>",
        "input enable : UInt<1>",
        "input args_0 : UInt<1>",
        "input args_1 : UInt<3>",
        "intrinsic = circt_chisel_assume",
        "parameter format = \"X is %x and Y is %d\"",
        "parameter guards = \"GUARD1;GUARD2\"",
        "parameter label = \"label\"",
        "module ChiselAssumeIntrinsic : ",
        "input x : UInt<1> ",
        "input y : UInt<3> ",
        "inst inst of Chisel_Assume ",
        "connect inst.clock, clock ",
        "connect inst.predicate, predicate ",
        "connect inst.enable, enable ",
        "connect inst.args_0, x ",
        "connect inst.args_1, y "
      )
    )
  }
    it should "work for Chisel Cover" in {
    val fir = ChiselStage.emitCHIRRTL(new ChiselCoverIntrinsic)
    (
      (fir.split('\n').map(_.trim.takeWhile(_ != '@')) should contain).allOf(
        "intmodule Chisel_Cover : ",
        "input clock : Clock",
        "input predicate : UInt<1>",
        "input enable : UInt<1>",
        "intrinsic = circt_chisel_cover",
        "parameter guards = \"GUARD1;GUARD2\"",
        "parameter label = \"label\"",
        "module ChiselCoverIntrinsic : ",
        "inst inst of Chisel_Cover ",
        "connect inst.clock, clock ",
        "connect inst.predicate, predicate ",
        "connect inst.enable, enable ",
      )
    )
  }
}
