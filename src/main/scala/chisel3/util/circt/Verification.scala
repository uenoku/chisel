package chisel3.util.circt

import chisel3._
import chisel3.internal._
import chisel3.experimental.IntrinsicModule
import chisel3.internal.Builder

import circt.Intrinsic

/** Create a module for `ifelsefatal` style assertion.
  */
private class Chisel_IfElseFatal[T <: Data](format: String, data: Seq[T])
    extends IntrinsicModule("circt_chisel_ifelsefatal", Map("format" -> format)) {
  val clock = IO(Input(Clock()))
  val predicate = IO(Input(Bool()))
  val enable = IO(Input(Bool()))
  val args = data.map({
    case d =>
      IO(Input(chiselTypeOf(d)))
  })
}

object circt_chisel_ifelsefatal {

  /** Generate a particular Verilog sequence that's similar to an assertion.
    * This corresponds to a default lowering of printf-encoded assertion (chisel3.assert). 
    *
    * @example {{{
    * circt_chisel_ifelse_fatal(clock, predicate, enable, "X is %d and Y is %d", "label", Seq("GUARD1;GUARD2"), x, y)
    * }}}
    */
  def apply(clock: Clock, predicate: Bool, enable: Bool, format: String, data: Bits*): Unit = {
    val inst = Module(new Chisel_IfElseFatal(format, data))
    inst.clock := clock
    inst.predicate := predicate
    inst.enable := enable
    inst.args
      .zip(data)
      .foreach({
        case (lhs, rhs) => lhs := rhs
      })

  }
}

private class Chisel_Assert_Assume[T <: Data](format: String, label: String, guards: String, data: Seq[T])
    extends IntrinsicModule(
      "circt_chisel_assert_assume",
      Map("format" -> format, "label" -> label, "guards" -> guards)
    ) {
  val clock = IO(Input(Clock()))
  val predicate = IO(Input(Bool()))
  val enable = IO(Input(Bool()))
  val args = data.map({
    case d =>
      IO(Input(chiselTypeOf(d)))
  })
}

private class Chisel_Assume[T <: Data](format: String, label: String, guards: String, data: Seq[T])
    extends IntrinsicModule("circt_chisel_assume", Map("format" -> format, "label" -> label, "guards" -> guards)) {
  val clock = IO(Input(Clock()))
  val predicate = IO(Input(Bool()))
  val enable = IO(Input(Bool()))
  val args = data.map({
    case d =>
      IO(Input(chiselTypeOf(d)))
  })
}

private class Chisel_Cover(label: String, guards: String)
    extends IntrinsicModule("circt_chisel_cover", Map("label" -> label, "guards" -> guards)) {
  val clock = IO(Input(Clock()))
  val predicate = IO(Input(Bool()))
  val enable = IO(Input(Bool()))
}



object circt_chisel_assert_assume {

  /** Generate a clocked SV assertion with companion assume statement for the SiFive verificatio library.
    * @example {{{
    * circt_chisel_assert_assume(clock, predicate, enable, "X is %d and Y is %d", "label", Seq("GUARD1;GUARD2"), x, y)
    * }}}
    */
  def apply(
    clock:     Clock,
    predicate: Bool,
    enable:    Bool,
    format:    String,
    label:     String,
    guards:    Seq[String],
    data:      Bits*
  ): Unit = {
    val inst = Module(new Chisel_Assert_Assume(format, label, guards.mkString(";"), data))
    inst.clock := clock
    inst.predicate := predicate
    inst.enable := enable
    inst.args
      .zip(data)
      .foreach({
        case (lhs, rhs) => lhs := rhs
      })
  }
}

object circt_chisel_assume {

  /** Generate a clocked assume statement for the SiFive verification library.
    *
    * @example {{{
    * circt_chisel_assume(clock, predicate, enable, "label", Seq("GUARD1;GUARD2"))
    * }}}
    */
  def apply(
    clock:     Clock,
    predicate: Bool,
    enable:    Bool,
    format:    String,
    label:     String,
    guards:    Seq[String],
    data:      Bits*
  ): Unit = {
    val inst = Module(new Chisel_Assume(format, label, guards.mkString(";"), data))
    inst.clock := clock
    inst.predicate := predicate
    inst.enable := enable
    inst.args
      .zip(data)
      .foreach({
        case (lhs, rhs) => lhs := rhs
      })
  }
}

object circt_chisel_cover {

  /** Generate a clocked cover statement for the SiFive verification library.
    *
    * @example {{{
    * circt_chisel_cover(clock, predicate, enable, "label", Seq("GUARD1;GUARD2"))
    * }}}
    */
  def apply(
    clock:     Clock,
    predicate: Bool,
    enable:    Bool,
    label:     String = "",
    guards:    Seq[String] = Seq.empty
  ): Unit = {
    val inst = Module(new Chisel_Cover(label, guards.mkString(";")))
    inst.clock := clock
    inst.predicate := predicate
    inst.enable := enable
  }
}
