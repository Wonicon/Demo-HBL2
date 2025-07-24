package coupledL2

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.utils._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLPermissions._

// IN: a, c
// OUT: a, c
class SinkMX(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // in
    val a = Flipped(DecoupledIO(new TLBundleA(edgeIn.bundle)))
    val c = Flipped(DecoupledIO(new TLBundleC(edgeIn.bundle)))
    // out
    val out_a = DecoupledIO(new TLBundleA(edgeIn.bundle))
    val out_c = DecoupledIO(new TLBundleC(edgeIn.bundle))
  })

  def isMatrixPut(a: TLBundleA): Bool = {
    (a.opcode === PutFullData || a.opcode === PutPartialData) &&
    (a.user.lift(MatrixKey).getOrElse(0.U) === 1.U)
  }

  def isMatrixGet(a: TLBundleA): Bool = {
    val en = a.opcode === Get && (a.user.lift(MatrixKey).getOrElse(0.U) === 1.U)
    en
  }

  val out_a = WireInit(io.a)
  val out_c = WireInit(io.c)

  val a = io.a.bits
  val c = io.c.bits

  // Determine out_a.valid:
  // 1. if a is not a matrix put, flow io.a to out_a
  // 2. if a is a matrix put, and in_c is not valid, flow io.a to out_c, handled above
  // 3. if a is a matrix put, and in_c is valid, stall a

  // Handle MatrixGet
  when(isMatrixGet(a) && io.a.valid) {
    out_a.bits.opcode := io.a.bits.opcode//Get Or AcquireBlock
    out_a.bits.param := NtoT
  }

  // Handle MatrixPut
  when(isMatrixPut(a) && io.a.valid) {
    // By anycase, CoupledL2 cannot accept Put from A channel.
    // Wait for C channel be spare.
    out_a.valid := false.B

    when(!io.c.valid) {
      // If c is not valid, convert the request to ReleaseData
      out_c.bits.opcode := ReleaseData
      out_c.bits.param := TtoN // Set appropriate parameters for ReleaseData
      out_c.bits.data := a.data // Use data from the A channel
      out_c.bits.address := a.address // Use address from the A channel
      out_c.bits.size := a.size
      out_c.bits.source := a.source
      out_c.bits.corrupt := a.corrupt
    //   out_c.bits.user(VaddrKey) := a.address
      out_c.valid := true.B
    //   io.a.ready := false.B // Hold the A channel until the C channel is processed
    }
  }

  // Connect output signals
  io.out_a <> out_a
  io.out_c <> out_c

  // Handle ready signals
  // io.a.ready := out_a.ready
  io.c.ready := out_c.ready
  // Bypass channel A matrix put to C, but stalled for original C channel requests.
  io.a.ready := Mux(isMatrixPut(a), out_c.ready && !io.c.valid, out_a.ready)
}