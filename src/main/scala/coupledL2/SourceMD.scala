/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package coupledL2

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._

class SourceMD(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    // receive task from MainPipe
    val d_task = Flipped(DecoupledIO(new TaskWithData()))
    // send Release/Grant/ProbeAck via D channels
    val toSourceD = DecoupledIO(new TaskWithData())
    // send GrantMatrixData via  MD channels
    val toMatrixD = DecoupledIO(new MatrixDataBundle())
  })

  val mdata_enq = Wire(io.toMatrixD.cloneType)
  val mdata_deq = Queue(mdata_enq, 128, pipe = true, flow = true)

  mdata_enq.valid := false.B
  mdata_enq.bits.data.data := 0.U
  mdata_enq.bits.sourceId := 0.U
  mdata_enq.bits.channel := 0.U

  io.toSourceD <> io.d_task

  when (io.d_task.valid && io.d_task.bits.task.matrixTask) {
    mdata_enq.valid := true.B
    mdata_enq.bits.data := io.d_task.bits.data
    mdata_enq.bits.sourceId := io.d_task.bits.task.ameIndex
    mdata_enq.bits.channel := io.d_task.bits.task.ameChannel
    assert(io.d_task.bits.task.ameChannel < 8.U, "channel must be valid value (0~7) in matrix task")
    io.toSourceD.valid := false.B
  }

  io.toMatrixD <> mdata_deq
}
