package XiaoHe

import chisel3._
import chisel3.util._
import difftest._
class DIFFTESTIO extends NutCoreBundle{
    val dt_ld0 = new DiffLoadEventIO
    val dt_ld1 = new DiffLoadEventIO

    val dt_sb0 = new DiffSbufferEventIO
    val dt_sb1 = new DiffSbufferEventIO

    val dt_ic0 = new DiffInstrCommitIO
    val dt_ic1 = new DiffInstrCommitIO

    val dt_iw0 = new DiffIntWritebackIO
    val dt_iw1 = new DiffIntWritebackIO

    val dt_ae = new DiffArchEventIO

    val dt_te = new DiffTrapEventIO

    val dt_cs = new DiffCSRStateIO

    val dt_irs = new DiffArchIntRegStateIO
}