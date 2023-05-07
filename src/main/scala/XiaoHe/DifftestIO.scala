package XiaoHe

import chisel3._
import chisel3.util._
import difftest._
class DIFFTESTIO extends NutCoreBundle{
    val dt_ld0 = Flipped(new DiffLoadEventIO)
    val dt_ld1 = Flipped(new DiffLoadEventIO)

    val dt_sb0 = Flipped(new DiffStoreEventIO)
    val dt_sb1 = Flipped(new DiffStoreEventIO)

    val dt_ic0 = Flipped(new DiffInstrCommitIO)
    val dt_ic1 = Flipped(new DiffInstrCommitIO)

    val dt_iw0 = Flipped(new DiffIntWritebackIO)
    val dt_iw1 = Flipped(new DiffIntWritebackIO)

    val dt_ae = Flipped(new DiffArchEventIO)

    val dt_te = Flipped(new DiffTrapEventIO)

    val dt_cs = Flipped(new DiffCSRStateIO)

    val dt_irs = Flipped(new DiffArchIntRegStateIO)
}