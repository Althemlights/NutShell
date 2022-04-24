/*
io.out1.bits.BypassCtl.rs2bypasse3 := VecInit(
  false.B,
  i1rs2valid && Valid(0) && i1rs2hitStage === 0.U && FuType(0).alu && FuType(0).subalu
    || Valid(0) && i1rs2hitStage === 0.U && FuType(0).load
    || Valid(0) && i1rs2hitStage === 0.U && FuType(0).mul,
  i1rs2valid && Valid(1) && i1rs2hitStage === 1.U && FuType(1).load
    || Valid(1) && i1rs2hitStage === 1.U && FuType(1).alu && FuType(1).subalu
    || Valid(1) && i1rs2hitStage === 1.U && FuType(1).mul,
  i1rs2valid && Valid(2) && i1rs2hitStage === 2.U && FuType(2).alu && FuType(2).subalu
    || Valid(2) && i1rs2hitStage === 2.U && FuType(2).load
    || Valid(2) && i1rs2hitStage === 2.U && FuType(2).mul,
  i1rs2valid && Valid(3) && i1rs2hitStage === 3.U && FuType(3).alu && FuType(3).subalu
    || Valid(3) && i1rs2hitStage === 3.U && FuType(3).load
    || Valid(3) && i1rs2hitStage === 3.U && FuType(3).mul
)*/
