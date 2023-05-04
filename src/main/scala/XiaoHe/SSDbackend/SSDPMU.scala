package SSDbackend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class PMUIO0 extends Bundle{
  val normali0 = Output(Bool())
  val normali1 = Output(Bool())

  val frontendi0 = Output(Bool())
  val frontendi1 = Output(Bool())

  val laterStageStalli0 = Output(Bool())
  val laterStageStalli1 = Output(Bool())

  val loadRsNotreadyi1 = Output(Bool())
  val load_sub_rs1_i1 = Output(Bool())
  val load_md_rs1_i1 = Output(Bool())
  val load_load_rs1_i1 = Output(Bool())
  val load_sub_rs2_i1 = Output(Bool())
  val load_md_rs2_i1 = Output(Bool())
  val load_load_rs2_i1 = Output(Bool())

  val storeRsNotreadyi1 = Output(Bool())
  val mulRsNotreadyi1 = Output(Bool())
  val divRsNotreadyi1 = Output(Bool())

  val i1Stalli0 = Output(Bool())
  val hitSubalui0 = Output(Bool())
  val bothLsui0 = Output(Bool())
  val bothBrui0 = Output(Bool())
  val LsuBri0 = Output(Bool())

  val loadRsNotreadyi0 = Output(Bool())
  val storeRsNotreadyi0 = Output(Bool())
  val mulRsNotreadyi0 = Output(Bool())
  val divRsNotreadyi0 = Output(Bool())

}
class ALU2PMUIO extends Bundle {
  val branchRight = Output(Bool())
  val branchWrong = Output(Bool())
  val jalRight = Output(Bool())
  val jalWrong = Output(Bool())
  val jalrRight = Output(Bool())
  val jalrWrong = Output(Bool())
  val retRight = Output(Bool())
  val retWrong = Output(Bool())
  val branchTargetWrong = Output(Bool())
  val branchDirectionWrong = Output(Bool())
}
class PMUIO1 extends Bundle {
  val branchRight = Output(UInt(3.W))
  val branchWrong = Output(UInt(3.W))
  val jalRight = Output(UInt(3.W))
  val jalWrong = Output(UInt(3.W))
  val jalrRight = Output(UInt(3.W))
  val jalrWrong = Output(UInt(3.W))
  val retRight = Output(UInt(3.W))
  val retWrong = Output(UInt(3.W))
  val branchTargetWrong = Output(UInt(3.W))
  val branchDirectionWrong = Output(UInt(3.W))
}
class PMUIO2 extends Bundle{
  val branchInst = Output(UInt(2.W))
  val jalInst = Output(UInt(2.W))
  val jalrInst = Output(UInt(2.W))
  val retInst = Output(UInt(2.W))

  val loadInst = Output(UInt(2.W))
  val storeInst = Output(UInt(2.W))
  val mulInst = Output(UInt(2.W))
  val divInst = Output(UInt(2.W))
}

class PMU extends Module{
  val io = IO(new Bundle {
    val in0 = Flipped(new PMUIO0)
    val in1 = Flipped(new PMUIO1)
    val in2Issue = Flipped(new PMUIO2)
    val in2Commit = Flipped(new PMUIO2)
    val coreTrap = Input(Bool())
    val cycleCnt = Input(UInt(64.W))
  })

  val in0 = io.in0
  val issue1Vec = VecInit(Seq(in0.normali1,in0.frontendi1,in0.laterStageStalli1,in0.loadRsNotreadyi1,in0.storeRsNotreadyi1,in0.mulRsNotreadyi1,in0.divRsNotreadyi1))
  val issue0Vec = VecInit(Seq(in0.normali0,in0.frontendi0,in0.laterStageStalli0,in0.i1Stalli0,in0.hitSubalui0,in0.bothLsui0,in0.LsuBri0,
    in0.loadRsNotreadyi0,in0.storeRsNotreadyi0,in0.mulRsNotreadyi0,in0.divRsNotreadyi0))
  //so ugly
  val issue1Num = WireInit(0.U(4.W))
  val issue0Num = WireInit(0.U(4.W))
  issue1Num := in0.normali1+in0.frontendi1+in0.laterStageStalli1+in0.loadRsNotreadyi1+in0.storeRsNotreadyi1+in0.mulRsNotreadyi1+in0.divRsNotreadyi1
  issue0Num := in0.normali0+in0.frontendi0+in0.laterStageStalli0+in0.i1Stalli0+in0.hitSubalui0+in0.bothLsui0+in0.LsuBri0+in0.bothBrui0+
    in0.loadRsNotreadyi0+in0.storeRsNotreadyi0+in0.mulRsNotreadyi0+in0.divRsNotreadyi0
//  assert(issue1Num === 1.U)
//  assert(issue0Num === 1.U)
  dontTouch(issue1Num)
  dontTouch(issue0Num)

  //stall counter
  val StallCntList = Map(
    io.in0.normali0             ->   (0x00,  "normali0          "),
    io.in0.frontendi0           ->   (0x01,  "frontendi0        "),
    io.in0.laterStageStalli0    ->   (0x02,  "laterStageStalli0 "),
    io.in0.i1Stalli0            ->   (0x03,  "i1Stalli0         "),
    io.in0.hitSubalui0          ->   (0x04,  "hitSubalui0       "),
    io.in0.bothLsui0            ->   (0x05,  "bothLsui0         "),
    io.in0.bothBrui0            ->   (0x12,  "bothBrui0         "),
    io.in0.LsuBri0              ->   (0x06,  "LsuBri0           "),
    io.in0.loadRsNotreadyi0     ->   (0x07,  "loadRsNotreadyi0  "),
    io.in0.storeRsNotreadyi0    ->   (0x08,  "storeRsNotreadyi0 "),
    io.in0.mulRsNotreadyi0      ->   (0x09,  "mulRsNotreadyi0   "),
    io.in0.divRsNotreadyi0      ->   (0x0a,  "divRsNotreadyi0   "),

    io.in0.normali1             ->   (0x0b,  "normali1          "),
    io.in0.frontendi1           ->   (0x0c,  "frontendi1        "),
    io.in0.laterStageStalli1    ->   (0x0d,  "laterStageStalli1 "),
    io.in0.loadRsNotreadyi1     ->   (0x0e,  "loadRsNotreadyi1  "),
    io.in0.storeRsNotreadyi1    ->   (0x0f,  "storeRsNotreadyi1 "),
    io.in0.mulRsNotreadyi1      ->   (0x10,  "mulRsNotreadyi1   "),
    io.in0.divRsNotreadyi1      ->   (0x11,  "divRsNotreadyi1   "),

    io.in0.load_sub_rs1_i1     ->   (0x12,  "load_sub_rs1_i1    "),
    io.in0.load_md_rs1_i1      ->   (0x13,  "load_md_rs1_i1     "),
    io.in0.load_load_rs1_i1    ->   (0x14,  "load_load_rs1_i1   "),
    io.in0.load_sub_rs2_i1     ->   (0x12,  "load_sub_rs2_i1    "),
    io.in0.load_md_rs2_i1      ->   (0x13,  "load_md_rs2_i1     "),
    io.in0.load_load_rs2_i1    ->   (0x14,  "load_load_rs2_i1   ")
  )
  val stallCntNum = 25
  val stallCnts = List.fill(stallCntNum)(RegInit(0.U(64.W)))
  val stallCntCond = List.fill(stallCntNum)(WireInit(false.B))
  if(SSDCoreConfig().EnablePMU){(stallCnts zip stallCntCond).map{ case(a,b) => { when(b) { a := a + 1.U }}}}
  StallCntList.map{ case(a,(b,c)) => {stallCntCond(b) := a}}

  if(SSDCoreConfig().EnableStallCnt) {
    when(RegNext(io.coreTrap)) {
      StallCntList.map { case (a, (b, c)) => {printf(c + " ->  %d\n", stallCnts(b))}
      }
    }
  }

  //general performance counter
  val PerfCntList = Map(
    io.in1.branchRight           ->   (0x00,  "branchRight       "),
    io.in1.branchWrong           ->   (0x01,  "branchWrong       "),
    io.in1.jalRight              ->   (0x02,  "jalRight          "),
    io.in1.jalWrong              ->   (0x03,  "jalWrong          "),
    io.in1.jalrRight             ->   (0x04,  "jalrRight         "),
    io.in1.jalrWrong             ->   (0x05,  "jalrWrong         "),
    io.in1.retRight              ->   (0x06,  "retRight          "),
    io.in1.retWrong              ->   (0x07,  "retWrong          "),
    io.in1.branchTargetWrong     ->   (0x08,  "branchTgtWrong    "),
    io.in1.branchDirectionWrong  ->   (0x09,  "branchDrctWrong   ")
  )
  val perfCntNum = 10
  val perfCnts = List.fill(perfCntNum)(RegInit(0.U(64.W)))
  val perfIncrease = List.fill(perfCntNum)(WireInit(0.U(3.W)))
  if(SSDCoreConfig().EnablePMU){(perfCnts zip perfIncrease).map{ case(a,b) => { a := a + b }}}
  PerfCntList.map{ case(a,(b,c)) => {perfIncrease(b) := a}}

  if(SSDCoreConfig().EnableBPUCnt) {
    when(RegNext(io.coreTrap)) {
      PerfCntList.map { case (a, (b, c)) => {printf(c + " ->  %d\n", perfCnts(b))}
      }
    }
  }

  //inst counter
  val instCntList = Map(
    io.in2Issue.branchInst        ->   (0x00,  "branchInstIssue   "),
    io.in2Issue.jalInst           ->   (0x01,  "jalInstIssue      "),
    io.in2Issue.jalrInst          ->   (0x02,  "jalrInstIssue     "),
    io.in2Issue.retInst           ->   (0x03,  "retInstIssue      "),
    io.in2Issue.loadInst          ->   (0x04,  "loadInstIssue     "),
    io.in2Issue.storeInst         ->   (0x05,  "storeInstIssue    "),
    io.in2Issue.mulInst           ->   (0x06,  "mulInstIssue      "),
    io.in2Issue.divInst           ->   (0x07,  "divInstIssue      "),
    io.in2Commit.branchInst       ->   (0x08,  "branchInstCommit  "),
    io.in2Commit.jalInst          ->   (0x09,  "jalInstCommit     "),
    io.in2Commit.jalrInst         ->   (0x0a,  "jalrInstCommit    "),
    io.in2Commit.retInst          ->   (0x0b,  "retInstCommit     "),
    io.in2Commit.loadInst         ->   (0x0c,  "loadInstCommit    "),
    io.in2Commit.storeInst        ->   (0x0d,  "storeInstCommit   "),
    io.in2Commit.mulInst          ->   (0x0e,  "mulInstCommit     "),
    io.in2Commit.divInst          ->   (0x0f,  "divInstCommit     ")
  )

  val instCntNum = 16
  val instCnts = List.fill(instCntNum)(RegInit(0.U(64.W)))
  val instIncrease = List.fill(instCntNum)(WireInit(0.U(2.W)))
  if(SSDCoreConfig().EnablePMU){(instCnts zip instIncrease).map{ case(a,b) => { a := a + b }}}
  instCntList.map{ case(a,(b,c)) => {instIncrease(b) := a}}


  if(SSDCoreConfig().EnableInstCnt) {
    when(RegNext(io.coreTrap)) {
      instCntList.map { case (a, (b, c)) => {printf(c + " ->  %d\n", instCnts(b))}
      }
    }
  }
  val cacheCntList = Map(
    "icacheLoad"                  ->   (0x00,  "icacheLoad        "),
    "icacheStore"                 ->   (0x01,  "icacheStore       "),
    "icacheLoadMiss"              ->   (0x02,  "icacheLoadMiss    "),
    "icacheStoreMiss"             ->   (0x03,  "icacheStoreMiss   "),
    "dcacheLoad"                  ->   (0x04,  "dcacheLoad        "),
    "dcacheStore"                 ->   (0x05,  "dcacheStore       "),
    "dcacheLoadMiss"              ->   (0x06,  "dcacheLoadMiss    "),
    "dcacheStoreMiss"             ->   (0x07,  "dcacheStoreMiss   "),
    //tmp
    "pmuUpdateCnt"                ->   (0x08,  "pmuUpdateCnt      ")
//    "pmuBranshReqCnt"             ->   (0x08,  "pmuBranshReqCnt   "),
//    "pmuJalReqCnt"                ->   (0x08,  "pmuJalReqCnt      "),
//    "pmuJalrReqCnt"               ->   (0x08,  "pmuJalrReqCnt     "),
//    "pmuRetReqCnt"                ->   (0x08,  "pmuRetReqCnt      "),
  )

  val cacheCntNum = 9
  val cacheCnts = List.fill(cacheCntNum)(RegInit(0.U(64.W)))
  val cacheCntCond = List.fill(cacheCntNum)(WireInit(false.B))
  if(SSDCoreConfig().EnablePMU){(cacheCnts zip cacheCntCond).map{ case(a,b) => { when(b) { a := a + 1.U }}}}
  cacheCntList.map{ case(a,(b,c)) => { BoringUtils.addSink(cacheCntCond(b),a)}}


  if(SSDCoreConfig().EnableCacheCnt) {
    when(RegNext(io.coreTrap)) {
      cacheCntList.map { case (a, (b, c)) => {printf(c + " ->  %d\n", cacheCnts(b))}
      }
    }
  }
}


