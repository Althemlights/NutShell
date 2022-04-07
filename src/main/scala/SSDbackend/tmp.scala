import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import top.Settings
import difftest._
import nutcore._

//printf(p"cacheLevel = $cacheLevel")
//printf(p"TotalSize = $TotalSize")
//printf(p"Ways = $Ways")
//printf(p"LineSize = $LineSize")
//printf(p"LineBeats = $LineBeats")
//printf(p"Sets = 4Sets")
//printf(p"OffsetBits = $OffsetBits")
//printf(p"IndexBits = $IndexBits")
//printf(p"WordIndexBits = $WordIndexBits")
//printf(p"TagBits = $TagBits")

