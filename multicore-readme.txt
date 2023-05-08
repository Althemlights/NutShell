关于multicore:

需要确保NEMU:
1.编译NEMU的configs为riscv64-dual-xs-ref_defconfig
2.编译后将build/riscv64-nemu-interpreter-dual-so重命名为riscv64-wukong-interpreter-dual-so

需要确保 NutShell:
1.NutShell/difftest/Makefile NUM_CORES为2
2.difftest/src/test/csrc/difftest NEMU_SO_FILENAME "build/riscv64-wukong-interpreter-dual-so"
3.src/main/scala/top/Settings: CoreNums 改为2
编译命令：  make emu EMU_CXX_EXTRA_FLAGS="-DFIRST_INST_ADDRESS=0x80000000" EMU_TRACE=1 -j8

多核test:
1.ready-to-run/ldvio-riscv64-nutshell.bin
运行命令：  ./build/emu -i ./ready-to-run/ldvio-riscv64-nutshell.bin

// Dcache Icache DataTemplate
Index bits: 7   Ways: 4   Banks: 4  SramTemplate Addr bits: 7 + 1 = 8  SramDatabits: 6