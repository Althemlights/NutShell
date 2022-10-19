make clean
rm ./build/*.vcd
make emu EMU_CXX_EXTRA_FLAGS="-DFIRST_INST_ADDRESS=0x80000000" EMU_TRACE=1 -j8
# ./build/emu -b 0 -e 350000 -i ~/xs-env/NutShell/ready-to-run/coremark-riscv64-nutshell.bin --dump-wave --diff=/home/bread/xs-env/NEMU/build/riscv64-nemu-interpreter-so >1.txt 2>&1
./build/emu -b 0 -e 100000 -i ~/xs-env/NutShell/ready-to-run/coremark-riscv64-nutshell-copy.bin --dump-wave --diff=/home/bread/xs-env/NEMU/build/riscv64-nemu-interpreter-so
# ./build/emu -b 0 -e 20000 -i ~/xs-env/NutShell/ready-to-run/jpz-riscv64-nutshell.bin --dump-wave --diff=/home/bread/xs-env/NEMU/build/riscv64-nemu-interpreter-so
# ./build/emu -b 0 -e 100000 -i ~/xs-env/NutShell/ready-to-run/jpz-riscv64-nutshell.bin --dump-wave --diff=/home/bread/xs-env/NEMU/build/riscv64-nemu-interpreter-so >1.txt 2>&1
