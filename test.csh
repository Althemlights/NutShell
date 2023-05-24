make clean
make emu BOARD=sim EMU_CXX_EXTRA_FLAGS="-DFIRST_INST_ADDRESS=0x80000000" EMU_TRACE=1 -j8
./build/emu -i ready-to-run/rtthread.bin
# ./ssd.sh -r ./ready-to-run/all/