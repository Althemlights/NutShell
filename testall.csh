make clean
make emu EMU_CXX_EXTRA_FLAGS="-DFIRST_INST_ADDRESS=0x80000000" -j8
./ssd.sh -r ./ready-to-run/all/
