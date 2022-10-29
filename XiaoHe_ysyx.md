1. 在NutShell目录下执行 mill chiselModule.runMain top.ysyx BOARD=soctest
2. 在目录下找到ysyx_210062.v，把ysyx_210062_ysyx_210062改成ysyx_210062（划到最后的最后一个module)
3. 把ysyx_210062.v复制到SoC-main/projects/soc/vsrc
4. 在SoC目录下执行./build.sh -e soc -b -s -y -v '--timescale "1ns/1ns" -Wno-fatal --trace' -a "-i ysyxSoC/flash/hello-flash.bin --dump-wave"


