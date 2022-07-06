#!/bin/bash
# Author: Jiayuan
# Date: 2022-3-14
#!/bin/bash
cd ..
source env.sh
cd NutShell/
source env.sh

echo `date "+%Y-%m-%d %H:%M:%S"`
echo "The regression testing of SSDCore is in progress... (っ'-' )っ"


help(){
  echo "Usage:"
  echo "-b: BPU performance analysis"
  echo "-c: Coremark test"
  echo "-m: Generate verilog for test module"
  echo "-r: Run test cases of the specifoed directory"
  echo "-h: Help information"
}
cmarktest(){
  make clean
  make emu -j4
  build/emu -i /home/jy/xs-env/nexus-am/apps/coremark/build/coremark-riscv64-nutshell.bin --wave-path=wave.vcd -b 0 >1.log 2>&1
  python3 bpu.py
}
bputest(){
  make clean
  make emu -j4
  build/emu -i /home/jy/xs-env/nexus-am/apps/brancntest/build/branchtest-riscv64-nutshell.bin --wave-path=wave.vcd -b 0 >1.log 2>&1
  python3 bpu.py
}
moduleGenVerilog(){
  rm -rf moduleBuild/
  mkdir moduleBuild
  mill chiselModule.runMain top.moduleTop -td moduleBuild --output-file nutcore.v
}


#create_soft_link() {
#  mkdir ${1} 1>/dev/null 2>&1
#  find -L ${1} -type l -delete
#  FILES=`eval "find ${2} -mindepth 1 -maxdepth 1 -name ${3}"`
#  for FILE in ${FILES[@]}
#  do
#    eval "ln -s \"`realpath --relative-to="${1}" "$FILE"`\" \"${1}/${FILE##*/}\" 1>/dev/null 2>&1"
#  done
#}
#
#create_bin_soft_link() {
#  find -L $TEST_BIN_FLODER -maxdepth 1 -type l -delete
#  FOLDERS=`find bin -mindepth 1 -maxdepth 1 -type d`
#  for FOLDER in ${FOLDERS[@]}
#  do
#    SUBFOLDER=${FOLDER##*/}
#  eval "ln -s \"`realpath --relative-to="$BUILD_PATH" "$OSCPU_PATH/$FOLDER"`\" \"$BUILD_PATH/${FOLDER##*/}\" 1>/dev/null 2>&1"
#  done
#
#  # create soft link ($BUILD_PATH/*.bin -> $OSCPU_PATH/$BIN_FOLDER/*.bin). Why? Because of laziness!
#    create_soft_link $BUILD_PATH $OSCPU_PATH/$BIN_FOLDER \"*.bin\"
#}

while getopts "c b r:h m" OPT; do
  case $OPT in
    h) help;;
    c) cmarktest;;
    b) bputest;;
    m) moduleGenVerilog;;
    g) ;;
    r) TEST_CASES="$OPTARG";;
    ?) echo "Wrong Options";;
  esac
done
# Initialize variables
TEST_BIN_FLODER="testcases"
PASS_NUM=
FAIL_NUM=
TESTCASES_NUM=

#Build test bin floder
if [[ ! -d $TEST_BIN_FLODER ]]; then
    mkdir $TEST_BIN_FLODER
fi

#Run all test cases
if [[ -n $TEST_CASES ]]; then
  echo "Testing result..."
  mkdir log 1>/dev/null 2>&1
  PASS_NUM=0
  FAIL_NUM=0
  TESTCASES_NUM=
for FOLDER in ${TEST_CASES[@]}
    do
        BIN_FILES=`eval "find $FOLDER -mindepth 1 -maxdepth 1 -regex \".*\.\(bin\)\""`
        for BIN_FILE in $BIN_FILES; do
            FILE_NAME=`basename ${BIN_FILE%.*}`
            printf "[%23s] " $FILE_NAME
            LOG_FILE=log/$FILE_NAME-log.txt
            build/emu -i $BIN_FILE &> $LOG_FILE
            TESTCASES_NUM=`expr $TESTCASES_NUM + 1`
            if (grep 'HIT GOOD TRAP' $LOG_FILE > /dev/null) then
                echo -n -e "\033[1;32mPASS!\033[0m"
                rm $LOG_FILE
                PASS_NUM=`expr $PASS_NUM + 1`
            else
                echo -n -e "\033[1;31mFAIL!\033[0m"
                FAILL_NUM=`expr $FAILL_NUM + 1`
            fi
            if [ `expr $TESTCASES_NUM % 3` == 0 ]; then
                echo ""
            fi
        done
    done
    echo ""
    echo "A total of $TESTCASES_NUM cases were tested, $PASS_NUM passed and $FAILL_NUM failed"

fi


