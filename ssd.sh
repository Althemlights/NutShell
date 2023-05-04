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


