#!/bin/sh

_red='\033[1;31m'
_green='\033[1;32m'
_reset='\033[0m'

rm parser
make parser

if [ $? -gt 0 ]; then
  exit 1
fi

TEST_FILE_DIR=parser-test-files
infiles=$(ls ${TEST_FILE_DIR}/*.test)
cnt=$((0))
correct_cnt=$((0))
for infile in $infiles; do
  # lexer $infile | ./parser 2>&1 |\
  ./myparser $infile 2>&1 |\
    awk '{
            reg = "'${TEST_FILE_DIR}'\/";
            sub(reg, "", $0);
            reg = "#[0-9]+[\r\n]*$";
            sub(reg, "", $0);
            reg = "line[ ][0-9]+";
            sub(reg, "", $0);
            print $0
         }' > A

    cat ${infile}.out |\
      awk '{
            reg = "#[0-9]+[\r\n]*$";
            sub(reg, "", $0);
            reg = "line[ ][0-9]+";
            sub(reg, "", $0);
            print $0
           }' > B

  diff -y A B > diff.out

  if [ $? -eq 0 ]; then
    correct_cnt=$((correct_cnt+1))
    echo "[${_green}PASS${_reset}]\t$infile"
  else
    echo "[${_red}FAILE${_reset}]\t$infile"
    echo "-----------------------------------------------------------------"
    echo "left column is mylparser output, right column is correct output"
    echo "-----------------------------------------------------------------"
    cat diff.out
    echo "-----------------------------------------------------------------"
    echo "press any key to continue"
    read answer
  fi

  cnt=$((cnt+1))
done

echo ""
echo "[cool-parser Score] ${correct_cnt}/${cnt}"

rm A B diff.out
