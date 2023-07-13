#!/bin/sh

_red='\033[1;31m'
_green='\033[1;32m'
_reset='\033[0m'

rm lexer
make lexer

if [ $? -gt 0 ]; then
  exit 1
fi

TEST_FILE_DIR=lexer-test-files
infiles=$(ls ${TEST_FILE_DIR}/*.cool)
cnt=$((0))
correct_cnt=$((0))
for infile in $infiles; do
  ./lexer $infile |\
    awk '{
          if (NR != 1) {
            for (i=2; i<=NF; i++)
              printf "%s ", $i; printf "\n"; 
            } 
         }' > A
  awk '{
          if (NR != 1) {
            for (i=2; i<=NF; i++)
              printf "%s ", $i;
            printf "\n"; 
          } 
       }' ${infile}.out > B

  diff -y --horizon-lines=1000 --suppress-common-lines A B > diff.out

  if [ $? -eq 0 ]; then
    correct_cnt=$((correct_cnt+1))
    echo "[${_green}PASS${_reset}]\t$infile"
  else
    echo "[${_red}FAILE${_reset}]\t$infile"
    echo "-----------------------------------------------------------------"
    echo "left column is mylexer output, right column is correct output"
    echo "-----------------------------------------------------------------"
    cat diff.out
    echo "-----------------------------------------------------------------"
    echo "press any key to continue"
    read answer
  fi

  cnt=$((cnt+1))
done
echo ""
echo "[cool-lexer Score] ${correct_cnt}/${cnt}"

