#!/bin/bash

_red='\e[31m'
_green='\e[32m'
_reset='\e[0m'

rm semant
make semant

if [ $? -gt 0 ]; then
  exit 1
fi

# 检查命令行参数个数是否为1
if ! [ "$#" -eq 1 ]; then
  echo "\none argument, file or directory !"
  exit 1
fi

infiles=

TEST_FILE_DIR=$*
if [ -d "$*" ]; then
    infiles=$(ls $*/*)
else
    infiles=$(ls $*)
fi

cnt=$((0))
correct_cnt=$((0))

for infile in $infiles; do
  echo $infile
  ./lexer $infile | ./parser | ../../bin/semant > ref.out 2>&1
  # all test pass
  ./lexer $infile | ./parser | ./semant > my.out 2>&1
  # some test line number differ
  #./mylexer $infile | ./myparser | ./semant > my.out 2>&1
#  ./mysemant $infile 2>&1 |\
#    awk '{
#              reg = "^.*:[0-9]+:[ ]";
#              sub(reg, "", $0);
#              reg = "'${TEST_FILE_DIR}'\/";
#              sub(reg, "", $0);
#              reg = "#[0-9]+[\r\n]*$";
#              sub(reg, "", $0);
#              reg = "line[ ][0-9]+";
#              sub(reg, "", $0);
#              print $0
#           }' > A
#
#  cat ${infile}.out |\
#    awk '{
#              reg = "^.*:[0-9]+:[ ]";
#              sub(reg, "", $0);
#              reg = "#[0-9]+[\r\n]*$";
#              sub(reg, "", $0);
#              reg = "line[ ][0-9]+";
#              sub(reg, "", $0);
#              print $0
#          }' > B

#  diff -y A B > diff.out

  diff -y my.out ref.out > diff.out
  if [ $? -eq 0 ]; then
    correct_cnt=$((correct_cnt+1))
    echo -e "[${_green}PASS${_reset}]\t$infile"
  else
    echo -e "[${_red}FAILE${_reset}]\t$infile"
    echo "-----------------------------------------------------------------"
    echo "left column is mysemant output, right column is correct output"
    echo "-----------------------------------------------------------------"
    cat diff.out
    echo "-----------------------------------------------------------------"
    echo "press any key to continue"
    read answer
  fi

  cnt=$((cnt+1))
done

echo ""
echo "[cool-semant Score] ${correct_cnt}/${cnt}"
