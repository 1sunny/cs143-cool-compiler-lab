#!/bin/sh

_red='\033[1;31m'
_green='\033[1;32m'
_reset='\033[0m'

rm cgen
make cgen

if [ $? -gt 0 ]; then
  echo "one argument"
  exit 1
fi

# 检查命令行参数个数是否为1
if ! [ "$#" -eq 1 ]; then
  echo "\none argument, file or directory !"
  exit 1
fi

infiles=

if [ -d "$*" ]; then
    infiles=$(ls $*/*)
else
    infiles=$(ls $*)
fi

cnt=$((0))
correct_cnt=$((0))

for infile in $infiles; do
  ./mycoolc ${infile}
  echo ${infile}
  spim "test.s" > "test.out"
  coolc -o "ref.s" ${infile}
  spim "ref.s" > "ref.out"

  diff -y --suppress-common-lines "test.out" "ref.out" > diff.out
  if [ $? -eq 0 ]; then
    correct_cnt=$((correct_cnt+1))
    echo "[${_green}PASS${_reset}]\t$infile"
  else
    echo "[${_red}FAILE${_reset}]\t$infile"
    echo "-----------------------------------------------------------------"
    echo "left column is mycodegen output, right column is correct output"
    echo "-----------------------------------------------------------------"
    cat diff.out
    echo "-----------------------------------------------------------------"
    echo "press any key to continue"
    read answer
  fi

  cnt=$((cnt+1))
done

echo ""
echo "[cool-codegen Score] ${correct_cnt}/${cnt}"
