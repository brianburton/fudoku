#!/bin/bash

dll=Fudoku/bin/Debug/net5.0/Fudoku.dll
soldir=../SudokuSolver
tmpdir=tmp

set -e

for i in $soldir/*.txt ; do
  name=`basename $i .txt`
  echo `date` ${name}.txt
  dotnet $dll $i > $tmpdir/${name}.txt
done
echo `date` done
