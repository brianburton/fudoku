#!/bin/bash

dotnet build Fudoku
bash solve-all.sh
diff -rq old tmp
