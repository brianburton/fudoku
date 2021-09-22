#!/bin/bash

for i in "$@" ; do
  echo "File  : $i"
  echo "Result: `tail -n1 $i`"
  echo Steps:
  grep ^Step $i | perl -pe 's@^.*Step [0-9]+: @@'|sort|uniq -c
  echo
done
