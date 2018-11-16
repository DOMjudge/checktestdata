#!/bin/bash
IFS=$'\n'
progs=$(find tests/ | grep testprog | sort -V)
for prog in $progs
do
  for data in $(ls "$(echo $prog | sed -e 's/testprog\([^.]*\)\..*/testdata\1/')".*)
  do
    echo checktestdata $prog $data
    ./checktestdata $prog $data
    retval=$?
    if [[ ($prog == *.err*) || ($data == *.err*) ]]
    then
      retval=$[! $retval]
    fi
    if [ "$retval" -ne 0 ]
    then
      exit 1
    fi
  done
done
exit $retval
