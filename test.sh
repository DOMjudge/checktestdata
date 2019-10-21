#!/bin/bash
IFS=$'\n'
progs=$(find tests -wholename "tests/testprog*.in" | sort -V)
for prog in $progs
do
  echo "* $prog"
  for data in $(ls "$(echo $prog | sed -e 's/testprog\([^.]*\)\..*/testdata\1/')".*)
  do
    echo "  - $data"
    ( ./checktestdata "$prog" "$data" || false ) &> check.log
    retval=$?
    if [[ ($prog == *.err*) || ($data == *.err*) ]]
    then
      retval=$((! retval))
    fi
    if [ "$retval" -ne 0 ]
    then
      cat check.log
      exit 1
    fi
  done
done

exit 0
