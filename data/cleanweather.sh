#!/usr/bin/env bash

for x in {21..27};
do tail -n+13 $x.csv |sed "s/[[:space:]]\{1,\}/,/g" |grep -v PRE > $x-clean.csv;
done
