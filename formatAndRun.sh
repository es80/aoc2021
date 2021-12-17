#!/usr/bin/env bash

set -e

function day() {
  DAY=src/Day"$1".hs
  brittany --columns 100 --write-mode=inplace "$DAY"
  hlint --no-summary "$DAY"
  runghc "$DAY" input/input"$1"
}

DAY=$1
if [[ -z $DAY ]]; then
  for i in {01..17}; do
    echo "Day $i"
    day "$i"
    echo
  done
else
  echo "Day $DAY"
  day "$DAY"
fi
