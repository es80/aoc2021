#!/usr/bin/env bash

set -e

if [[ -z $1 ]]; then
  diff <(./formatAndRun.sh) answers
  cat answers
else
  diff <(./formatAndRun.sh "$1") <(grep -A 2 "Day $1" answers)
  grep -A 2 "Day $1" answers
fi
