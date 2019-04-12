#!/usr/bin/env bash

for l in localsampledcorpus/*; do
  l_file=$(basename "$l")
  c=cumulativesampledcorpus/"c_${l_file##l_}"
  diff -u "$l" "$c"
done
