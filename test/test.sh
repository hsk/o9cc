#!/bin/bash
assert() {
  expected="$1"
  input="$2"
  cflag=""
  if [ `uname -m` = "arm64" ]; then
    cflag="-arch x86_64"
  fi
  echo "$input" | dune exec -- o9cc   >& tmp.log
  cc $cflag -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 49 '49'
assert  6  '4+2'
assert  2  '4-2'
assert  9  '3*3'
assert  4  '12/3'

assert 47 '5+6*7'
assert 15 '5*(9-6)'
assert 4 '(3+5)/2'

echo OK