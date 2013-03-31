#!/bin/sh

chart() {
    echo "$2 (time in ns)"
    awk -v pf=$1 '
BEGIN { printf("    ") }
!s || s==$2 {
  printf("%s ", substr($3, pf, length($3) - pf + 1)); s=$2
}
END { print "" }
    ' $3
    
    awk '
$2 != s {
  if (s) print ""
  printf("%2d  ", $2)
}
{ printf("%8d ", $4); s=$2 }
END { print "" }
    ' $3
    echo ""
}

if [ $# -lt 1 ]; then
    echo "usage: $0 FILE"
    exit 1
fi

grep Build $1 > out.tmp1
grep Contains $1 > out.tmp2
grep Delete $1 > out.tmp3

chart 6 "building set" out.tmp1
chart 9 "membership tests" out.tmp2
chart 7 "removing elements" out.tmp3

date
uname -a
java -version 2>&1
