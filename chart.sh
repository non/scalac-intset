#!/bin/sh

chart() {
    echo "$2 (time in ns)"
    awk -v pf=$1 '
BEGIN { printf("    ") }
!s || s==$2 {
  printf("%s ", substr($3, pf, length($3) - pf + 1)); s=$2
}
END { print "" }
    ' out2.build
    
    awk '
$2 != s {
  if (s) print ""
  printf("%2d  ", $2)
}
{ printf("%8d ", $4); s=$2 }
END { print "" }
    ' out2.build
    echo ""
}

chart 6 "building set"
chart 8 "membership tests"
chart 6 "removing elements"
