#/bin/bash

(niz -no-line-wrap story/zork1.88-840726.z3 -trace; echo '[no more input]') \
    | sed 's/ *  (/ (/g' \
    | sed 's/Print *"/Print "/' \
    | sed 's/Print \./Print "."/' \
    | sed 's/Print >/Print ">"/' \
    | sed 's/\\ *\\n/\\n/' \
          > run.expected.full
