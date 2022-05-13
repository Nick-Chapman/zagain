#/bin/bash

# run old/new interpreter...

# new (temp: pass stdin on comman line)
stack run -- reg "$@" $(cat -); exit

# old
(niz -no-line-wrap "$@" ; echo '[no more input]') \
    | tail +2 \
    | sed 's/ *  (/ (/g' \
    | sed 's/Print *"/Print "/' \
    | sed 's/Print \./Print "."/' \
    | sed 's/Print >/Print ">"/' \
    | sed 's/\\ *\\n/\\n/' \
    | cat
