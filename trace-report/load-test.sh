#!/usr/bin/env bash

set -euf

if ! [ $# -eq 1 ] ; then
    echo "Copies the generated C code to the genc directory, so that it can be compiled manually."
    echo
    echo "Usage: $0 <test-dir>"
    echo
    exit 2
fi

GENCDIR=genc
GENCSRC=quickcheckgen.c

GITROOT="$(git rev-parse --show-toplevel)"
TESTDIR="$1"

if ! [ -f "${TESTDIR}/${GENCSRC}" ] ; then
    echo "Error: ${TESTDIR}/${GENCSRC} doesn't exist."
    exit 1
fi

echo cp "${TESTDIR}/${GENCSRC}" "${GITROOT}/${GENCDIR}/${GENCSRC}"
cp "${TESTDIR}/${GENCSRC}" "${GITROOT}/${GENCDIR}/${GENCSRC}"
