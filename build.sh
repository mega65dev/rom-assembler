#!/bin/bash
#
#		ROM conversion code.
#
set -e
rm -f parts/* tmp/* convert/*
rm -Rf source/*
#
#		Split into local groups
#
python scripts/split.py
#
#		Convert basic.header
#
python scripts/headerprocessor.py 
#
#		Convert basic groups
#
python scripts/codeprocessor.py
#
#		Stick back together and assemble
#
python scripts/rejoin.py
acme -v1 --cpu m65 -o tmp/basic.bin -r tmp/basic.lst tmp/basic.asm
#
#		Binary check & ROM construction
#
python scripts/check.py
#
#		Check the binary matches.
#
cmp bin/rom.bin original/b65.rom
#
#		Build the 'split files' version
#
python scripts/fsplit.py
#
#		Reassemble this version.
#
pushd source
acme -v1 --cpu m65 -o ../tmp/basic.bin -r ../tmp/basic.lst basic.asm
popd
#
#		Check and recreate again.
#
python scripts/check.py
cmp bin/rom.bin original/b65.rom
