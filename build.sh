set -e
rm -f parts/* tmp/* convert/*
python scripts/split.py
python scripts/headerprocessor.py 
python scripts/codeprocessor.py
python scripts/rejoin.py
acme -v2 --cpu m65 -o tmp/basic.bin -r tmp/basic.lst tmp/basic.asm
python scripts/check.py