rm parts/* tmp/* convert/*
python scripts/split.py
python scripts/headerprocessor.py 
python scripts/codeprocessor.py
python scripts/rejoin.py
acme --cpu m65 -r tmp/basic.lst tmp/basic.asm
