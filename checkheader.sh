rm parts/* tmp/*
python scripts/split.py
python scripts/headerprocessor.py 
acme  -r tmp/test.lst convert/basic.header
