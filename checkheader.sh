rm parts/* tmp/*
python scripts/split.py
python scripts/headerprocessor.py 
acme --cpu 65ce02 -r tmp/test.lst convert/basic_c.header
