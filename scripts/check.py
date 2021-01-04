# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       check.py
#      Purpose:    Check ROM matches original and build final image.
#      Created:    4th January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

import sys

def checkRange(start,end):
	global errorCount
	for a in range(start,end):
		b1 = orgROM[a-0x2000]
		b2 = newROM[a]
		if b1 != b2:
			print("\tAt {0:04x} original {1:02x} new {2:02x}".format(a,b1,b2))
			errorCount+=1

print("Checking the ROM image ....")
errorCount = 0
orgROM = open("original/b65.rom","rb").read(-1)
newROM = open("tmp/basic.bin","rb").read(-1)

checkRange(0x2000,0x8000)
checkRange(0xAf00,len(newROM))
print("\t{0} errors.".format(errorCount))
if errorCount != 0:
	sys.exit(1)


sys.exit(0)	