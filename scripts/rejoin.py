# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       rejoin.py
#      Purpose:    Put the code back together
#      Created:    4th January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

print("Rebuilding source file")
srcFiles = [ "basic_c.header" ]
for i in range(1,319):
	srcFiles.append("basic_c."+str(i))
tgt = open("tmp/basic.asm","w")
for f in srcFiles:
	for l in open("convert/"+f).readlines():
		tgt.write(l.rstrip()+"\n")
tgt.close()