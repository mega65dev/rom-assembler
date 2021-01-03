# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       split.py
#      Purpose:    Split the ROM Source into the header, and localised chunks.
#      Created:    3rd January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

import re
#
#		Is line a label
#
def isLabel(s):
	return s[0].lower() >= 'a' and s[0].lower() <= 'z'
#
#		Is line a local label e.g. nn$
#
def isLocalLabel(s):
	if s[0] < '0' or s[0] > '9':
		return None
	m = re.match("^([0-9]+\\$)",s) 
	assert m is not None
	return m.group(1)

print("Splitting source code into localised chunks.")
src = open("original/b65.src","r").readlines()								# Read source in
src = [x.rstrip().replace("\t"," ")+" " for x in src]						# bit of preprocessing, no blank lines.
lineCount = len(src)														# number of lines.
#
#		Get rid of GKI.
#
src = [x.replace("GKI.","GKI__") for x in src]
#
#		Find where the first bit is.
#
p = 0																		# find the start of the program code.
while src[p].find("* = $2000") < 0: 						
	p+=1

open("parts/basic.header","w").write("\n".join(src[:p])) 					# write the header out.
outCount = len(src[:p])														# track how many written out.
#
#		Now work through the code dividing it into chunks that
#		have locals in them.
#
currentStart = p 															# where we are
partCount = 0 																# parts so far.

while currentStart < lineCount:												# until we've done the lot.
	partCount += 1
	foundLocal = False
	p = currentStart
	while p < lineCount and (not (isLabel(src[p]) and foundLocal)):			# look to global after locals
		if isLocalLabel(src[p]) is not None:								# found a local
			foundLocal = True 												# note that.
		p += 1

	open("parts/basic."+str(partCount),"w").write("\n".join(src[currentStart:p]))
	outCount += (p - currentStart)
	currentStart = p 														# start from next.

print("\tOriginal lines ",p)
print("\tTotal lines ",outCount)
print("\tCode Segments",partCount)

