# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       fsplit.py
#      Purpose:    Functional split of C65 Basic Source code.
#      Created:    4th January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

import re,os

currentFile = None												# File being created
sourceList = []													# List of source files.
sourceCode = {}													# Code for each source file.
includes = []

#
#		Split up around the [[<name>]] markups.
#
print("Building segments")
for src in [x.rstrip() for x in open("tmp/basic.asm").readlines()]:
	if src.startswith(";[["):
		m = re.match("^\\;\\[\\[(.*)\\]\\]\\s*$",src)
		assert m is not None,"Bad section "+src
		currentFile = m.group(1)
		assert currentFile not in sourceCode,"Duplicate "+currentFile
		sourceList.append(currentFile)
		sourceCode[currentFile] = []
	else:
		sourceCode[currentFile].append(src)
#
#		Create the individual files.
#
for s in sourceList:
	target = (s.replace(".",os.sep)+".asm").split(os.sep)
	directory = os.sep.join(target[:-1])
	if directory != "":
		path = "source"+os.sep+directory
		if not os.path.exists(path):
			os.makedirs(path)
	h = open("source"+os.sep+os.sep.join(target),"w")
	h.write("\n".join(sourceCode[s]))
	h.close()
	includes.append(os.sep.join(target))
#
#		Create the master file
#
h = open("source/basic.asm","w")
for i in includes:
	file = '!source "{0}"'.format(i)
	h.write(file+"\n")
h.close()

