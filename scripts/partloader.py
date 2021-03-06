# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       partloader.py
#      Purpose:    Loads a chunk and deals with variables.
#      Created:    3rd January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

# ***************************************************************************************************************
#
#											Load part, handling locals
#
# ***************************************************************************************************************

import re,sys

class PartLoader(object):
	#
	#		Load a part, processing locals.
	#
	def load(self,partID):
		src = open("parts/basic."+str(partID)).readlines()							# get code in.

		baseLabel = None 															# find a base label to use
		for ln in src:
			if ln != "" and ln[0].lower() >= 'a' and ln[0].lower() <= 'z':
				baseLabel = ln.split()[0].strip().replace("$","_")
		baseLabel = "l"+str(partID)

		labels = {}
		localCount = 0
		for ln in src:																# scan it
			if ln != "" and ln[0].lower() >= 'a' and ln[0].lower() <= 'z':			# found a normal label.
				label = ln.split()[0]												# get the label
				if label.find("$") >= 0:											# found a label$nn
					labels[label] = label.replace("$","__")							# add a sub in for that.
					
			if ln != "" and ln[0] >= '0' and ln[0] <= '9':							# found a label 0-9 a local.
				m = re.match("^([0-9]+\\$)",ln)
				assert m is not None,"Bad line "+ln
				assert m.group(1) not in labels,"Duplicate "+ln
				localCount += 1
				labels[m.group(1)] = baseLabel+"_"+str(localCount)
				PartLoader.labelCount += 1

		lKeys = [x for x in labels.keys()]											# keys
		lKeys.sort(key = lambda x:-len(x))											# sort/reverse puts smallest last
		#
		for i in range(0,len(src)):													# now do the replacement.
			if src[i].find("$") >= 0:												# a $ present ?
				for lk in lKeys:													# substitute all 20$ for labels.
					src[i] = src[i].replace(lk,labels[lk])
		#	
		return src		

PartLoader.labelCount = 1000														# unique label static.