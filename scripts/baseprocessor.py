# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       baseprocessor.py
#      Purpose:    Handle very simple translations.
#      Created:    3rd January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

import re

# ***************************************************************************************************************
#
#													Processor base class
#
# ***************************************************************************************************************

class BaseProcessor(object):
	#
	#		Process very basic stuff
	#
	def process(self,s):
		st = s.strip()
		#
		#		Empty line.
		#
		if st == "":
			return st
		#
		#		Line comment.
		#
		if st[0] == ";":
			return s.strip()
		#
		#		Dot commands that are stripped out.
		#
		if st == ".page" or st[:7] == ".subttl" or st[:4] == ".nam" or st[:6] == ".STORE" or st[:6] == ".blist":
			return ""
		if st[:5] == ".ifgt" or st[:6] == ".messg" or st == ".endif" or st == ".end":
			return ""
		#
		#		Split into the relevant parts.
		#
		self.label = ""																
		self.comment = ""
		self.body = ""
		#
		if s[0] != ' ':																# is there a label ?
			m = re.match("^([0-9a-zA-Z\\_\\.]+)(.*)$",st)							# split it out.
			assert m is not None,"Bad line "+s
			self.label = m.group(1)
			s = m.group(2).strip()
		#
		if s.rfind(";") >= 0:														# is there a comment ?
			p = s.rfind(";")
			self.comment = "; "+s[p+1:].strip()
			s = s[:p]
		#
		self.body = s.strip()		
		#print(">>> [{0}|{1}|{2}]".format(self.label,self.body,self.comment))
		#
		if self.tryConvert():														# try to convert it.
			return "{0:16}{1:40}{2}".format(self.label,self.body,self.comment)
		return None
	#
	#		Simple converter. Handles label on its own.
	#
	def tryConvert(self):
		if self.label != "" and self.body == "":
			return True
		return False


if __name__ == "__main__":
	bp = BaseProcessor()
	for l in open("parts/basic.header").readlines():		
		p = bp.process(l)
		if p is not None:
			print(">>"+p)









