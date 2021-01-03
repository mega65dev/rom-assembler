# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       headerprocessor.py
#      Purpose:    Translates the header.
#      Created:    3rd January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

import re
from baseprocessor import *

# ***************************************************************************************************************
#
#							Specialist processor for the header file.
#
# ***************************************************************************************************************

class HeaderProcessor(BaseProcessor):
	#
	#		Try to convert.
	#
	def tryConvert(self):
		#
		#	some label = something
		#
		if self.body.startswith("="):
			return True
		#
		#	Check for *=*+n
		#
		if self.body.startswith("*=*+"):
			self.body = "!fill "+self.body[4:]
			return True
		#
		#	Set label e.g. * = something
		#
		if re.match("^\\*\\s*\\=",self.body) is not None:
			return True

		return BaseProcessor.tryConvert(self)


if __name__ == "__main__":
	print("Converting basic.header to ACME")
	tgt = open("convert/basic.header","w")
	hp = HeaderProcessor()
	for l in open("parts/basic.header").readlines():		
		p = hp.process(l)
		if p is not None:
			tgt.write(p+"\n")
		else:
			raise Exception(l)
	tgt.close()