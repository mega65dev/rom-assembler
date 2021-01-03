# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       codeprocessor.py
#      Purpose:    Translates the code
#      Created:    3rd January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

import re,os,sys
from baseprocessor import *
from partloader import *
from c65cpu import *

# ***************************************************************************************************************
#
#							Specialist processor for the header file.
#
# ***************************************************************************************************************

class CodeProcessor(BaseProcessor):
	def __init__(self):
		BaseProcessor.__init__(self)
		self.setProgramCounter(None)
		self.cpuInfo = Commodore65CPU()		
		self.romImage = open("original/b65.rom","rb").read(-1)
	#
	#		Set the PC value to the given value
	#
	def setProgramCounter(self,pc):
		self.currentPC = pc
	#
	#		Try to convert.
	#
	def tryConvert(self):
		#
		#		Check for * = <hex>
		#
		if self.body.startswith("*"):
			m = re.match("^\\*\\s*\\=\\s*\\$([0-9A-Fa-f]+)\\s*$",self.body)
			assert m is not None,self.body
			self.setProgramCounter(int(m.group(1),16))
			return True 
		#
		#		Check for an assembler mnemonic.
		#
		if self.body != "":
			word = self.body.split()[0]
			if self.cpuInfo.getOpcodeList(word) is not None:
				self.processCode(word)
				return True
		#
		#		Try superclass
		#
		return BaseProcessor.tryConvert(self)
	#
	#		Have a mnemonic. Check it is correct and adjust the PCTR.
	#
	def processCode(self,mnemonic):
		romOpcode = self.readROM(self.currentPC) 								# what is actually in the ROM ?
		opList = self.cpuInfo.getOpcodeList(mnemonic)							# the possible mnemonics it could be.
		assert romOpcode in opList
		self.currentPC += opList[romOpcode]										# adjust the program counter.
	#
	#		Read ROM.
	#		
	def readROM(self,addr):
		assert addr >= 0x2000
		if addr < 0x8000:
			return self.romImage[addr-0x2000]
		assert False

if __name__ == "__main__":
	hp = CodeProcessor()
	for page in range(1,4):
		for l in PartLoader().load(page):		
			p = hp.process(l)
			if p is not None:
				#print("{0:04x} {1}".format(hp.currentPC,p))
				pass
			else:
				print(l)
