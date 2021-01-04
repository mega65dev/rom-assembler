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
		#		Casing errors.
		#
		if self.body == ".word  screen-1":
			self.body = ".word Screen-1"
		if self.body == ".word  C65__viewport-1":
			self.body = ".word C65__Viewport-1"
		if self.label == "Key_Load":
			self.label = "Key_load"
		#
		#		A label called "inz" at $6BD7 confuses it.
		#
		if self.label == "inz":
			self.label = "inz1"
		#
		#		A weird text string.
		#
		if self.label == "keydat":
			self.body = '!text "($RHC+",$22'
			self.currentPC += 7
			return True
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
		#		Check for .byte
		#
		if self.body.startswith(".byte"):
			self.processByte()
			return True
		#
		#		Check for .word
		#
		if self.body.startswith(".word"):
			self.processWord()
			return True
		#
		#		Check for equate e.g. x = 4
		#
		if self.label != "" and self.body.startswith("="):
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
		if romOpcode not in opList:
			print("ROM mismatch error looking for {0:02x}".format(romOpcode))
			sys.exit(0)
		#
		#		This fudge required because ACME BRA long does not work. It replaces
		#		long BRA with +lbra etc.
		#
		if 	romOpcode == 0x83 or romOpcode == 0x93 or romOpcode == 0xD3 or romOpcode == 0x13 or \
			romOpcode == 0xF3 or romOpcode == 0xB3 or romOpcode == 0x33 or romOpcode == 0x73 or \
			romOpcode == 0x53:
			self.body = "+l"+self.body
		#
		else: 
			if ((romOpcode & 0x1F) == 0x13):
				print("{0:02x} {1}".format(romOpcode,self.body))
				sys.exit(0)
		#
		self.currentPC += opList[romOpcode]										# adjust the program counter.
		#
		#		ACME does not like NEG A/DEC A
		#
		if self.body == "neg a" or self.body == "dec a" or self.body == "asl a" or \
		   self.body == "lsr a" or self.body == "rol a" or self.body == "ror a" or \
		   self.body == "inc a":
			self.body = self.body[:-1].strip()
		#
		#		We jump to a local, a really good idea.
		#
		if self.body == "jsr restore$1":
			self.body = "jsr restore__1"
		#
		#		A lda #';' gets mangled due to my lazy parsing.
		#
		if self.body == "lda #'":
			self.body = "lda #';'"
		#
		#		Fix binary constants which seem confusing.
		#
		if self.body == "lda #%11" or self.body == "lda #%10":
			self.body = "lda #{0}".format(int(self.body[-2:],2))
		#
		#		Handle INZ branch replacement.
		#
		if self.body == "beq inz":
			self.body = "beq inz1"
		#
		#		Precedence I think
		#
		if self.body == "lda #<beats_ntsc/4":
			self.body = "lda #(<beats_ntsc)/4"
	#			
	#		Read ROM.
	#		
	def readROM(self,addr):
		assert addr >= 0x2000
		if addr < 0x8000:
			return self.romImage[addr-0x2000]
		assert addr >= 0xAF00
		return self.romImage[addr-0x2000]
	#
	#		Process Byte.
	#
	def processByte(self):
		s = self.body[5:].strip()
		byteElements = []
		while s != "":
			if s.startswith("'"):												# quoted string. wierd use of 'x'+$80
				p = self.findQuote(s[1:])										# find matching closing quote
				assert p >= 0
				textPart = s[:p+2]												# get quoted part,
				textPart = textPart.replace("''","'")							# handle double quotes
				s = s[p+2:]														# and strip it.
				if s == "" or s.startswith(","):								# ordinary boring string found
					for i in range(0,len(textPart)-2):							# check it is in memory.
						assert self.readROM(self.currentPC+i) == ord(textPart[i+1])
					self.currentPC += len(textPart)-2 							# just add it as is
					byteElements.append('"'+textPart[1:-1]+'"')					# convert format to double quotes
					if s.startswith(","):										# throw connecting comma.
						s = s[1:]
				else:
					m = re.match("^(\\+\\$[0-9A-Fa-f]+)(\\,?)(.*)$",s)			# analyse it.
					assert m is not None and len(textPart) == 3 				# check ok and only one char in string
					byteValue = int(m.group(1)[2:],16)+ord(textPart[1])			# work out the actual ROM value and check
					assert self.readROM(self.currentPC) == byteValue
					self.currentPC += 1											# bump PC
					byteElements.append(textPart+m.group(1))					# add 'x'+$80 or whatever
					s = m.group(3)

			else:																# something else, which is probably a single byte.
				element = s.split(",")[0]										# if not we have problems. take it off.
				s = s[len(element)+1:].strip()
				if element[0] == "@": 											# octal conversion
					element = str(int(element[1:],8))
				byteElements.append(element)
				if re.match("^\\d+$",element) is not None:						# can we check it ? if so do so.
					assert self.readROM(self.currentPC) == int(element)		
				self.currentPC += 1 											# and add a byte
		#
		self.body = "!text "+",".join(byteElements)
	#
	#		Process word
	#
	def processWord(self):
		#
		#	Line at $2846 which has this comment in the code.
		#
		self.body = self.body.replace("[910109]","")
		#
		wordCount = len(self.body[5:].split(","))
		self.body = "!word "+self.body[5:].strip()
		self.currentPC += (wordCount * 2)
	#
	#		Find position of closing quote
	#
	def findQuote(self,s):
		p = 0
		while p < len(s):
			if s[p:p+2] == "''":
				p += 2
			else:
				if s[p] == "'":
					return p
				p += 1
		return s 

if __name__ == "__main__":
	hp = CodeProcessor()
	print("Converting segments to ACME.")
	for page in range(1,319):
		tgt = open("convert/basic_c."+str(page),"w")
		for l in PartLoader().load(page):
			#print("====== "+l+" ========================",hp.currentPC)
			startPC = 0 if hp.currentPC is None else hp.currentPC 
			p = hp.process(l)
			if p is not None:
				#tgt.write("{0} ;; @@{1:04x} {2}\n".format(p,startPC,page))
				tgt.write("{0}\n".format(p))
				#print("{0:2}:${1:02x} {2}".format(page,startPC,p))
			else:
				assert False,"Couldn't fathom {"+l+"}"
		tgt.close()