# ***************************************************************************************************************
# ***************************************************************************************************************
#
#      Name:       c65cpu.py
#      Purpose:    
#      Created:    3rd January 2020
#      Author:     Paul Robson (paul@robsons.org.uk)
#
# ***************************************************************************************************************
# ***************************************************************************************************************

import re

# ***************************************************************************************************************
#
#								Information class about the processor
#
# ***************************************************************************************************************

class Commodore65CPU(object):
    #
    #   Set up structures
    #		
    def __init__(self):
    	self.nameBytes = {} 							# for each opcode, all the possible opcodes and sizes
    	self.toName = {}								# opcode to name.
    	#
    	for s in [x.strip().lower() for x in self.getSource().strip().split("\n") if x.strip() != ""]:
    		m = re.match("^([a-z0-9]+)\\s*(.*)\\s*([0-9a-f][0-9a-f])$",s)
    		assert m is not None,"Bad code "+s
    		mnemonic = m.group(1)
    		byteCount = int(sum(1 if c == 'n' else 0 for c in m.group(2))/2)+1
    		opcode = int(m.group(3),16)
    		#print(mnemonic,opcode,byteCount)
    		assert opcode not in self.toName,"Duplicate opcode "+s
    		self.toName[opcode] = mnemonic
    		if mnemonic not in self.nameBytes:
    			self.nameBytes[mnemonic] = {}
    		self.nameBytes[mnemonic][opcode] = byteCount
    	
    	#print(self.toName)
    	#print(self.nameBytes)
    #
    #		Get opcode indices
    #
    def getOpcodeList(self,mnemonic):
    	return None if mnemonic not in self.nameBytes else self.nameBytes[mnemonic]


    def getSource(self):
        return """
   ADC #$nn            69   
   ADC $nn             65   
   ADC $nn,X           75   
   ADC $nnnn           6D   
   ADC $nnnn,X         7D   
   ADC $nnnn,Y         79   
   ADC ($nn),Y         71   
   ADC ($nn),Z         72   
   ADC ($nn,X)         61   
   AND #$nn            29   
   AND $nn             25   
   AND $nn,X           35   
   AND $nnnn           2D   
   AND $nnnn,X         3D   
   AND $nnnn,Y         39   
   AND ($nn),Y         31   
   AND ($nn),Z         32   
   AND ($nn,X)         21   
   ASL $nn             06   
   ASL $nn,X           16   
   ASL $nnnn           0E   
   ASL $nnnn,X         1E   
   ASL A               0A   
   ASR $nn             44   
   ASR $nn,X           54   
   ASR A               43   
   ASW $nnnn           CB   
   BBR0 $nn,$nn        0F   
   BBR1 $nn,$nn        1F   
   BBR2 $nn,$nn        2F   
   BBR3 $nn,$nn        3F   
   BBR4 $nn,$nn        4F   
   BBR5 $nn,$nn        5F   
   BBR6 $nn,$nn        6F   
   BBR7 $nn,$nn        7F   
   BBS0 $nn,$nn        8F   
   BBS1 $nn,$nn        9F   
   BBS2 $nn,$nn        AF   
   BBS3 $nn,$nn        BF   
   BBS4 $nn,$nn        CF   
   BBS5 $nn,$nn        DF   
   BBS6 $nn,$nn        EF   
   BBS7 $nn,$nn        FF   
   BCC $nn             90   
   BCC $nnnn           93   
   BCS $nn             B0   
   BCS $nnnn           B3   
   BEQ $nn             F0   
   BEQ $nnnn           F3   
   BIT #$nn            89   
   BIT $nn             24   
   BIT $nn,X           34   
   BIT $nnnn           2C   
   BIT $nnnn,X         3C   
   BMI $nn             30   
   BMI $nnnn           33   
   BNE $nn             D0   
   BNE $nnnn           D3   
   BPL $nn             10   
   BPL $nnnn           13   
   BRA $nn             80   
   BRA $nnnn           83   
   BRK                 00   
   BSR $nnnn           63   
   BVC $nn             50   
   BVC $nnnn           53   
   BVS $nn             70   
   BVS $nnnn           73   
   CLC                 18   
   CLD                 D8   
   CLE                 02   
   CLI                 58   
   CLV                 B8   
   CMP #$nn            C9   
   CMP $nn             C5   
   CMP $nn,X           D5   
   CMP $nnnn           CD   
   CMP $nnnn,X         DD   
   CMP $nnnn,Y         D9   
   CMP ($nn),Y         D1   
   CMP ($nn),Z         D2   
   CMP ($nn,X)         C1   
   CPX #$nn            E0   
   CPX $nn             E4   
   CPX $nnnn           EC   
   CPY #$nn            C0   
   CPY $nn             C4   
   CPY $nnnn           CC   
   CPZ #$nn            C2   
   CPZ $nn             D4   
   CPZ $nnnn           DC   
   DEC                 3A   
   DEC $nn             C6   
   DEC $nn,X           D6   
   DEC $nnnn           CE   
   DEC $nnnn,X         DE   
   DEW $nn             C3   
   DEX                 CA   
   DEY                 88   
   DEZ                 3B   
   EOR #$nn            49   
   EOR $nn             45   
   EOR $nn,X           55   
   EOR $nnnn           4D   
   EOR $nnnn,X         5D   
   EOR $nnnn,Y         59   
   EOR ($nn),Y         51   
   EOR ($nn),Z         52   
   EOR ($nn,X)         41   
   INC                 1A   
   INC $nn             E6   
   INC $nn,X           F6   
   INC $nnnn           EE   
   INC $nnnn,X         FE   
   INW $nnnn           E3   
   INX                 E8   
   INY                 C8   
   INZ                 1B   
   JMP $nnnn           4C   
   JMP ($nnnn)         6C   
   JMP ($nnnn,X)       7C   
   JSR $nnnn           20   
   JSR ($nnnn)         22   
   JSR ($nnnn,X)       23   
   LDA #$nn            A9   
   LDA $nn             A5   
   LDA $nn,X           B5   
   LDA $nnnn           AD   
   LDA $nnnn,X         BD   
   LDA $nnnn,Y         B9   
   LDA ($nn),Y         B1   
   LDA ($nn),Z         B2   
   LDA ($nn,SP),Y      E2   
   LDA ($nn,X)         A1   
   LDX #$nn            A2   
   LDX $nn             A6   
   LDX $nn,Y           B6   
   LDX $nnnn           AE   
   LDX $nnnn,Y         BE   
   LDY #$nn            A0   
   LDY $nn             A4   
   LDY $nn,X           B4   
   LDY $nnnn           AC   
   LDY $nnnn,X         BC   
   LDZ #$nn            A3   
   LDZ $nnnn           AB   
   LDZ $nnnn,X         BB   
   LSR $nn             46   
   LSR $nn,X           56   
   LSR $nnnn           4E   
   LSR $nnnn,X         5E   
   LSR A               4A   
   MAP                 5C   
   NEG                 42   
   NOP                 EA   
   ORA #$nn            09   
   ORA $nn             05   
   ORA $nn,X           15   
   ORA $nnnn           0D   
   ORA $nnnn,X         1D   
   ORA $nnnn,Y         19   
   ORA ($nn),Y         11   
   ORA ($nn),Z         12   
   ORA ($nn,X)         01   
   PHA                 48   
   PHP                 08   
   PHW #$nnnn          F4   
   PHW $nnnn           FC   
   PHX                 DA   
   PHY                 5A   
   PHZ                 DB   
   PLA                 68   
   PLP                 28   
   PLX                 FA   
   PLY                 7A   
   PLZ                 FB   
   RMB0 $nn            07   
   RMB1 $nn            17   
   RMB2 $nn            27   
   RMB3 $nn            37   
   RMB4 $nn            47   
   RMB5 $nn            57   
   RMB6 $nn            67   
   RMB7 $nn            77   
   ROL $nn             26   
   ROL $nn,X           36   
   ROL $nnnn           2E   
   ROL $nnnn,X         3E   
   ROL A               2A   
   ROR $nn             66   
   ROR $nn,X           76   
   ROR $nnnn           6E   
   ROR $nnnn,X         7E   
   ROR A               6A   
   ROW $nnnn           EB   
   RTI                 40   
   RTS                 60   
   RTS #$nn            62   
   SBC #$nn            E9   
   SBC $nn             E5   
   SBC $nn,X           F5   
   SBC $nnnn           ED   
   SBC $nnnn,X         FD   
   SBC $nnnn,Y         F9   
   SBC ($nn),Y         F1   
   SBC ($nn),Z         F2   
   SBC ($nn,X)         E1   
   SEC                 38   
   SED                 F8   
   SEE                 03   
   SEI                 78   
   SMB0 $nn            87   
   SMB1 $nn            97   
   SMB2 $nn            A7   
   SMB3 $nn            B7   
   SMB4 $nn            C7   
   SMB5 $nn            D7   
   SMB6 $nn            E7   
   SMB7 $nn            F7   
   STA $nn             85   
   STA $nn,X           95   
   STA $nnnn           8D   
   STA $nnnn,X         9D   
   STA $nnnn,Y         99   
   STA ($nn),Y         91   
   STA ($nn),Z         92   
   STA ($nn,SP),Y      82   
   STA ($nn,X)         81   
   STX $nn             86   
   STX $nn,Y           96   
   STX $nnnn           8E   
   STX $nnnn,Y         9B   
   STY $nn             84   
   STY $nn,X           94   
   STY $nnnn           8C   
   STY $nnnn,X         8B   
   STZ $nn             64   
   STZ $nn,X           74   
   STZ $nnnn           9C   
   STZ $nnnn,X         9E   
   TAB                 5B   
   TAX                 AA   
   TAY                 A8   
   TAZ                 4B   
   TBA                 7B   
   TRB $nn             14   
   TRB $nnnn           1C   
   TSB $nn             04   
   TSB $nnnn           0C   
   TSX                 BA   
   TSY                 0B   
   TXA                 8A   
   TXS                 9A   
   TYA                 98   
   TYS                 2B   
   TZA                 6B   
"""

if __name__ == "__main__":
    c = Commodore65CPU()