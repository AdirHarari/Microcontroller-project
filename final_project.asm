
LIST 	P=PIC16F877
include 	<P16f877.inc>
org		0x00

reset:
	goto start

	org		0x10
start:	
	bcf			STATUS, RP0
	bcf			STATUS, RP1	;bank 0
	clrf		PORTD		;clear old data from PORTD
	clrf		PORTE		;clear old data from PORTE

	bsf			STATUS, RP0	;bank 1
	bcf			INTCON,GIE			;No interrupt
	movlw		0x06	         	;set data as digital
	movwf		ADCON1
		
	clrf		TRISD	;set PORTD as output
	clrf		TRISE	;set PORTE as output

	movlw		0x0F
	movwf		TRISB
	bcf			OPTION_REG,0x7		;Enable PortB Pull-Up
	bcf			STATUS,RP0			;Bank0
	
 	call		init	;initialized the LCD	
					
	clrf	0x21	;register for A
	clrf	0x30	;register for B
	clrf	0x40	;register for C
	clrf	0x60	;result register 1
	clrf	0x61	;result register 2
		
	goto	wkb		;call keyboard = start action 
		
;------------------display input---------------------------------	
A_disp:
	
	movlw	0x80	;THE FIRST PLACE TO PUT A3
	movwf	0x50
	
;******************************"A=" display***************************
;A
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x41			;A CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;=
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x3D			;= CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;******************************A3***************************
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	A_KB
	btfss	0x21,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x21,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;******************************A2***************************
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	A_KB
	btfss	0x21,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x21,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;******************************A1***************************	
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	A_KB
	btfss	0x21,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x21,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;******************************A0**************************
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	A_KB
	btfss	0x21,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x21,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	
	goto	wkb				;back to main keyboard
	
B_disp:

	movlw	0x87	;THE FIRST PLACE TO PUT B3
	movwf	0x51
	
;******************************"B=" display***************************
;B
	movf	0x51,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x42			;B CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x51,1
;=
	movf	0x51,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x3D			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x51,1
;******************************B3***************************
	movf	0x51,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	B_KB
	btfss	0x30,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x30,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x51,1
;******************************B2***************************
	movf	0x51,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	B_KB
	btfss	0x30,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x30,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x51,1
;******************************B1****************************	
	movf	0x51,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	B_KB
	btfss	0x30,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x30,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x51,1
;******************************B0***************************
	movf	0x51,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	B_KB
	btfss	0x30,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x30,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel

	goto	wkb				;back to main keyboard
	
C_disp:

	movlw	0xC0	;THE FIRST PLACE TO PUT C3
	movwf	0x52
	
;******************************"C=" display***************************
;C
	movf	0x52,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x43			;B CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x52,1
;=
	movf	0x52,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x3D			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x52,1	
;******************************C3***************************
	movf	0x52,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel
	
	call 	C_KB
	btfss	0x40,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x40,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x52,1
;******************************C2***************************
	movf	0x52,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	C_KB
	btfss	0x40,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x40,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x52,1
;******************************C1***************************
	movf	0x52,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	C_KB
	btfss	0x40,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x40,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x52,1
;******************************C0***************************
	movf	0x52,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	call 	C_KB
	btfss	0x40,0			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x40,0			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
;-----------------------------------------------------------------
	movlw	0x01			;CLR THE LCD for result
	movwf	0x20
	call 	lcdc
	call	mdel
	goto	CHECK_C
;******************************clr*******************************
clr:
	call 	delay
	movlw	0x01			;CLR THE LCD
	movwf	0x20
	call 	lcdc
	call	mdel
	goto	start
;------------------check command ALU---------------------------------
CHECK_C:
	;movlw	0x0A	
	;movwf	0x40
	btfss	0x40,3		;check if bit 3 is 1/0 (xxxx ?xxx)
	goto	CH_0xxx		;go check bit 2 (xxxx 0?xx)
	goto	CH_1xxx		;go check bit 2 (xxxx 1?xx)

CH_0xxx:
	btfss	0x40,2		;check if bit 2 is 1/0 (xxxx 0?xx)
	goto	CH_00xx		;go check bit 1 (xxxx 00?x)
	goto	CH_01xx		;go check bit 1 (xxxx 01?x)

CH_00xx:
	btfss	0x40,1		;check if bit 1 is 1/0 (xxxx 00?x)
	goto	CH_000x		;go check bit 0 (xxxx 000?)
	goto	CH_001x		;go check bit 0 (xxxx 001?)

CH_000x:
	goto 	error_disp	;no such commands (0000/0001)
	
CH_001x:
	btfss	0x40,0		;check if bit 0 is 1/0 (xxxx 001?)
	goto	COM_0010	;go to comand 0010 (A-B)
	goto	COM_0011	;go to comand 0011 (A-B)^B

CH_01xx:
	btfss	0x40,1		;check if bit 1 is 1/0 (xxxx 01?x)
	goto	CH_010x		;go check bit 0 (xxxx 010?)
	goto	CH_011x		;go check bit 0 (xxxx 011?)

CH_010x:
	btfss	0x40,0		;check if bit 0 is 1/0 (xxxx 010?)
	goto	COM_0100	;go to comand 0100 (A*B)
	goto	error_disp	;no such command (0101)

CH_011x:
	btfss	0x40,0		;check if bit 0 is 1/0 (xxxx 011?)
	goto	COM_0110	;go to comand 0110 (A/B)
	goto	COM_0111	;display she'erit (A/B)	
	
CH_1xxx:
	btfss	0x40,2		;check if bit 2 is 1/0 (xxxx 1?xx)
	goto	CH_10xx		;check bit 1 (xxxx 10?x)
	goto	CH_11xx		;check bit 1 (xxxx 11?x)	

CH_10xx:
	btfss	0x40,1		;check if bit 1 is 1/0 (xxxx 10?x)
	goto	CH_100x		;go check bit 0 (xxxx 100?)
	goto	CH_101x		;go check bit 0 (xxxx 101?)

CH_100x:
	btfss	0x40,0		;check if bit 0 is 1/0 (xxxx 100?)
	goto	COM_1000	;go to comand 1000 (A^B)
	goto	error_disp	;no such command (1001)
	
CH_101x:
	btfss	0x40,0		;check if bit 0 is 1/0 (xxxx 101?)
	goto	COM_1010	;go to comand 1010 (the sum of '1' in A)
	goto	COM_1011	;go to comand 1011 (the sum of double '1' in B)

CH_11xx:
	btfss	0x40,1		;check if bit 1 is 1/0 (xxxx 11?x)
	goto	CH_110x		;go check bit 0 (xxxx 110?)
	goto	CH_111x		;go check bit 0 (xxxx 111?)

CH_110x:
	btfss	0x40,0		;check if bit 0 is 1/0 (xxxx 110?)
	goto	COM_11OO	;go to comand 1100 (the sum of '0' in B)
	goto	error_disp	;no such command (1101)

CH_111x:
	goto 	error_disp	;no such commands (1110/1111)
	
	
;------------------COMMANDS-OPRATION-----------------------------
;****************************************************************
COM_0010:				;A-B function (A>Ox21, B>0x30)
	
	movf	0x30,w		;B TO W
	subwf	0x21,w		;sub A-B
	movwf	0x60		;result
	btfss 	STATUS,C	;check if answer in negative
	goto 	negative
	goto	display_result
	
negative:
	movf	0x21,w		;A TO W
	subwf	0x30,w		;sub B-A
	movwf	0x60
;display (-)
	movlw 	0xC0		;first place in second line
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x2D			;CHAR (-) (the data )
	movwf	0x20
	call 	lcdd
	call	mdel

	goto	display_result

;****************************************************************
COM_0011:				;(A-B)^B function (A>Ox21, B>0x30)
	
		movf	0x30,w			;B TO W
		subwf	0x21,w			;sub A-B
		MOVWF	0x35			;help register
		MOVWF	0x61			;MulL recieves A-B
		decf	0x30,w
		movwf	0x26			;power (B-1)	
		CLRF 	0x60			;MulH clear
loop44:		
		movLW 	0x08 
		MOVWF 	0x25 			;counter set to 8
		
loop33:	BCF 	0X03,0 			;clear carry bit
		MOVF 	0X35,0 			;enter X to W
		BTFSC 	0X61,0 			;if MulL<0> = 0 skip 
		ADDWF 	0X60,1 			;MulH+X=MulH
		RRF 	0X60,1 			;rotate right MulH
		RRF 	0X61,1 			;rotate right MulL
		DECFSZ 	0X25,1 			;run loop 8 times
		goto 	loop33
		decfsz	0x26,1			;run loop B-1 times
		goto	loop44
		movf	0x61,w
		movwf	0x60			;move 0x61 to 0x60

	
	goto	display_result
	
;****************************************************************	
COM_0100:				;A*B function (A>Ox21, B>0x30)
	
		MOVF 	0x30,0			;move B to w
		MOVWF	0x61			;MulL recieves B
		CLRF 	0x60			;MulH clear
		movLW 	0x08 
		MOVWF 	0x25 			;counter set to 8
		
loop:	BCF 	0X03,0 			;clear carry bit
		MOVF 	0X21,0 			;enter X to W
		BTFSC 	0X61,0 			;if MulL<0> = 0 skip 
		ADDWF 	0X60,1 			;MulH+X=MulH
		RRF 	0X60,1 			;rotate right MulH
		RRF 	0X61,1 			;rotate right MulL
		DECFSZ 	0X25,1 			;run loop 8 times
		goto 	loop
		movf	0x61,w
		movwf	0x60			;move 0x61 to 0x60
		goto	display_result	


;****************************************************************
COM_0110:						;A/B function

		CLRF 	0x61 			;M (she'erit)
		movf 	0x21,0 			;move A to W
		movwf 	0x60 			;Result (set as the numerator)  
		movLW 	0x08 
		MOVWF 	0x25			;counter set to 8
		
loop2:	BCF 	0X03,0 			;clear carry bit
		RLF 	0x60,1 			;rotate left R
		RLF 	0x61,1 			;rotate left M 
		MOVF 	0x30,0 			;enter B to W
		SUBWF 	0X61,0 			;M-B to W
		BTFSS 	STATUS,C 		;skip if M-B is negtive(C=0)
		goto 	E_O_L 			;skip next 3 steps and go to end of loop
		MOVF 	0X30,0 			;enter B to W	
		SUBWF 	0X61,1 			;M-B to M 
		BSF 	0X60,0 			;set 1 to A<0>	
E_O_L:	DECFSZ 	0X25,1 			;run loop 8 times
		goto 	loop2
		goto	display_result
		
;****************************************************************
COM_0111:						;A/B function display she'erit

		CLRF 	0x61 			;M (she'erit)
		movf 	0x21,0 			;move A to W
		movwf 	0x60 			;Result (set as the numerator)  
		movLW 	0x08 
		MOVWF 	0x25			;counter set to 8
		
loop22:	BCF 	0X03,0 			;clear carry bit
		RLF 	0x60,1 			;rotate left R
		RLF 	0x61,1 			;rotate left M 
		MOVF 	0x30,0 			;enter B to W
		SUBWF 	0X61,0 			;M-B to W
		BTFSS 	STATUS,C 		;skip if M-B is negtive(C=0)
		goto 	E_O_L2 			;skip next 3 steps and go to end of loop
		MOVF 	0X30,0 			;enter B to W	
		SUBWF 	0X61,1 			;M-B to M 
		BSF 	0X60,0 			;set 1 to A<0>	
E_O_L2:	DECFSZ 	0X25,1 			;run loop 8 times
		goto 	loop22
		movf	0x61,w
		movwf	0x60
		goto	display_result		
;****************************************************************
COM_1000:				;A^B function (A*A, B-1 times)
		
		MOVF 	0x21,0			
		MOVWF	0x61			;MulL recieves A
		movf	0x30,w
		movwf	0x26			;power (B)
		decf	0x26,f			;run B-1 times	
		CLRF 	0x60			;MulH clear
loop4:		
		movLW 	0x08 
		MOVWF 	0x25 			;counter set to 8
		
loop3:	BCF 	0X03,0 			;clear carry bit
		MOVF 	0X21,0 			;enter X to W
		BTFSC 	0X61,0 			;if MulL<0> = 0 skip 
		ADDWF 	0X60,1 			;MulH+X=MulH
		RRF 	0X60,1 			;rotate right MulH
		RRF 	0X61,1 			;rotate right MulL
		DECFSZ 	0X25,1 			;run loop 8 times
		goto 	loop3
		decfsz	0x26,1			;run loop B-1 times
		goto	loop4
		movf	0x61,w
		movwf	0x60			;move 0x61 to 0x60
		goto 	display_result
;****************************************************************
COM_1010:				;sum of '1' in A function
		
		clrf	0x60	;counter set to 0 (the result)
		movlw 	0x04
		movwf 	0x25 	;counter set to 4
loop5:		
		btfsc	0x21,3	;check bit 3 at A
		incf	0x60,f	;+1 if bit is '1'
		rlf		0x21,f	;move to next bit
		decfsz	0x25,f
		goto 	loop5
		goto	display_result
		
		
;****************************************************************
COM_11OO:				;sum of '0' in B function

		clrf	0x60	;counter set to 0 (the result)
		movlw 	0x04
		movwf 	0x25 	;counter set to 4
loop6:		
		btfss	0x30,3	;check bit 3 at B
		incf	0x60,f	;+1 if bit is '0'
		rlf		0x30,f	;move to next bit
		decfsz	0x25,f
		goto 	loop6
		goto	display_result
;****************************************************************
COM_1011:				;sum of double '1' in B function

		clrf	0x60	;counter set to 0 (the result)
		movlw 	0x04
		movwf 	0x25 	;counter set to 4
loop7:		
		btfss	0x30,3	;check bit 3 at B
		goto	rotate
		btfss	0x30,2	;check bit 2 at B
		goto	rotate
		incf	0x60,f	;+1 if bits is '11'
rotate:		
		rlf		0x30,f	;move to next bit
		decfsz	0x25,f
		goto 	loop7
		goto	display_result

;------------------display result---------------------------
display_result:
	

	
	movlw	0x80	
	movwf	0x50
;first letter R
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x52			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;second letter E
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x45			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;third letter S
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x53			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;fourth letter U	
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x55			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;fifth letter L
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x4C			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;sixth letter T
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x54			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel	
	incf	0x50,1
;last char ":"	
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x3A			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel		
;***********************************************************************	
	movlw 	0xC1
	movwf 	0x53
	
	movlw 	0x08
	movwf 	0x25 			;counter set to 8 (8 bits)
loop8:
	movf	0x53,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	btfss	0x60,7			;check if KB data is 0/1
	movlw	0x30			;if 0 
	btfsc	0x60,7			;skip next if bit is 0
	movlw	0x31			;if 1
	
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x53,f
	rlf		0x60,f
	decfsz	0x25,f
	goto	loop8
	call 	delay
	goto	finish
	
;------------------error_display---------------------------
error_disp:
	movlw	0x01			;CLR THE LCD
	movwf	0x20
	call 	lcdc
	call	mdel
	
	movlw	0x80	
	movwf	0x50
;first letter E
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x45			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;second letter R
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x52			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;third letter R
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x52			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;fourth letter O	
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x4F			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	incf	0x50,1
;fifth letter R
	movf	0x50,w			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x52			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel

	goto	clr
	
;****************main KB**************************
wkb:	bcf			PORTB,0x4			;scan Row 1
		bsf			PORTB,0x5
		bsf			PORTB,0x6
		bsf			PORTB,0x7
		btfss		PORTB,0x0
		goto		kb01
		btfss		PORTB,0x1
		goto		kb02
		btfss		PORTB,0x2
		goto		kb03
		btfss		PORTB,0x3
		goto		kb0a

		bsf			PORTB,0x4
		bcf			PORTB,0x5			;scan Row 2
		btfss		PORTB,0x0
		goto		kb04
		btfss		PORTB,0x1
		goto		kb05
		btfss		PORTB,0x2
		goto		kb06
		btfss		PORTB,0x3
		goto		kb0b

		bsf			PORTB,0x5
		bcf			PORTB,0x6			;scan Row 3
		btfss		PORTB,0x0
		goto		kb07
		btfss		PORTB,0x1
		goto		kb08
		btfss		PORTB,0x2
		goto		kb09
		btfss		PORTB,0x3
		goto		kb0c

		bsf			PORTB,0x6
		bcf			PORTB,0x7			;scan Row 4
		btfss		PORTB,0x0
		goto		kb0e
		btfss		PORTB,0x1
		goto		kb00
		btfss		PORTB,0x2
		goto		kb0f
		btfss		PORTB,0x3
		goto		kb0d

		goto		wkb

kb00:	call		wait_till_release
		goto		error_disp
kb01:	call		wait_till_release
		goto		error_disp
kb02:	call		wait_till_release
		goto		error_disp
kb03:	call		wait_till_release
		goto		error_disp
kb04:	call		wait_till_release		
		goto		error_disp
kb05:	call		wait_till_release
		goto		error_disp
kb06:	call		wait_till_release
		goto		error_disp
kb07:	call		wait_till_release
		goto		error_disp
kb08:	call		wait_till_release
		goto		error_disp
kb09:	call		wait_till_release
		goto		error_disp
;-------------------------------------		
kb0a:	call		wait_till_release
		goto		A_disp
kb0b:	call		wait_till_release
		goto		B_disp
kb0c:	call		wait_till_release
		goto		C_disp
;-------------------------------------		
kb0d:	call		wait_till_release
		goto		error_disp
kb0e:	call		wait_till_release
		goto		error_disp
kb0f:	call		wait_till_release
		goto		error_disp

wait_till_release:
		movlw		0x0F
		movwf		PORTB
		subwf		PORTB, w
		btfss		STATUS, Z
		goto		wait_till_release
		return

;****************A_kb**************************
A_KB:	bcf			PORTB,0x4			;scan Row 1
		bsf			PORTB,0x5
		bsf			PORTB,0x6
		bsf			PORTB,0x7
		btfss		PORTB,0x0
		goto		Akb01
		btfss		PORTB,0x1
		goto		Akb02
		btfss		PORTB,0x2
		goto		Akb03
		btfss		PORTB,0x3
		goto		Akb0a

		bsf			PORTB,0x4
		bcf			PORTB,0x5			;scan Row 2
		btfss		PORTB,0x0
		goto		Akb04
		btfss		PORTB,0x1
		goto		Akb05
		btfss		PORTB,0x2
		goto		Akb06
		btfss		PORTB,0x3
		goto		Akb0b

		bsf			PORTB,0x5
		bcf			PORTB,0x6			;scan Row 3
		btfss		PORTB,0x0
		goto		Akb07
		btfss		PORTB,0x1
		goto		Akb08
		btfss		PORTB,0x2
		goto		Akb09
		btfss		PORTB,0x3
		goto		Akb0c

		bsf			PORTB,0x6
		bcf			PORTB,0x7			;scan Row 4
		btfss		PORTB,0x0
		goto		Akb0e
		btfss		PORTB,0x1
		goto		Akb00
		btfss		PORTB,0x2
		goto		Akb0f
		btfss		PORTB,0x3
		goto		Akb0d

		goto		A_KB
;-------------------------------------
Akb00:	call		wait_till_release
		rlf			0x21,1
		bcf			0x21,0
		return
Akb01:	call		wait_till_release
		rlf			0x21,1
		bsf			0x21,0
		return
;-------------------------------------
Akb02:	call		wait_till_release
		goto		error_disp
Akb03:	call		wait_till_release
		goto		error_disp
Akb04:	call		wait_till_release		
		goto		error_disp
Akb05:	call		wait_till_release
		goto		error_disp
Akb06:	call		wait_till_release
		goto		error_disp
Akb07:	call		wait_till_release
		goto		error_disp
Akb08:	call		wait_till_release
		goto		error_disp
Akb09:	call		wait_till_release
		goto		error_disp		
Akb0a:	call		wait_till_release
		goto		error_disp
Akb0b:	call		wait_till_release
		goto		error_disp
Akb0c:	call		wait_till_release
		goto		error_disp		
Akb0d:	call		wait_till_release
		goto		error_disp
Akb0e:	call		wait_till_release
		goto		error_disp
Akb0f:	call		wait_till_release
		goto		error_disp

;****************B_kb**************************
B_KB:	bcf			PORTB,0x4			;scan Row 1
		bsf			PORTB,0x5
		bsf			PORTB,0x6
		bsf			PORTB,0x7
		btfss		PORTB,0x0
		goto		Bkb01
		btfss		PORTB,0x1
		goto		Bkb02
		btfss		PORTB,0x2
		goto		Bkb03
		btfss		PORTB,0x3
		goto		Bkb0a

		bsf			PORTB,0x4
		bcf			PORTB,0x5			;scan Row 2
		btfss		PORTB,0x0
		goto		Bkb04
		btfss		PORTB,0x1
		goto		Bkb05
		btfss		PORTB,0x2
		goto		Bkb06
		btfss		PORTB,0x3
		goto		Bkb0b

		bsf			PORTB,0x5
		bcf			PORTB,0x6			;scan Row 3
		btfss		PORTB,0x0
		goto		Bkb07
		btfss		PORTB,0x1
		goto		Bkb08
		btfss		PORTB,0x2
		goto		Bkb09
		btfss		PORTB,0x3
		goto		Bkb0c

		bsf			PORTB,0x6
		bcf			PORTB,0x7			;scan Row 4
		btfss		PORTB,0x0
		goto		Bkb0e
		btfss		PORTB,0x1
		goto		Bkb00
		btfss		PORTB,0x2
		goto		Bkb0f
		btfss		PORTB,0x3
		goto		Bkb0d

		goto		B_KB

;-------------------------------------
Bkb00:	call		wait_till_release
		rlf			0x30,1
		bcf			0x30,0
		return
Bkb01:	call		wait_till_release
		rlf			0x30,1
		bsf			0x30,0
		return
;-------------------------------------
Bkb02:	call		wait_till_release
		goto		error_disp
Bkb03:	call		wait_till_release
		goto		error_disp
Bkb04:	call		wait_till_release		
		goto		error_disp
Bkb05:	call		wait_till_release
		goto		error_disp
Bkb06:	call		wait_till_release
		goto		error_disp
Bkb07:	call		wait_till_release
		goto		error_disp
Bkb08:	call		wait_till_release
		goto		error_disp
Bkb09:	call		wait_till_release
		goto		error_disp		
Bkb0a:	call		wait_till_release
		goto		error_disp
Bkb0b:	call		wait_till_release
		goto		error_disp
Bkb0c:	call		wait_till_release
		goto		error_disp		
Bkb0d:	call		wait_till_release
		goto		error_disp
Bkb0e:	call		wait_till_release
		goto		error_disp
Bkb0f:	call		wait_till_release
		goto		error_disp

;****************C_kb**************************
C_KB:	bcf			PORTB,0x4			;scan Row 1
		bsf			PORTB,0x5
		bsf			PORTB,0x6
		bsf			PORTB,0x7
		btfss		PORTB,0x0
		goto		Ckb01
		btfss		PORTB,0x1
		goto		Ckb02
		btfss		PORTB,0x2
		goto		Ckb03
		btfss		PORTB,0x3
		goto		Ckb0a

		bsf			PORTB,0x4
		bcf			PORTB,0x5			;scan Row 2
		btfss		PORTB,0x0
		goto		Ckb04
		btfss		PORTB,0x1
		goto		Ckb05
		btfss		PORTB,0x2
		goto		Ckb06
		btfss		PORTB,0x3
		goto		Ckb0b

		bsf			PORTB,0x5
		bcf			PORTB,0x6			;scan Row 3
		btfss		PORTB,0x0
		goto		Ckb07
		btfss		PORTB,0x1
		goto		Ckb08
		btfss		PORTB,0x2
		goto		Ckb09
		btfss		PORTB,0x3
		goto		Ckb0c

		bsf			PORTB,0x6
		bcf			PORTB,0x7			;scan Row 4
		btfss		PORTB,0x0
		goto		Ckb0e
		btfss		PORTB,0x1
		goto		Ckb00
		btfss		PORTB,0x2
		goto		Ckb0f
		btfss		PORTB,0x3
		goto		Ckb0d

		goto		C_KB

;-------------------------------------
Ckb00:	call		wait_till_release
		rlf			0x40,1
		bcf			0x40,0
		return
Ckb01:	call		wait_till_release
		rlf			0x40,1
		bsf			0x40,0
		return
;-------------------------------------
Ckb02:	call		wait_till_release
		goto		error_disp
Ckb03:	call		wait_till_release
		goto		error_disp
Ckb04:	call		wait_till_release		
		goto		error_disp
Ckb05:	call		wait_till_release
		goto		error_disp
Ckb06:	call		wait_till_release
		goto		error_disp
Ckb07:	call		wait_till_release
		goto		error_disp
Ckb08:	call		wait_till_release
		goto		error_disp
Ckb09:	call		wait_till_release
		goto		error_disp		
Ckb0a:	call		wait_till_release
		goto		error_disp
Ckb0b:	call		wait_till_release
		goto		error_disp
Ckb0c:	call		wait_till_release
		goto		error_disp		
Ckb0d:	call		wait_till_release
		goto		error_disp
Ckb0e:	call		wait_till_release
		goto		error_disp
Ckb0f:	call		wait_till_release
		goto		error_disp

;*****************************
;subroutine to initialize LCD*
;*****************************
init:	bcf	STATUS,RP0

	movlw	0x30
	movwf	0x20
	call 	lcdc
	call	del_41

	movlw	0x30
	movwf	0x20
	call 	lcdc
	call	del_01

	movlw	0x30
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x01		; display clear
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x06		; I/D=1,S=0 :increment,no  shift 000001 I/D S
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x0e		; D=1,C=B=0 :set display ,no cursor, no blinking
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x38		; dl=1 ( 8 bits interface,n=1:2 lines,f=0:5x7 dots)
	movwf	0x20
	call 	lcdc
	call	mdel
	return
;***********************************
;subroutine to write command to LCD*
;***********************************

lcdc:	bcf		STATUS, RP0        
		movlw		0x00		; E=0,RS=0 
		movwf		PORTE
		movf		0x20,w
		addlw		0x71
		btfsc		STATUS,0x01
		goto		one
		goto		two
one:	
		movlw		0xc0
		goto		three
two:	
		movf		0x20,w
		movwf		PORTD
three:	
		movlw		0x01		; E=1,RS=0
		movwf		PORTE

        call		sdel

		movlw		0x00		; E=0,RS=0
		movwf		PORTE
return

;***********************************
;subroutine to write data to LCD*
;***********************************

lcdd:	bcf		STATUS, RP0        
		movlw		0x02		; E=0, RS=1
		movwf		PORTE
		movf		0x20,w
		movwf		PORTD
        movlw		0x03		; E=1, rs=1  
		movwf		PORTE
		call		sdel
		movlw		0x02		; E=0, rs=1  
		movwf		PORTE
return

del_41:	movlw		0xcd
		movwf		0x23
lulaa6:	movlw		0x20
		movwf		0x22
lulaa7:	decfsz		0x22,1
		goto		lulaa7
		decfsz		0x23,1
		goto 		lulaa6 
		return
	
del_01:	movlw		0x20
		movwf		0x22
lulaa8:	decfsz		0x22,1
		goto		lulaa8
		return
;************************************************
sdel:	movlw		0xff		; movlw = 1 cycle
		movwf		0x23		; movwf	= 1 cycle
lulaa2:	movlw		0xfa
		movwf		0x22
lulaa1:	decfsz		0x22,1		; decfsz= 1/2 cycle
		goto		lulaa1		; goto	= 2 cycles
		decfsz		0x23,1
		goto 		lulaa2 
		return
;************************************************
mdel:	movlw		0x0a
		movwf		0x24
lulaa5:	movlw		0x19
		movwf		0x23
lulaa4:	movlw		0xfa
		movwf		0x22
lulaa3:	decfsz		0x22,1
		goto		lulaa3
		decfsz		0x23,1
		goto 		lulaa4 
		decfsz		0x24,1
		goto		lulaa5
		return

;*******************************************
delay:						
	movlw		0xFF    
	movwf		0x31	;N1
count3:
	movlw		0xFF	;N2 
	movwf		0x32
count2:
	movlw		0xF0	;N3
	movwf		0x33
count1:
	decfsz		0x33
	GOTO 		count1
	decfsz		0x32
	GOTO 		count2
	decfsz		0x31
	GOTO 		count3	
	return

finish:
end