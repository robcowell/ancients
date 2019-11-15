	opt	o+,s-

	MOVEA.L	sp,A5		;save stack pointer
	MOVEA.L	4(A5),A5	;basepage address i.i. program start
	MOVE.L	12(A5),D0	;get length of text (the code)
	ADD.L	20(A5),D0	;add length of defined data (DC.W...)
	ADD.L	28(A5),D0	;add length of BSS reserved space (DS.W...)
	ADDI.L	#256,D0		;add basepage length
	MOVE.L	D0,-(sp)	;total memory required
 	MOVE.L	A5,-(sp)	;program start address

	CLR.W	-(sp)		;junk word
	MOVE.W	#$4A,-(sp)	;SETBLOCK command
	TRAP	#1		
	LEA		12(sp),sp

	move.l	#64000+256,-(sp)		;malloc()
	move.w	#72,-(sp)			;screen memory
	trap	#1				;
	addq.l	#6,sp				;
	tst.l	d0				;
	beq.w	pterm			;
	
	add.l	#256,d0				;init screens
	clr.b	d0				;even by 256 bytes
	move.l	d0,screen_adr			;
	add.l	#32000,d0			;
	move.l	d0,screen_adr2			;

	clr.l	-(sp)				;Enter supervisor mode
	move.w	#32,-(sp)			;
	trap	#1				;
	addq.l	#6,sp				;
	move.l	d0,save_stack			;
	
	move.b	#$12,$fffffc02.w		;Kill mouse

* save the old palette; old_palette
	lea	old_palette,a0		;put backup address in a0
	movem.l	$ffff8240,d0-d7		;all palettes in d0-d7
	movem.l	d0-d7,(a0)			;move data into old_palette
* end palette save	

* saves the old screen adress
	move.w	#2,-(sp)			;get physbase
	trap	#14
	addq.l	#2,sp
	move.l	d0,save_screenadr		;save old screen address
* end screen save

* save the old resolution into old_resolution
* and change resolution to low (0)
	move.w	#4,-(sp)			;get resolution
	trap	#14
	addq.l	#2,sp
	move.w	d0,old_resolution	;save resolution
	
	move.w	#0,-(sp)		;low resolution
	move.l	#-1,-(sp)		;keep physbase
	move.l	#-1,-(sp)		;keep logbase
	move.w	#5,-(sp)		;change screen
	trap	#14
	add.l	#12,sp
* end resolution save	

showpic:
	movem.l	picture+2,d0-d7			;skip PI1 header, load palette values into data registers
	movem.l	d0-d7,$ff8240 			; then move those data registers into the palette registers
	lea     picture+34,a0			;load pic data into a0 - 34 bytes to skip header and palette data
	move.l  $44e,a1				;load screen address into a1
	move.l  a1,screen_adr
	move 	#(30554/4),d0 	;counter
	;move 	#(32034/4),d0 	;counter
.copypic
	move.l  (a0)+,(a1)+ 		;copy the picture data in a0 to the screen memory in a1
	dbra    d0,.copypic			;loop

start	
        bsr	play

wait	tst.w	vblcount			;Wait VBL
			
	beq.s	wait			;
	clr.w 	vblcount

	move.l	screen_adr,d0			;swap screens
	move.l	screen_adr2,screen_adr		;doublebuffer
	move.l	d0,screen_adr2			;
	
	bsr scroller
	move.b	$fffc02,d0
	cmpi.b	#$39,d0
	bne.s	wait

	move.w	#3,replay+28

	move.w	#30000,d0
loop	rept	150
	nop
	endr
	dbf	d0,loop

	bsr	stop
	bsr	mouseon
	
	move.l	old_70,$70.w
	move.l	#old_sp,-(sp)
	move.w	#$20,-(sp)
	trap	#1
	addq.l	#6,sp

pterm	clr.l	-(sp)
	trap	#1

; Values for registers:
; ---------------------
;
; D0 = 
;
; $1388 = 5.0Khz
; $2134 = 8.5 khz
; $2ee0 = 12 khz
; $36b0 = 15.0 khz
; $5208 = 21 khz maximum ???
;
; D1 = 
;
; 0 = takes a bit of processor time (all volume changes)
; 2 = takes a little bit less (no volume changes ?)
; 3 = Shit !! Write an intro with this value !!
;
; D2 = 
;
; 0 = takes a bit of processor time (all pitch changes ?)
; 2 = takes a little bit less (no pitch changes ?)


play	bclr	#0,$484.w               ; click off

	move.w	#$36b0,d0		; Khz
	moveq.l	#0,d1			; volume variation ?
	moveq.l	#0,d2			; pitch changes
	lea	music,a0
	jsr	replay
	move.l	#new_70,$70.w
	jsr	replay+4
exitpl	rts

stop	bsr	replay+8
	bset	#0,$484.w
	rts

new_70	addq.w #1,vblcount		;increment vbl counter
	movem.l	a0-a4/d0-d6,-(sp)
	jsr	replay+12
	movem.l	(sp)+,a0-a4/d0-d6
	rte

mouseoff	move.l	#moff,-(a7)
	clr.w	-(a7)
	move.w	#$19,-(a7)
	trap	#14
	addq.l	#8,a7
	dc.w	$a00a
	rts

mouseon	move.l	#mon,-(a7)
	clr.w	-(a7)
	move.w	#$19,-(a7)
	trap	#14
	addq.l	#8,a7
	dc.w	$a009
	rts

moff	dc.w	$1212
mon	dc.w	$0808

loadmod bsr getsize
	move.l	#lz7mod,filebuffer
	move.l	#dta+26,filelength
	bsr	loader
	rts

*** Get file size
getsize:
	pea dta 					;set up dta buffer
	move.w #$1a,-(sp)			;SetDTA function
	trap #1 					;GemDOS call
	addq.l #6,sp 				;tidy stack

	move.w #0,-(sp) 		;attribute value
	move.l	a6,-(sp)			;file to search for
	move.w #$4e,-(sp) 			;fsfirst function
	trap #1 					;GemDOS call
	addq.l #8,sp 				;tidy stack

	tst d0 						;file found?
	bne .notfound

	move.w #$2f,-(sp) 			;GetDTA
	trap #1						;GemDOS call
	addq.l #2,sp 				;tidy stack
	move.l d0,dta 				;get results into our dta buffer
.notfound
	rts
*** End of get file size

*** File loader

* in:   filename.l = address to filename (null term)
*     filebuffer.l = destination address
*     filelength.l = bytes to load

loader:
		clr.w	-(sp)					;Open file read only
		move.l	a6,-(sp)			;Address to filename
		move.w	#$3d,-(sp)				;
		trap	#1						;
		addq.l	#8,sp					;
		move.w	d0,.fn					;Store filenumber
 
		move.l	filebuffer,-(sp)		;Buffer address
		move.l	filelength,-(sp)		;Length of file
		move.w	.fn,-(sp)				;Filenumber
		move.w	#$3f,-(sp)				;
		trap	#1						;
		lea.l	12(sp),sp				;

		move.w	.fn,-(sp)				;Filenumber for closing
		move.w	#$3e,-(sp)				;
		trap	#1						;
		addq.l	#4,sp					;

		rts
.fn:		dc.w	0	

Scroll_ROX:
	lea	Buffer_scroll,a0
 rept	8
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	roxl	-(a0)
	lea	84(a0),a0
 endr
	rts

scroller:
	bsr	Scroll_ROX                   ; Move bloc of screen
	bsr	Scroll_ROX
	addi.b	#1,COUNTER              ; Test counter of text
	cmpi.b	#4,COUNTER
	bne.s	.put_scrolling
	clr.b	COUNTER
	movea.l	TXT_POINTER,a0             ; New character
	moveq	#0,d0                      ; Test the end of the sentence
	move.b	(a0)+,d0
	tst.b	d0
	bpl.s	.not_end_text
	lea	TEXT,a0                     ; Wrap sentence 
	move.b	(a0)+,d0                 ; Next character
.not_end_text:
	move.l	a0,TXT_POINTER             ; Adjust pointer of the text
	lea	FONT8_8,a0                   ; Load font
	adda.w	d0,a0                    ; Seeking the character of the font
	lea	Adr_scroll,a1                ; Assign buffer to put character inside
	move.b	(a0),(a1)
	move.b	256(a0),42(a1)
	move.b	512(a0),84(a1)
	move.b	768(a0),126(a1)
	move.b	1024(a0),168(a1)
	move.b	1280(a0),210(a1)
	move.b	1536(a0),252(a1)
	move.b	1792(a0),294(a1)
.put_scrolling:
	lea	Line_scroll,a0
	movea.l	screen_adr,a1
	add.w	#160*191,a1                ; Add 180 lines to start at the end of the screen
	addq.w	#6,a1                    ; Add plane for the color
	moveq	#7,d0                      ; 7 lines copied
.loop:
a set 0
	rept 20
	move.w	(a0)+,a(a1)
a set a+8
	endr	
	lea	160(a1),a1
	addq.w	#2,a0
	dbf	d0,.loop
	rts


        section	data

** 	filenames - 0 terminated
**  '12345678.123',0,''	; 12 characters per entry
filetab:		dc.b 	'bignum.lz7',0,'  '
			dc.b 	'chipsupp.lz7',0,''
			dc.b 	'dah.lz7',0,'     '
			dc.b 	'dboned.lz7',0,'  '
			dc.b 	'dro.lz7',0,'     '
			dc.b 	'epiclove.lz7',0,''
			dc.b 	'flib.lz7',0,'    '
			dc.b 	'folk.lz7',0,'    '
			dc.b 	'hippos.lz7',0,'  '
			dc.b 	'hopeflea.lz7',0,''
			dc.b 	'hppytime.lz7',0,''
			dc.b 	'hppyvibe.lz7',0,''
			dc.b 	'hvywater.lz7',0,''
			dc.b 	'levity.lz7',0,'  '
			dc.b 	'lifewarm.lz7',0,''
			dc.b 	'moreflea.lz7',0,''
			dc.b 	'ratsbats.lz7',0,''
			dc.b 	'roofduct.lz7',0,''
			dc.b 	'rounderb.lz7',0,''
			dc.b 	'schizo.lz7',0,'  '
			dc.b 	'turnips.lz7',0,' '
			dc.b 	'vak8ed.lz7',0,'  '
			dc.b 	'weeeeeee.lz7',0,''
			dc.b 	'woohoo.lz7',0,'  '
			dc.b 	0
			even
*** end of filenames

;music	incbin	dah.mod,0
m_end	even

picture	incbin	ancients.pi1
replay	incbin	ninja342.bin
FONT8_8	incbin	FONT8_8.DAT
	even
COUNTER:	dc.w	$0
	even
TXT_POINTER:	dc.l	TEXT

TEXT:
	dc.b 	"RiFT are proud to present      "
	dc.b 	"TECHNOLOGIES OF THE ANCIENTS     "
	dc.b 	"A little music disk showcasing the talents of our musician, Bextula of RiFT    "
	dc.b 	"Over the years, our Bex has gone by a number of handles - Bassline, Bex, Sick Man, Meek and more -  "
	dc.b 	"but one thing that has remained the same is the quality of the MODs produced     "
	dc.b 	"                                           " 		;bit of empty space before we wrap - LEAVE IT
	dc.b	$FF,$0												; end of text marker - LEAVE IT
	
	even

        section bss
        
        ds.b	256
screen	ds.b	160*288
save_screenadr 	ds.l 	1
screen_adr:	ds.l 	1
screen_adr2:	ds.l 	1
vblcount: 	ds.w	1
dta:	ds.b    44							;dta block about file info
old_resolution:	ds.w 	1
save_stack	ds.l  	1
old_palette	ds.l	8
lz7mod		ds.b	57000
music		ds.b    57000
Line_scroll:	ds.b	20*2+1
Adr_scroll:	ds.b	1
Buffer_scroll:	ds.b	21*8*20
old_sp	ds.l	1
old_70	ds.l	1
old_a07	ds.b	1
old_a13	ds.b	1
old_a15	ds.b	1
	even

