
	section text

	jsr initialise
	jsr init

	bclr #0,$484

	lea filetab,a6
	bsr loadmod
	bsr lzdepack

	move.l	#screen+255,d0	; add 255 to round UP not DOWN!!
	clr.b	d0		; round to 256 bytes
	move.l	d0,screen_adr
	move.l	d0,screen_adr2

	movem.l	d0-d7/a0-a6,-(sp)	;backup registers
	jsr	music_lance_pt50_init
	movem.l	(sp)+,d0-d7/a0-a6	;restore registers

	jsr depackpic
	jsr piccy

	move.l	#vbl,$70

mainloop:
		tst.w	vblcount			;Wait VBL
		beq.s	mainloop			;
		clr.w 	vblcount

;		move.l	screen_adr,d0			;swap screens
;		move.l	screen_adr2,screen_adr	;doublebuffer
;		move.l	d0,screen_adr2			;

		cmp.b 	#$39,$fffffc02.w    ; SPACE for next mod
		beq	nextmod

		cmp.b	#$01,$fffffc02.w 	;Escape?
		beq	exit			;no, keep looping

		bra mainloop


*** Cleanup

exit:
	move.w	#$2700,sr		;ints off
	move.l	#backup,a0
	move.l	(a0)+,$70		;restore vector $70 (vbl)
	move.w	#$2300,sr		;ints on

	movem.l	d0-d7/a0-a6,-(sp)	;backup registers
	jsr music_deinit
	movem.l	(sp)+,d0-d7/a0-a6	;restore registers

	move.l	#backup,a0
	move.l	(a0)+,$70			;restore vector $70 (vbl)
	move.l	(a0)+,$120			;restore vector $120 (timer b)
	move.b	(a0)+,$fffa07		;restore enable a
	move.b	(a0)+,$fffa13		;restore mask a
	move.b	(a0)+,$fffa15		;restore mask b
	move.b	(a0)+,$fffa1b		;restore timer b control
	move.b	(a0)+,$fffa21		;restore timer b data

	jsr restore

pterm	clr.w -(sp)			;exit
	trap #1


init
	move.l	#backup,a0
	move.l	$70,(a0)+			;backup vector $70 (VBL)
	move.l	$120,(a0)+			;backup vector $120 (timer b)
	move.b	$fffa07,(a0)+		;backup enable a
	move.b	$fffa13,(a0)+		;backup mask a
	move.b	$fffa15,(a0)+		;backup mask b
	move.b	$fffa1b,(a0)+		;backup timer b control
	move.b	$fffa21,(a0)+		;backup timer b data
	rts


piccy	movem.l	picture+2,d0-d7
	movem.l	d0-d7,$ff8240

	move.l 	screen_adr,d0	;get screen address
	move.l	d0,a0			;copy to a0
	clr.b	$ff820d			;clear vid address low byte (ste)
	lsr.l	#8,d0
	move.b	d0,$ff8203		;set vid address mid byte
	lsr.w	#8,d0
	move.b	d0,$ff8201		;set vid address high byte

	move.l	#picture+34,a1	;skip header and palette data
	move.l	#(32000/4)-1,d0

.loop1	move.l	(a1)+,(a0)+		;copy pic to screen
	dbf	d0,.loop1
	move.l	#((160*10)/4)-1,d0

.loop2	move.l	#0,(a0)+
	dbf	d0,.loop2
	move.l	#picture+34,a1
	move.l	#((160*78)/4)-1,d0
	rts

*** VBL Routine ***
vbl
	addq.w #1,vblcount

	movem.l	d0-d7/a0-a6,-(sp)	;backup registers
	bsr	scroller
	jsr	music_play
	movem.l	(sp)+,d0-d7/a0-a6	;restore registers

	rte


music_init:
	movem.l	d0-d7/a0-a6,-(sp)	;backup registers
	jsr	music_lance_mod_init
	movem.l	(sp)+,d0-d7/a0-a6	;restore registers
	rts

music_deinit:
	movem.l	d0-d7/a0-a6,-(sp)	;backup registers
	jsr	music_lance_pt50_exit
	movem.l	(sp)+,d0-d7/a0-a6	;restore registers
	rts

music_play:
	movem.l	d0-d7/a0-a6,-(sp)	;backup registers
	jsr	music_lance_pt50_play
	movem.l	(sp)+,d0-d7/a0-a6	;restore registers
	rts


*** Next MOD
nextmod:
	jsr	music_lance_pt50_stop

	lea 13(a6),a6	; move on 13 characters, so one filename
	tst.b (a6)		; is it a zero?
	bne.s .tryload 	; no, so try loading the filename
	lea filetab,a6  ; otherwise loop back to first file in table
	bra .tryload

.tryload
	movem.l	d0-d7/a0-a6,-(sp)	;backup registers
	bsr loadmod
	bsr lzdepack
	movem.l	(sp)+,d0-d7/a0-a6	;restore registers

	bsr music_init

	bra mainloop
*** End  next



loadmod bsr getsize
	move.l	#lz7mod,d0
	move.l	dta+26,d1
	sub.l	d1,d0
	and.w	#$fffc,d0

	move.l	d0,filebuffer
	move.l	d1,filelength
	bsr	loader
	rts

*** Get file size
getsize:
	pea dta						;set up dta buffer
	move.w #$1a,-(sp)			;SetDTA function
	trap #1						;GemDOS call
	addq.l #6,sp				;tidy stack

	move.w #0,-(sp) 			;attribute value
	move.l	a6,-(sp)			;file to search for
	move.w #$4e,-(sp)			;fsfirst function
	trap #1						;GemDOS call
	addq.l #8,sp 				;tidy stack

	tst d0						;file found?
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
		move.l	a6,-(sp)				;Address to filename
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

*** Uncompress LZ77 packed picture
lzdepack:
	move.l	filebuffer,a0
	lea	mt_data,a1
	bsr	lz77
	rts
*** End decompress

depackpic:
	lea lz7pic,a0
	lea picture,a1
	bsr lz77
	rts

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
	bsr	Scroll_ROX		; Move bloc of screen
	bsr	Scroll_ROX
	addi.b	#1,COUNTER		; Test counter of text
	cmpi.b	#4,COUNTER
	bne.s	.put_scrolling
	clr.b	COUNTER
	movea.l	TXT_POINTER,a0		; New character
	moveq	#0,d0			; Test the end of the sentence
	move.b	(a0)+,d0
	tst.b	d0
	bpl.s	.not_end_text
	lea	TEXT,a0			; Wrap sentence
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

	; Add 186 lines to start at the bottom of the screen
	; plus 6 to get the right bitplane
	add.w	#(160*186)+6,a1
	moveq	#7,d0			; 8 lines copied
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

	include "lz77.s"
	include	'INITLIB.S'
	include	'pt_src50.s'		;Protracker player, Lance 50 kHz (STe)


        section	data

filename:	dc.l	0
filebuffer:	dc.l	0
filelength:	dc.l	0

*** TUNE LIST ***
* flib
* roof duct inc.
* real chipper supper
* epic love
* dah
* heavy water
* woo hoo doo goo boo
* vak8ed looks
* d-boned
* hope, joy & fleas
* happy times
* 16th folk song ii
* weeeeeeeeeeeeeeeeed
* screw levity
* rounderbout fishy
* dro, hells mom
* turnips
* lifes warmth
* schizo affected
* rats, bats & fleas
* more fleas
* slapping hippos
* happy vibes
* big numbers
***

** 	filenames - 0 terminated
**  '12345678.123',0,''	; 12 characters per entry
filetab:		dc.b 	'flib.lz7',0,'    '
			dc.b 	'roofduct.lz7',0,''
			dc.b 	'chipsupp.lz7',0,''
			dc.b 	'epiclove.lz7',0,''
			dc.b 	'dah.lz7',0,'     '
			dc.b 	'hvywater.lz7',0,''
			dc.b 	'woohoo.lz7',0,'  '
			dc.b 	'vak8ed.lz7',0,'  '
			dc.b 	'dboned.lz7',0,'  '
			dc.b 	'hopeflea.lz7',0,''
			dc.b 	'hppytime.lz7',0,''
			dc.b 	'folk.lz7',0,'    '
			dc.b 	'weeeeeee.lz7',0,''
			dc.b 	'levity.lz7',0,'  '
			dc.b 	'rounderb.lz7',0,''
			dc.b 	'dro.lz7',0,'     '
			dc.b 	'turnips.lz7',0,' '
			dc.b 	'lifewarm.lz7',0,''
			dc.b 	'schizo.lz7',0,'  '
			dc.b 	'ratsbats.lz7',0,''
			dc.b 	'moreflea.lz7',0,''
			dc.b 	'hippos.lz7',0,'  '
			dc.b 	'hppyvibe.lz7',0,''
			dc.b 	'bignum.lz7',0,'  '
			dc.b 	0
			even
*** end of filenames





lz7pic		incbin	ancients.lz7
lz7title	incbin	shiny.lz7
lz7credits	incbin 	credits.lz7

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
	dc.b 	"Now over to our amazing musician Bex, whose fantastic tunes you're listening to, for a few words....."
	dc.b	"hey guys. This is bextula speaking into Google assistant (not really but let's pretend I'm brave enough). "
	dc.b	"Just want to send a few greets out to the following ppl: all in RiFT (obvs), all in SLP, all in C0SINE, "
	dc.b	"all in DSR, Motion of Artstate, Raizor of (whoes he with now?), H0ffman of logicoma, and of course the "
	dc.b	"loving memory of Giz/DSR. We still miss him, and he did so love a few of these tunes. Oh and Meaty. Still miss u too babe"
	dc.b 	"                                           " 		;bit of empty space before we wrap - LEAVE IT
	dc.b	$FF,$0												; end of text marker - LEAVE IT

	even

        section bss

save_stack:	ds.l 	1
save_screenadr:	ds.l	1
screen		ds.b	160*288
		ds.b	256	; extra bytes for alignment purposes
screen_adr:	ds.l 	1
screen_adr2:	ds.l	1
dta:		ds.b    44	;dta block about file info
vblcount: 	ds.w	1
mt_data		ds.b 	64000
		ds.w	31*640/2		;These zeroes are necessary!
lz7mod		; it points to the end of the buffer so we can unpack in-place

picture:	ds.b	32034
Line_scroll:	ds.b	20*2+1
Adr_scroll:	ds.b	1
Buffer_scroll:	ds.b	21*8*20

backup	ds.b	14
old_vbl	ds.l	1
	even
