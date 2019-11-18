	section text

	jsr initialise
	jsr init
	bclr #0,$484
	jsr music_init
	move.l	#vbl,$70
	
	jsr piccy
	

	


wait	
	cmp.b	#57,$FFFFFC02
	bne.s	wait
	jsr music_deinit
	bset #0,$484.w

	move.l	#backup,a0
	move.l	(a0)+,$70		;restore vector $70 (vbl)
	move.l	(a0)+,$120		;restore vector $120 (timer b)
	move.b	(a0)+,$fffa07		;restore enable a
	move.b	(a0)+,$fffa13		;restore mask a
	move.b	(a0)+,$fffa15		;restore mask b
	move.b	(a0)+,$fffa1b		;restore timer b control
	move.b	(a0)+,$fffa21		;restore timer b data

	jsr restore

	clr.w -(sp)			;exit
	trap #1

init
	move.l	#backup,a0
	move.l	$70,(a0)+		;backup vector $70 (VBL)
	move.l	$120,(a0)+		;backup vector $120 (timer b)
	move.b	$fffa07,(a0)+		;backup enable a
	move.b	$fffa13,(a0)+		;backup mask a
	move.b	$fffa15,(a0)+		;backup mask b
	move.b	$fffa1b,(a0)+		;backup timer b control
	move.b	$fffa31,(a0)+		;backup timer b data

	bclr #3,$fffffa17.w
	sf	$fffffa21.w
	sf	$fffffa1b.w
	move.l #Over,$120
	
	moveq	#0,d0
	lea	$fffffa00.w,a0
	movep.w	d0,$07(a0)
	movep.w	d0,$13(a0)


piccy	movem.l	picture+2,d0-d7
	movem.l	d0-d7,$ff8240

	move.l 	#screen,d0	;get screen address
	clr.b 	d0			;round to 256 byte boundary
	move.l	d0,a0		;copy to a0
	clr.b	$ff820d		;clear vid address low byte (ste)
	lsr.l	#8,d0
	move.b	d0,$ff8203	;set vid address mid byte
	lsr.w	#8,d0
	move.b	d0,$ff8201	;set vid address high byte

	move.l	#picture+34,a1	;skip header and palette data
	move.l	#(32000/4)-1,d0

.loop1	move.l	(a1)+,(a0)+		;copy pic to screen
	dbf	d0,.loop1
	move.l	#((160*10)/4)-1,d0

.loop2	move.l	#0,(a0)+
	dbf	d0,.loop2
	move.l	#picture+34,a1
	move.l	#((160*78)/4)-1,d0


Over:	
	move.l	(a1)+,(a0)+
	dbra	d7,Over
	rts

*** VBL Routine ***
vbl
	movem.l	d0-d7/a0-a6,-(sp)	;backup registers
	move.w #$700,$ffff8240.w	;bg color red

	jsr music_play
	
	st	Vsync

	move.l	#Over_rout,$120.w
	move.b	#199,$fffffa21.w
	move.b	#8,$fffffa1b.w

	move.w #$000,$ffff8240.w	;bg color black

	movem.l	(sp)+,d0-d7/a0-a6	;restore registers
	rte

Over_rout:
	

	move.w #$700,$ffff8240.w

	REPT	95	* Wait line end
	nop
	ENDR	
	sf	$ffff820a.w	* Modif Frequency 60 Hz !

	REPT	28	* Wait a little
	nop
	ENDR

	move.b	#$2,$ffff820a.w * 50 Hz !

	rte

music_init:
	jsr	music_lance_pt50_init
	rts

music_deinit:
	jsr	music_lance_pt50_exit
	rts

music_play:
	jsr	music_lance_pt50_play
	rts
		
	include	'initlib.s'
	include	'pt_src50.s'		;Protracker player, Lance 50 kHz (STe)


	section	data
mt_data	incbin bignum1024.mod
	ds.w	31*640/2		;These zeroes are necessary!
picture	incbin	shiny.pi1



blackpal:
		dcb.w	16,$0000			;Black palette

	section	bss
		
	ds.b	256
screen	ds.b	160*288
backup	ds.b	14
Vsync:	ds.b	1
		ds.b	1