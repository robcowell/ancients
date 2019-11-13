	opt	o+,s-

super	clr.l	-(a7)		; set TAB to 8
	move.w	#$20,-(a7)
	trap	#1
	addq.l	#6,a7
	move.l	d0,old_sp

	move.l	$70.w,old_70

	bsr piccy
	bsr	mouseoff

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

	move.l	#picture+34,a1
	move.l	#(32000/4)-1,d0
.loop1	move.l	(a1)+,(a0)+		;copy pic to screen
	dbf	d0,.loop1
	move.l	#((160*10)/4)-1,d0
.loop2	move.l	#0,(a0)+
	dbf	d0,.loop2
	move.l	#picture+34,a1
	move.l	#((160*78)/4)-1,d0

start	bsr	play

wait	move.b	$fffc02,d0
try_sp	cmpi.b	#$39,d0
	bne.s	wait

	move.w	#3,replay+28

	move.w	#30000,d0
loop	rept	150
	nop
	endr
	dbf	d0,loop

	bsr.s	stop
	bsr	mouseon

exit	move.l	old_70,$70.w
	move.l	#old_sp,-(sp)
	move.w	#$20,-(sp)
	trap	#1
	addq.l	#6,sp

	clr.l	-(sp)
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

new_70	movem.l	a0-a4/d0-d6,-(sp)
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

        section	data

music	incbin	dah.mod,0
m_end	even

picture	incbin	ancients.pi1
replay	incbin	ninja342.bin
	even

        section bss
        
        ds.b	256
screen	ds.b	160*288

old_sp	ds.l	1
old_70	ds.l	1
old_a07	ds.b	1
old_a13	ds.b	1
old_a15	ds.b	1
	even

