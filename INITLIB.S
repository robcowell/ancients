* Standard init function libary
* BBM/DMG 29-12-2004


initialise:	

* set supervisor
	clr.l	-(a7)			;clear stack
	move.w	#32,-(a7)		;prepare for supervisor mode
	trap	#1				;call gemdos
	addq.l	#6,a7			;clean up stack
	move.l	d0,old_stack	;backup old stack pointer
* end set supervisor

* save the old palette; old_palette
	move.l	#old_palette,a0		;put backup address in a0
	movem.l	$ffff8240,d0-d7		;all palettes in d0-d7
	movem.l	d0-d7,(a0)			;move data into old_palette
* end palette save

* saves the old screen adress
	move.w	#2,-(a7)			;get physbase
	trap	#14
	addq.l	#2,a7
	move.l	d0,old_screen		;save old screen address
* end screen save

* save the old resolution into old_resolution
* and change resolution to low (0)
	move.w	#4,-(a7)			;get resolution
	trap	#14
	addq.l	#2,a7
	move.w	d0,old_resolution	;save resolution
	
	move.w	#0,-(a7)		;low resolution
	move.l	#-1,-(a7)		;keep physbase
	move.l	#-1,-(a7)		;keep logbase
	move.w	#5,-(a7)		;change screen
	trap	#14
	add.l	#12,a7
* end resolution save	
	move.b	#$12,$fffffc02.w		;Kill mouse
	rts


restore:
	
* restores the old resolution and screen adress
	move.w	old_resolution,d0	;res in d0
	move.w	d0,-(a7)			;push resolution
	move.l	old_screen,d0		;screen in d0
	move.l	d0,-(a7)			;push physbase
	move.l	d0,-(a7)			;push logbase
	move.w	#5,-(a7)			;change screen
	trap	#14
	add.l	#12,a7
* end resolution and screen adress restore

* restores the old palette
	move.l	#old_palette,a0		;palette pointer in a0
	movem.l	(a0),d0-d7			;move palette data
	movem.l	d0-d7,$ffff8240		;smack palette in
* end palette restore

* clear soundchip just in case	
	lea	$ffff8800.w,a0		; Clear Yammy
	move.l	#$0707ffff,(a0)
	move.l	#$08080000,(a0)
	move.l	#$09090000,(a0)
	move.l	#$0a0a0000,(a0)
* end soundchip

enableMouse:
	move.b	#$8,$fffffc02.w			;Enable mouse
	move.l	#$80,d0
.x
	btst	#1,$fffffc00.w
	beq.s	.x
	move.b	d0,$fffffc02.w

	move.l	#$01,d0
.y
	btst	#1,$fffffc00.w
	beq.s	.y
	move.b	d0,$fffffc02.w

	moveq	#0,d0
.z	
	btst	#0,$fffffc00.w
	beq.s	.z
	move.b	d0,$fffffc02.w

	move.l	old_stack,-(sp)		;
	move.w	#32,-(sp)			;
	trap	#1				;
	addq.l	#6,sp		
	
	rts
		

	section data

old_resolution:	dc.w 0
old_stack:		dc.l 0
old_screen:		dc.l 0

	section bss

old_palette:	ds.l	8