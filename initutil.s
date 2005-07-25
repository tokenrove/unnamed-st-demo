*
* Initialisation and utilities
*
* Author: Michael Bricout.
*

* Symbols

; IKDB command lengths
_IKBD_reset_length = 1
_IKBD_mouseOff_length = 0
_IKBD_mouseOn_length = 0

; Registers
_regSynchro = $FF820A			; refresh rate and sync mode

	SECTION	TEXT

        XDEF iu_mouseOn, iu_mouseOff, iu_IKBD_reset

iu_setSuper:

* Switch to superuser mode
*   modifies: D0-D1/A1	(GEMDOS::SUPER)
 
	; Make sure we are not already in supervisor mode

	MOVE.L	#$1,-(SP)
	MOVE.W	#$20,-(SP)		; GEMDOS::SUPER
	TRAP	#1
	ADDQ.L	#6,SP
	CMP.L	#-1,D0
	BEQ	iu_ssEnd

	; Set Supervisor mode

	MOVE.L	#$0,-(SP)
	MOVE.W	#$20,-(SP)		; GEMDOS::SUPER
	TRAP	#1
	ADDQ.L	#6,SP

	MOVE.L	D0,iu_SSPBackup		; save pointer to previous Super. Stack

iu_ssEnd:
	RTS


iu_setUser:

* Switch to user mode
*   modifies: D0-D1/A1	(GEMDOS::SUPER)

	; Make sure we are not already in user mode

	MOVE.L	#$1,-(SP)
	MOVE.W	#$20,-(SP)		; GEMDOS::SUPER
	TRAP	#1
	ADDQ.L	#6,SP
	TST.L	D0
	BEQ	iu_suEnd

	; Set user mode

	MOVE.L	iu_SSPBackup,-(SP)
	MOVE.W	#$20,-(SP)		; GEMDOS::SUPER
	TRAP	#1
	ADDQ.L	#6,SP

iu_suEnd:
	RTS

iu_IKBD_reset:

* Reset the Keyboard processor to default
*

	MOVE.L	#IKBD_reset,-(SP)
	MOVE.W	#_IKBD_reset_length,-(SP)
	BRA	iu_doIKBD
	

iu_mouseOff:

* Disable the mouse
*

	MOVE.L  #IKBD_mouseOff,-(SP)
	MOVE.W  #_IKBD_mouseOff_length,-(SP)
	BRA	iu_doIKBD


iu_mouseOn:

* Activate the mouse
*

	MOVE.L  #IKBD_mouseOn,-(SP)
	MOVE.W	#_IKBD_mouseOn_length,-(SP)
	BRA	iu_doIKBD


iu_doIKBD:

* IKBD to execute command placed on the stack
*
	MOVE.W  #25,-(SP)		; XBIOS::Ikbdws
	TRAP    #14
	ADDQ.L  #8,SP
	RTS

iu_setRefreshRate:

* Set the refresh rate to 50Hz or 60Hz. or restore backed-up value
*  In: (byte) D0 - 0: 60Hz, 1: 50Hz, 2: restore

	CMP.B	#2,D0			; restore the previous refresh rate ?
	BNE	.setRefresh		; no: go force a new value
	MOVE.B	iu_SyncBackup,_regSynchro	; restore previous refresh rate
	RTS

.setRefresh
	MOVE.B	_regSynchro,iu_SyncBackup	; backup current refresh rate
	LSL.B	#1,D0			; D0: refresh rate is bit #1, not #0
	MOVE.B	D0,_regSynchro		; apply the new refresh rate
	RTS


	SECTION	DATA	

IKBD_reset:	DC.B    $80,$01	; IKDB command: reset
IKBD_mouseOff:	DC.B    $12	; IKDB command: mouse off
IKBD_mouseOn:	DC.B    $8	; IKDB command: mouse on

	SECTION BSS

iu_SSPBackup:	DS.L	1	; Backup for superuser stack pointer
iu_SyncBackup:	DS.B	1	; Backup for Sync mode (refresh rate)
	
	EVEN

