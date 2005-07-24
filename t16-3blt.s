* .SSS tiles blitting functions
* Author: Michael Bricout.
*
* -  Optimised routines for 3bpp 16x16 tiles
*
* Provides:
* - sss_t163_blit
* - sss_t163_clean_blit
* - sss_t163_redraw
* - sss_t163_blank
*
* Depends on:
*
* NOTE: routines are IDENTICAL to those in lib/sprites/opt (SST sprites set)
*
* MESSY!: maintained ss_ names for direct compatibility with SST subs.

	SECTION	TEXT

        XDEF sss_t163_blit, sss_t163_clean_blit, sss_t163_redraw
        XDEF sss_t163_blank

sss_t163_blit:
ss_t163_blit:

* indexed tile blit
*   In: (address) A0 - Beginning of the sprite
*   In: (address) A1 - Destination for blit (LIMITATION: 320 pixels wide buffer)

	MOVE.W	#160,D3			; D3: buffer line increment

	REPT	16

	MOVEM.W	(A0)+,D0-D2	; 24	; read 3 bitplanes
	MOVEM.W	D0-D2,(A1)	; 24	; write bitplanes
	ADDA.W	D3,A1		; 8	; A1: point to next line

	ENDR

	RTS


sss_t163_clean_blit:
ss_t163_clean_blit:

* indexed tile blit - to use instead of blit in case plane #3 is dirty
*   In: (address) A0 - Beginning of the sprite
*   In: (address) A1 - Destination for blit (LIMITATION: 320 pixels wide buffer)

	CLR.W	D3			; D3: clean
	MOVE.W	#160,D4			; D4: buffer line increment

	REPT	16

	MOVEM.W	(A0)+,D0-D2		; read 3 bitplanes
	MOVEM.W	D0-D3,(A1)		; write 4 bitplanes (last one is 0)
	ADDA.W	D4,A1			; A1: point to next line
	
	ENDR

	RTS


sss_t163_redraw:
ss_t163_redraw:

* indexed tile redraw
*   Call this to redraw a tile known to be dirty.
*   Only the lines which really are dirty are actually redrawn.
*   In: (address) A0 - Beginning of the sprite
*   In: (address) A1 - Destination for blit (LIMITATION: 320 pixels wide buffer)

M_test_dirty	MACRO
	CMP.W	6(A1),D3	; 12	; peek at content of plane #3
	ENDM

M_draw_line	MACRO
	MOVEM.W	(A0)+,D0-D2	; 24	; read 3 bitplanes + point to next sprt
	MOVEM.W	D0-D3,(A1)	; 24	; write 4 bitplanes (last one is 0)
	ADDA.W  D4,A1		; 8	; A1: point to next line
	ENDM

M_skip_line	MACRO
	ADDQ.W	#6,A0		; 4	; A0: point to next sprite
	ADDA.W  D4,A1		; 8	; A1: point to next line
	ENDM
	
	CLR.W	D3			; D3: clean (reusable "0" value)
	MOVE.W	#160,D4			; D4: buffer line increment

	; loop 1: lines redraw

	M_test_dirty
	BEQ     .skip00
.draw00
	M_draw_line
	M_test_dirty
	BEQ     .skip01
.draw01
	M_draw_line
	M_test_dirty
	BEQ     .skip02
.draw02
	M_draw_line
	M_test_dirty
	BEQ     .skip03
.draw03
	M_draw_line
	M_test_dirty
	BEQ     .skip04
.draw04
	M_draw_line
	M_test_dirty
	BEQ     .skip05
.draw05
	M_draw_line
	M_test_dirty
	BEQ     .skip06
.draw06
	M_draw_line
	M_test_dirty
	BEQ     .skip07
.draw07
	M_draw_line
	M_test_dirty
	BEQ     .skip08
.draw08
	M_draw_line
	M_test_dirty
	BEQ     .skip09
.draw09
	M_draw_line
	M_test_dirty
	BEQ     .skip10
.draw10
	M_draw_line
	M_test_dirty
	BEQ     .skip11
.draw11
	M_draw_line
	M_test_dirty
	BEQ     .skip12
.draw12
	M_draw_line
	M_test_dirty
	BEQ     .skip13
.draw13
	M_draw_line
	M_test_dirty
	BEQ     .skip14
.draw14
	M_draw_line
	M_test_dirty
	BEQ     .skip15
.draw15
	MOVEM.W (A0),D0-D2
	MOVEM.W D0-D3,(A1)
	RTS

	; loop 2: lines skip

.skip00
	M_skip_line
	M_test_dirty
	BNE	.draw01
.skip01
	M_skip_line
	M_test_dirty
	BNE	.draw02
.skip02
	M_skip_line
	M_test_dirty
	BNE	.draw03
.skip03
	M_skip_line
	M_test_dirty
	BNE	.draw04
.skip04
	M_skip_line
	M_test_dirty
	BNE	.draw05
.skip05
	M_skip_line
	M_test_dirty
	BNE	.draw06
.skip06
	M_skip_line
	M_test_dirty
	BNE	.draw07
.skip07
	M_skip_line
	M_test_dirty
	BNE	.draw08
.skip08
	M_skip_line
	M_test_dirty
	BNE	.draw09
.skip09
	M_skip_line
	M_test_dirty
	BNE	.draw10
.skip10
	M_skip_line
	M_test_dirty
	BNE	.draw11
.skip11
	M_skip_line
	M_test_dirty
	BNE	.draw12
.skip12
	M_skip_line
	M_test_dirty
	BNE	.draw13
.skip13
	M_skip_line
	M_test_dirty
	BNE	.draw14
.skip14
	M_skip_line
	M_test_dirty
	BNE	.draw15
.skip15
	RTS


sss_t163_blank:
ss_t163_blank:
* TODO: NOT TESTED !!

* erase tile
*   In: (address) A0 - Destination for blank (320 pixels wide buffer)

	CLR.W	D0			; D0: clean
	MOVE.W	D0,D1			; D1: clean
	MOVE.W	D0,D2			; D2: clean
	MOVE.W	#160,D3			; D3: buffer line increment

	REPT	16

	MOVEM.W	D0-D2,(A0)		; clear 3 bitplanes
	ADDA.W	D3,A0			; A0: point to next line

	ENDR

	RTS

* vim:syn=asm68k
