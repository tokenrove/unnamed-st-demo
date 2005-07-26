 * ST palette fade routines.
 *
 * I saw the trick of using the counters as comparators in the source
 * for a Cynix intro.
 *
 * Julian Squires / 2005

        SECTION TEXT

        XDEF fade_in, fade_out

        INCLUDE "st-constants.inc"

 * A0 = palette to fade in to
 * D0 = delay in VBLs.  Should be at least 1.
fade_in:
        MOVEM.L D0-D7, -(SP)
        MOVE.L #fade_vbl, vbl_vector
        MOVE.W #$2300, SR       ; Unmask most interrupts.
        MOVEQ #0, D7            ; blue fade
        MOVEQ #0, D6            ; green fade
        MOVEQ #0, D5            ; red fade
.top:   MOVEQ #15, D1
        LEA shifter_palette, A1
.0:     MOVE.W (A0)+, D2
        BEQ .1
        MOVE.W D2, D3
        MOVE.W D2, D4
        AND.W #$700, D2         ; red component
        AND.W #$070, D3         ; green
        AND.W #$007, D4         ; blue
        CMP.W D2, D5
        BGT .2
        MOVE.W D5, D2
.2:     CMP.W D3, D6
        BGT .3
        MOVE.W D6, D3
.3:     CMP.W D4, D7
        BGT .4
        MOVE.W D7, D4
.4      OR.W D3, D4             ; Combine color components.
        OR.W D4, D2
        MOVE.W D2, (A1)+        ; Write back to palette.
.1:     DBF D1, .0

        SUB.W #32, A0

        MOVE.B D0, fade_ctr
.wait:  TST.B fade_ctr
        BNE .wait

        ADD.W #$100, D5
        ADD.W #$010, D6
        ADD.W #1, D7
        CMP.W #8, D7
        BLT .top

.end:   MOVEM.L (SP)+, D0-D7
        RTS

 * Fade to black.
 * D0 = delay in VBLs.  Should be at least 1.
fade_out:
        MOVEM.L D0-D7, -(SP)
        MOVE.L #fade_vbl, vbl_vector
        MOVE.W #$2300, SR       ; Unmask most interrupts.
        MOVEQ #8-1, D7          ; Number of fade passes. (also blue comparator)
        MOVEQ #$70, D6          ; green fade
        MOVE.W #$700, D5         ; red fade
        ;; Check current palette against black.
.top:   LEA shifter_palette, A0
        MOVEQ #15, D1
.0:     MOVE.W (A0)+, D2
        BEQ .1
        MOVE.W D2, D3
        MOVE.W D2, D4
        AND.W #$700, D2         ; red component
        AND.W #$070, D3         ; green
        AND.W #$007, D4         ; blue
        CMP.W D2, D5
        BGT .2
        MOVE.W D5, D2
.2:     CMP.W D3, D6
        BGT .3
        MOVE.W D6, D3
.3:     CMP.W D4, D7
        BGT .4
        MOVE.W D7, D4
.4      OR.W D3, D4             ; Combine color components.
        OR.W D4, D2
        MOVE.W D2, -2(A0)       ; Write back to palette.
.1:     DBF D1, .0

        MOVE.B D0, fade_ctr
.wait:  TST.B fade_ctr
        BNE .wait

        SUB.W #$100, D5
        SUB.W #$010, D6
        DBF D7, .top

.end:   MOVEM.L (SP)+, D0-D7
        RTS

fade_vbl:
        SUB.B #1, fade_ctr
        RTE

        SECTION BSS
fade_ctr: DS.B 1
        EVEN

 * vim:syn=asm68k
