
        SECTION TEXT


        INCLUDE "st-constants.s"

        XDEF chunky_scroll_init, chunky_scroll_vbl
        XREF video_buffer

;;;; Chunky scrolling effect.

chunky_scroll_init:
        LEA video_buffer, A0
        MOVE.L A0, D0
        AND.L #$FFFFFF00, D0
        MOVE.L D0, A0
        BSR paint_checkers

        LEA video_buffer, A0
        MOVE.L A0, D0
        LSR.L #8, D0
        MOVE.W D0, vptr
        MOVE.W D0, vptr_bottom
        MOVE.B D0, shifter_video_base_high
        LSR.L #8, D0
        MOVE.B D0, shifter_video_base_mid

        MOVE.W vptr_bottom, vptr_top
        ADD.W #130, vptr_top
        MOVE.W #0, cur_scroll_pos
        MOVE.B #0, first_line

        ;;; Palette.
        MOVE.W #0, shifter_palette+0
        MOVE.W #%11000000000, shifter_palette+2
        MOVE.W #%11100000000, shifter_palette+4

        RTS

 * Chunky scrolling VBL handler.
chunky_scroll_vbl:
        MOVEM.L D0-D1, -(SP)
        ;; do physics
scroll_speed = 64
        MOVE.W cur_scroll_pos, D0
        ADD.W #scroll_speed, D0

        CMP.W #208<<8, D0
        BCS .3
        SUB.W #208<<8, D0
.3:     MOVE.W D0, cur_scroll_pos

        LSR.W #8, D0
        MOVE.B D0, first_line
        LSR.W #3, D0
        MOVEQ #0, D1
.4:     ADDQ.W #5, D1
        DBF D0, .4
.5:     SUBQ.W #5, D1

        ;MOVEQ #160, D1
        ;MULU.W D0, D1           ; This is totally unnecessary. XXX
        ;LSR.L #8, D1

        ;; Reposition vram.
        MOVE.W vptr_top, D0
        SUB.W D1, D0
        CMP.W vptr_top, D0
        BCS .2
        SUB.W vptr_top, D0
        ADD.W vptr_bottom, D0
.2:     MOVE.W D0, D1
        MOVE.B D0, shifter_video_base_high
        LSR.W #8, D0
        MOVE.B D0, shifter_video_base_mid
        MOVE.W D1, D0
        MOVE.B D0, $FF8207
        LSR.W #8, D0
        MOVE.B D0, $FF8205

        CMP.B #200, first_line
        BLS .1
        SUB.B #200, first_line
.1:

        MOVE.W #%000000000000, shifter_palette+0
        BCLR #5, $FFFA15
        CLR.B mfp_timer_b_control		; Disable timer B.
        CMP.B #0, first_line
        BEQ .0
        MOVE.B first_line, D0
        AND.B #$F8, D0
        MOVE.B D0, mfp_timer_b_data
        MOVE.L #set_vram_during_timerb, timer_b_vector
        MOVE.B #8, mfp_timer_b_control	; Enable timer B (event mode).
        BSET #3, mfp_vector_register
        BSET #0, mfp_interrupt_enable_a	; intA enable timer B.
        BSET #0, mfp_interrupt_mask_a	; intA mask, unmask timer B.

.0:     ;; XXX Update music.
        MOVEM.L (SP)+, D0-D1
        RTE

set_vram_during_timerb:
        MOVE.L D0, -(SP)
        MOVE.W #%000100010100, shifter_palette+0
        MOVE.B #0, mfp_timer_b_control      ; Disable timer B.
        ;;; Start drawing from the top of the buffer.
        MOVE.W vptr_bottom, D0
        MOVE.B D0, shifter_video_base_high
        LSR.W #8, D0
        MOVE.B D0, shifter_video_base_mid
        MOVE.W vptr_bottom, D0
        CLR.B $FF8209
        MOVE.B D0, $FF8207
        LSR.W #8, D0
        MOVE.B D0, $FF8205
        MOVE.L (SP)+, D0
        BCLR #0, mfp_interrupt_in_service_a ; Ack interrupt?
        RTE

 * Simple test pattern.  Takes video pointer in A0.
 * XXX More convoluted than it needs to be.
paint_checkers:
        MOVEQ #192/16-1, D2
.0:     MOVEQ #10-1, D0
.1:     MOVEQ #20-1, D1
.2:     MOVE.L #$FF000000, (A0)+
        CLR.L (A0)+
        DBF D1, .2
        DBF D0, .1
        MOVEQ #6-1, D0
.3:     MOVEQ #20-1, D1
.4:     MOVE.L #$000000FF, (A0)+
        CLR.L (A0)+
        DBF D1, .4
        DBF D0, .3
        DBF D2, .0
        RTS


        SECTION BSS
 * Current start of video buffer.  Stored as only high and med byte.
vptr: DS.W 1
vptr_bottom: DS.W 1             ; Bottom of video buffer, as per vptr.
vptr_top: DS.W 1                ; Top of video buffer, as per vptr.
cur_scroll_pos: DS.W 1
 * First line indicates the scanline where we have to swap vram back to
 * the other side of the video buffer.
first_line: DS.B 1
        EVEN


 * vim:syn=asm68k
