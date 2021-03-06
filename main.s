 *
 * unnamed ST demo
 *
 * tokenrove <julian@cipht.net> / 2005
 *
 * TODO:
 * - - replace memory references with symbols in st-constants.s

        SECTION TEXT

        ;; Things we export.
        XDEF video_buffer, video_buffer_second_page, video_buffer_end
        XDEF main

        ;; Symbols we import.
	XREF ymamoto_init, ymamoto_reset, ymamoto_update
        XREF fade_in, fade_out
        XREF chunky_scroll_init, chunky_scroll_main
        XREF palette_scroll_init, palette_scroll_main
        XREF rotobomb_init, rotobomb_main
        ;; from initutil.s
        XREF iu_mouseOn, iu_mouseOff, iu_IKBD_reset

        ;; Vectors and so forth.
        INCLUDE "st-constants.inc"

 * Main entry point.
main:	MOVE.L #super_main, -(SP)
	MOVE.W #38, -(SP)
	TRAP #14		    ; XBIOS Supexec
	ADDQ.L #6, SP

        MOVE.W #0, -(SP)            ; Exit.
        TRAP #1

super_main:
        MOVEM.L D0-D7/A0-A1, -(SP)

        ;;; Disable mouse.
        BSR iu_mouseOff

        ;;; Save system palette.
        MOVEM.L shifter_palette, D0-D7
        MOVEM.L D0-D7, saved_system_palette
        ;;; Save system resolution.
        MOVE.B shifter_resolution, D0
        LEA saved_system_res, A0
        MOVE.B D0, (A0)

        MOVE.W #$2700, SR      ; Mask interrupts.

        ;;; Get VRAM address.
        MOVE.B #0, shifter_resolution     ; Shifter resolution -- low res.
        MOVEQ #0, D0
        MOVE.B shifter_video_base_high, D0      ; Video RAM address.
        LSL.W #8, D0
        MOVE.B shifter_video_base_mid, D0
        LSL.L #8, D0
        ; XXX One more byte available on the STe.

        ;;; Clear VRAM.
        MOVE.L D0, vram_address
        MOVE.L D0, A0
        MOVE.W #32000/4, D1
        MOVEQ #0, D0
.1:     MOVE.L D0, (A0)+
        DBF D1, .1

        ; Save TOS VBL vector, interrupt settings.
        LEA old_vectors, A0
        MOVE.L hbl_vector, (A0)+
        MOVE.L vbl_vector, (A0)+
        MOVE.L kbd_vector, (A0)+
        MOVE.L timer_b_vector, (A0)+
        MOVE.B $FFFA07, (A0)+	; MFP stuff.
        MOVE.B $FFFA09, (A0)+
        MOVE.B $FFFA15, (A0)+
        MOVE.B $FFFA17, (A0)+
        MOVE.B $FFFA19, (A0)+
        MOVE.B $FFFA1b, (A0)+
        MOVE.B $FFFA1f, (A0)+
        MOVE.B $FFFA21, (A0)+

        ;;; setup music playback
        LEA tune, A0
        MOVEQ #1, D0
        BSR ymamoto_init

        MOVE.W #5, D0
        BSR fade_out

        ;; fast chunky scroller
        LEA chunky_map, A0
        BSR chunky_scroll_init
        MOVE.W #3, D0
        BSR fade_in
        BSR chunky_scroll_main
        MOVE.W #6, D0
        BSR fade_out

        ;; palette scroller
        LEA palscroll_map, A0
        BSR palette_scroll_init
        MOVE.W #3, D0
        BSR fade_in
        BSR palette_scroll_main
        MOVE.W #3, D0
        BSR fade_out

        ;; rotobomb
        MOVE.L vram_address, D0
        LSR.L #8, D0
        MOVE.B D0, shifter_video_base_mid
        LSR.L #8, D0
        MOVE.B D0, shifter_video_base_high
        BSR rotobomb_init
        MOVE.W #3, D0
        BSR fade_in
        BSR rotobomb_main
        MOVE.W #3, D0
        BSR fade_out

        ;; XXX end credits

        ;;; Begin shutting things down.
        MOVE.W #$2700, SR       ; Mask interrupts.

        ;;; Restore TOS VBL, timer B vectors.
        LEA old_vectors, A0
        MOVE.L (A0)+, hbl_vector
        MOVE.L (A0)+, vbl_vector
        MOVE.L (A0)+, kbd_vector
        MOVE.L (A0)+, timer_b_vector
        MOVE.B (A0)+, $FFFA07   ; Timers.
        MOVE.B (A0)+, $FFFA09
        MOVE.B (A0)+, $FFFA15
        MOVE.B (A0)+, $FFFA17
        MOVE.B (A0)+, $FFFA19
        MOVE.B (A0)+, $FFFA1b
        MOVE.B (A0)+, $FFFA1f
        MOVE.B (A0)+, $FFFA21

        ;;; Restore vram address
        MOVE.L vram_address, D0
        LSR.L #8, D0
        MOVE.B D0, shifter_video_base_mid
        LSR.L #8, D0
        MOVE.B D0, shifter_video_base_high

        ;;; Restore resolution and palette.
        MOVE.B saved_system_res, shifter_resolution   ; Restore shifter res.
        MOVEM.L saved_system_palette, D0-D7 ; Restore palette.
        MOVEM.L D0-D7, shifter_palette

        MOVE.W #$2300, SR       ; Unmask interrupts.

        BSR ymamoto_reset       ; Mute YM.

        ;;; Restore ACIA state.
        BSR iu_IKBD_reset
        BSR iu_mouseOn
        MOVEM.L (SP)+, D0-D7/A0-A1
        RTS

	SECTION BSS

vram_address: DS.L 1
old_vectors: DS.L 6
saved_system_palette: DS.B 32
saved_system_res: DS.B 1
        EVEN

        ;; We align by putting this pad here, then masking
        ;; video_buffer's address when we use it.  (because we can't
        ;; guarantee we will be loaded with a reasonable alignment).
        DS.B 256
        ; note: two full screens.
video_buffer: DS.B 320*200/2
video_buffer_second_page: DS.B 320*200/2
video_buffer_end:

        SECTION DATA

chunky_map: INCBIN "chunky.map"
palscroll_map: INCBIN "palscroll.map"
tune: INCBIN "tune.bin"

 * vim:syn=asm68k
