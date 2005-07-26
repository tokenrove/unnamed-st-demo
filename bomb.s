

        SECTION TEXT

        XDEF rotobomb_init, rotobomb_main

        XREF sine_lookup_table
        XREF plot_debug_dword
        XREF ymamoto_update, ymamoto_reset

        INCLUDE "st-constants.inc"

        ;; Some general, tweakable? parameters.
rotobomb_viewport_w = 320
rotobomb_viewport_h = 160


rotobomb_init:
        MOVEM.L D0-D5, -(SP)
        ;; wipe vram
        MOVEQ #0, D0
        MOVE.B shifter_video_base_high, D0
        LSL.L #8, D0
        MOVE.B shifter_video_base_mid, D0
        LSL.L #8, D0
        MOVE.L D0, A0
        MOVEQ #0, D1
        MOVEQ #0, D2
        MOVEQ #0, D3
        MOVEQ #0, D4
        MOVEQ #0, D5
        MOVE.W #32000/20, D0
.0:     MOVEM.L D1-D5, (A0)
        ADD.L #20, A0
        DBF D0, .0

        ;; incoming palette: white and black
        LEA rotobomb_palette, A0
        MOVEM.L (SP)+, D0-D5
        RTS

rotobomb_main:
        MOVE.L #rotobomb_vbl, vbl_vector
        MOVE.W #$80, scale_factor
        MOVE.B #0, theta
        MOVE.B #0, scale_direction

        MOVE.W #$2300, SR       ; Unmask most interrupts.

        BSR ymamoto_reset

        ;;; Wait for key press.
.main_loop:
        MOVE.B kbd_acia_data, D0
        CMP.B #$39, D0
        BEQ .exit

        BRA .main_loop

.exit:  MOVE.W #$2700, SR       ; Mask interrupts.
        CLR.B kbd_acia_data     ; Clear keypress.
        RTS

rotobomb_vbl:
        MOVEM.L D0-D7/A0-A4, -(SP)
        ;; update music, check triggers.
        ;BSR ymamoto_update

        CMP.B #0, scale_direction
        BEQ .scale_down
        SUB.W #16, scale_factor
        CMP.W #$20, scale_factor
        BGT .4
        MOVE.B #0, scale_direction
        BRA .4
.scale_down:
        ADD.W #16, scale_factor
        CMP.W #$200, scale_factor
        BLT .4
        MOVE.B #1, scale_direction
.4:

        ADD.B #1, theta

        ;; load sine, cosine, scale_factor
        ;; XXX adjust sine and cosine by scaling factor
        MOVEQ #0, D0
        MOVE.B theta, D0
        LSL.W #1, D0
        LEA sine_lookup_table, A0
        MOVE.W (A0,D0.W), D1
        MOVE.W D1, sine
        LSR.W #1, D0
        ADD.W #64, D0                   ; 90 degree shift
        AND.W #$FF, D0
        LSL.W #1, D0
        MOVE.W (A0,D0.W), D1
        MOVE.W D1, cosine

 * Main bomb redraw.
 * It would be possible to speed this up further in a variety of ways,
 * but notably if my assembler's macro capabilities were better. :-/

        ;; put vram pointer in A0.
        MOVEQ #0, D0
        MOVE.B shifter_video_base_high, D0
        LSL.L #8, D0
        MOVE.B shifter_video_base_mid, D0
        LSL.L #8, D0
        ADD.W #80-(rotobomb_viewport_w/4), D0
        ADD.W #160*(100-(rotobomb_viewport_h/2)), D0
        MOVE.L D0, A0
        ADD.L #160, D0
        MOVE.L D0, A2
        ADD.L #160, D0
        MOVE.L D0, A3
        ADD.L #160, D0
        MOVE.L D0, A4

        MOVEQ #0, D1            ; Y
        MOVE.L #bomb_bmp, A1
        MOVEQ #0, D2            ; rotscale step info.
        MOVEQ #0, D7
        MOVEQ #0, D3
        MOVE.W sine, D7
        MULU.W scale_factor, D7
        ASR.L #8, D7
        MOVE.W cosine, D3
        MULU.W scale_factor, D3
        ASR.L #8, D3
.y_loop:
        MOVEQ #0, D0            ; X counter.

        MOVEM.L D2/D1, -(SP)    ; ay, by become ay+ax, by+bx, inside the x loop.
                                ; also, save Y loop counter so we can use D1.
.x_loop:
        MOVEQ #0, D6            ; buffers for next 32 pixels
        MOVEQ #0, D1            ; 
M_inner_rotobomb MACRO
        ;; currently does second-reality-style tiled display, but
        ;; could be changed to do single image by changing the style of
        ;; clipping, below.
        MOVE.L D2, D4
        ROL.L #8, D4            ; this is the cool bit.
        AND.B #$F, D4           ; wrap values in oldskool style.
        MOVE.L D2, D5
        ROR.L #7, D5
        AND.W #$1E, D5
        ;; get source bit and draw (maybe)
        MOVE.W (A1,D5.W), D5
        BTST.L D4, D5
        BEQ .inner_end\@
        MOVEQ #%1111, D5        ; four pixels at a time.
        ROL.L D0, D5            ; safe because rol uses D0 modulo 32 anyway.
        OR.L D5, D6
.inner_end\@:
        ADDQ.W #4, D0
        ;; accumulate rotation
        SWAP D2
        ADD.W D3, D2            ; [0][0] + cosine
        SWAP D2
        SUB.W D7, D2            ; [1][0] - sine
        ENDM

        REPT 8                  ; 8*4 = 32 pixels
        M_inner_rotobomb
        ENDR
        EXG D6, D1
        REPT 8
        M_inner_rotobomb
        ENDR

        ;; Ugh, looks like it's time to fix the macros in my assembler.
        SWAP D6
        MOVE.W D6, (A0)
        MOVE.W D6, (A2)
        MOVE.W D6, (A3)
        MOVE.W D6, (A4)
        SWAP D6
        MOVE.W D6, 8(A0)
        MOVE.W D6, 8(A2)
        MOVE.W D6, 8(A3)
        MOVE.W D6, 8(A4)
        SWAP D1
        MOVE.W D1, 16(A0)
        MOVE.W D1, 16(A2)
        MOVE.W D1, 16(A3)
        MOVE.W D1, 16(A4)
        SWAP D1
        MOVE.W D1, 24(A0)
        MOVE.W D1, 24(A2)
        MOVE.W D1, 24(A3)
        MOVE.W D1, 24(A4)
        ADD.L #32, A0
        ADD.L #32, A2
        ADD.L #32, A3
        ADD.L #32, A4

        CMP.W #rotobomb_viewport_w, D0
        BLT .x_loop
        ;; end of X loop.

        ADD.W #3*160, A0
        ADD.W #3*160, A2
        ADD.W #3*160, A3
        ADD.W #3*160, A4
        MOVEM.L (SP)+, D2/D1    ; restore accumulated values and loop ctr.

        ;; accumulate rotation and scaling
        ;; we store the first component in the upper word, the second component
        ;; in the lower word.
        SWAP D2
        ADD.W D7, D2            ; [1][0] + cosine
        SWAP D2                 
        ADD.W D3, D2            ; [1][1] + sine

        ADDQ.W #4, D1
        CMP.W #rotobomb_viewport_h, D1
        BLT .y_loop
        ;; end of Y loop.

        MOVEM.L (SP)+, D0-D7/A0-A4
        RTE


        SECTION DATA
 * Geez, I should just pull this from ROM.
bomb_bmp:
        DC.W %0000110000000000
        DC.W %0101001000000000
        DC.W %0000000100000000
        DC.W %1001000010000000
        DC.W %0010001111100000
        DC.W %0000001111100000
        DC.W %0000111111111000
        DC.W %0001111111111100
        DC.W %0001110111111100
        DC.W %0011111111111110
        DC.W %0011111111011110
        DC.W %0001111111011100
        DC.W %0001111110111100
        DC.W %0000111111111000
        DC.W %0000011111110000
        DC.W %0000000111000000

rotobomb_palette: DC.W $0777, $0000
        DC.W 0,0,0,0,0,0
        DC.W 0,0,0,0,0,0,0,0

        SECTION BSS

scale_factor: DS.W 1
scale_direction: DS.B 1
theta: DS.B 1
        EVEN
sine: DS.W 1
cosine: DS.W 1

 * vim:syn=asm68k
