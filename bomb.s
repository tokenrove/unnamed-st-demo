

        SECTION TEXT

        XDEF rotobomb_init, rotobomb_main
        XREF sine_lookup_table
        XREF plot_debug_dword

        INCLUDE "st-constants.inc"

        ;; Some general, tweakable? parameters.
rotobomb_viewport_w = 128
rotobomb_viewport_h = 64


rotobomb_init:
        ;; incoming palette: white and black
        MOVE.L #rotobomb_palette, A0
        RTS

rotobomb_main:
        MOVE.L #rotobomb_vbl, vbl_vector
        ;; scale by half.
        MOVE.W #$80, scale_factor
        MOVE.B #0, theta
        MOVE.B #0, scale_direction

        MOVE.W #$2300, SR       ; Unmask most interrupts.

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
        MOVEM.L D0-D7/A0-A2, -(SP)
        ;; XXX update music, check triggers.

        CMP.B #0, scale_direction
        BEQ .scale_down
        SUB.W #1, scale_factor
        BNE .4
        MOVE.B #0, scale_direction
        BRA .4
.scale_down:
        ADD.W #1, scale_factor
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

        ;; put vram pointer in A0.
        MOVEQ #0, D0
        MOVE.B shifter_video_base_high, D0
        LSL.L #8, D0
        MOVE.B shifter_video_base_mid, D0
        LSL.L #8, D0
        ADD.W #(rotobomb_viewport_w/2), D0
        ADD.W #160*(rotobomb_viewport_h/2), D0
        MOVE.L D0, A0
        ADD.L #160, D0
        MOVE.L D0, A2

        MOVEQ #0, D1            ; Y
        MOVE.L #bomb_bmp, A1
        MOVEQ #0, D2
        MOVEQ #0, D7
        MOVEQ #0, D3
        MOVE.W sine, D7
        MOVE.W cosine, D3
.y_loop:
        MOVEQ #0, D0            ; X

        ;; accumulate rotation and scaling
        SWAP D2
        ADD.W D7, D2            ; we store the first component in the
        SWAP D2                 ; upper word, the second component in the
        ADD.W D3, D2            ; lower word.
        MOVEM.L D2/D1, -(SP)    ; ay, by become ay+ax, by+bx, inside the x loop.
                                ; also, save Y loop counter so we can use D1.
        SWAP D2
        ADD.W D3, D2
        SWAP D2
        ADD.W D7, D2
.x_loop:
        MOVEQ #0, D6            ; buffers for next 32 pixels
        MOVEQ #0, D1            ; 
M_inner_rotobomb MACRO
        ;; accumulate rotation
        SWAP D2
        ADD.W D3, D2
        SWAP D2
        SUB.W D7, D2
        ;; currently does second-reality-style tiled display, but
        ;; could be changed to do single image by changing the style of
        ;; clipping, below.
        MOVE.L D2, D4
        ROL.L #8, D4            ; this is the cool bit.
        AND.W #$000F, D4        ; wrap values in oldskool style.
        MOVE.L D2, D5
        ROR.L #7, D5
        AND.W #$001e, D5
        ;; get source bit and draw (maybe)
        MOVE.W (A1,D5.W), D5
        EXT.L D5
        BTST.L D4, D5
        BEQ .inner_end\@
        ADDQ.B #4, D0
        BSET.L D0, D6
        SUBQ.B #1, D0
        BSET.L D0, D6
        SUBQ.B #1, D0
        BSET.L D0, D6
        SUBQ.B #1, D0
        BSET.L D0, D6
.inner_end\@:
        ADDQ.W #4, D0
        ENDM

        REPT 8
        M_inner_rotobomb
        EXG D6, D1
        M_inner_rotobomb
        ENDR

        MOVE.W D1, (A0)+
        ADD.L #6, A0
        MOVE.W D1, (A2)+
        ADD.L #6, A2
        SWAP D1
        MOVE.W D1, (A0)+
        ADD.L #6, A0
        MOVE.W D1, (A2)+
        ADD.L #6, A2

        MOVE.W D6, (A0)+
        ADD.L #6, A0
        MOVE.W D6, (A2)+
        ADD.L #6, A2
        SWAP D6
        MOVE.W D6, (A0)+
        ADD.L #6, A0
        MOVE.W D6, (A2)+
        ADD.L #6, A2


        CMP.W #rotobomb_viewport_w, D0
        BLT .x_loop
        ;; end of X loop.

        ADD.W #160+160-(rotobomb_viewport_w/2), A0
        ADD.W #160+160-(rotobomb_viewport_w/2), A2
        MOVEM.L (SP)+, D2/D1    ; restore accumulated values and loop ctr.

        ADDQ.W #1, D1
        CMP.W #rotobomb_viewport_h, D1
        BLT .y_loop
        ;; end of Y loop.

        ADD.W #160, A0
        MOVE.L D2, D0
        BSR plot_debug_dword

        MOVEM.L (SP)+, D0-D7/A0-A2
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
