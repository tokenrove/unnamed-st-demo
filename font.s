 * Just some fourth-bitplane onebit font routines for debugging.
 *
 * Julian Squires / 2005

        GLOBAL plot_debug_dword, plot_debug_string

 * A0 = destination
 * D0 = long word to plot as hex string.
plot_debug_dword:
	MOVEM.L D0-D2/A0-A2, -(SP)
        LEA debug_string_buf, A2

        LEA hex_xlat, A1
        MOVEQ #8-1, D1
.0:     ROL.L #4, D0
        MOVE.B D0, D2
        AND.L #$f, D2
        MOVE.B (A1,D2), (A2)+
        DBF D1, .0
        MOVE.B #0, (A2)+
        LEA debug_string_buf, A2
        BSR plot_debug_string
	MOVEM.L (SP)+, D0-D2/A0-A2
        RTS


 * A0 = destination (VRAM)
 * A2 = address of string, NUL terminated.
plot_debug_string:
.0:     MOVEQ #0, D0
        MOVE.B (A2)+, D0
        BEQ .1
        LEA one_bpp_font, A1
        LSL.W #3, D0
        ADD.W D0, A1
        MOVEQ #7, D0
.2:     MOVE.B (A1)+, 6(A0)
        ADD.W #160, A0
        DBF D0, .2
        SUB.W #1280, A0
        MOVE.L A0, D0
        BTST #0, D0
        BNE .3
        ADDQ.L #1, A0
        BRA .0
.3:     ADDQ.L #7, A0
        BRA .0
.1:     RTS

	SECTION BSS
debug_string_buf: ds.b 10

        SECTION DATA
one_bpp_font: incbin "readable.f08"
hex_xlat: DC.B "0123456789abcdef"

 * vim:syn=asm68k
