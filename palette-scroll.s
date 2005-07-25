 *
 * Palette scrolling.
 * Julian Squires / 2005
 *
 * See also chunky-scroll.s and tilemap-tool.lisp.  Bugs in the basic
 * scrolling code are probably also bugs in chunky-scroll.s.
 *
 * TODO: Ideally, we can find a nice API for sharing chunky and palette
 * scroll functionality without duplicating code.
 *
 * Provides: palette_scroll_init, palette_scroll_vbl

        SECTION TEXT

        INCLUDE "st-constants.s"

        XDEF palette_scroll_init, palette_scroll_vbl, palette_scroll_stop

        ;; main.s
        XREF video_buffer, video_buffer_second_page, video_buffer_end
        XREF main
        ;; t16-3blt.s
        XREF sss_t163_blit, sss_t163_clean_blit
        ;; font.s
        XREF plot_debug_dword

        ;; Takes the map data in A0.
palette_scroll_init:
        MOVEM.L D0-D4/A0-A2, -(SP)
        ;; Initialize pointers to things.
        ;; map_header
        MOVE.L A0, map_header
        MOVE.W (A0)+, map_width     ; XXX we don't really care.
        MOVE.W (A0)+, map_height
        ADDQ.L #2, A0               ; skip number of tiles.
        MOVE.L A0, map_data
        MOVE.W map_width, D0
        LSL.W #1, D0
        MULU.W map_height, D0
        ADD.L D0, A0
        MOVE.L A0, map_colors
        MOVEQ #0, D0
        MOVE.W map_height, D0
        LSL.L #8, D0                ; skip ahead by 2*height*16.
        ADD.L D0, A0
        MOVE.L A0, map_tiles

        ;; Setup base video pointers.
        ;; set vptr to second page
        MOVE.L #video_buffer_second_page, D0
        LSR.L #8, D0
        MOVE.W D0, vptr
        MOVE.B D0, shifter_video_base_mid
        LSR.W #8, D0
        MOVE.B D0, shifter_video_base_high

        MOVE.L #video_buffer, D0
        LSR.L #8, D0
        MOVE.W D0, vptr_bottom
        MOVE.L #video_buffer_end, D0
        LSR.L #8, D0
        MOVE.W D0, vptr_top
        MOVE.W D0, drawptr

        MOVE.W #0, scroll_ctr

        ;MOVE.L map_tiles, colorptr_base  ; end of colors
        ;SUB.L #200*16, colorptr_base     ; one screen back.
        MOVE.L map_colors, colorptr_base
        ADD.L #7*16, colorptr_base

        MOVE.L map_data, mapptr     ; will cause wrap-around.
        REPT 13
        BSR draw_new_tiles
        ENDR

        ;; Spend 600 frames before changing scroll speed.
        MOVE.W #600, screen_ctr
        MOVE.W #32, scroll_speed

        MOVEM.L (SP)+, D0-D4/A0-A2
        RTS

palette_scroll_stop:
        CLR.B mfp_timer_b_control		; Disable timer B.
        MOVE.L #disable_hbl, hbl_vector
        RTS

disable_hbl:
        MOVE.W #$2300, SR
        RTE

 * Draws a line of tiles and advances (or rather regresses) the map
 * pointer and draw pointer.
draw_new_tiles:
        ;; Move the draw pointer.
        MOVE.W drawptr, D0
        SUB.W #10, D0               ; 10 = (16*160)>>8 = 1 tile
        CMP.W vptr_bottom, D0
        BCS .wrap_and_skip
.0:     MOVE.W D0, drawptr

        ;; Move the mapptr a line.
        MOVEQ #0, D1
        MOVE.W map_width, D1
        ADD.W D1, D1                ; width*2 as each entry is a word.
        MOVE.L mapptr, D2
        SUB.L D1, D2
        CMP.L map_data, D2
        BCC .2
        MOVE.L map_colors, D2       ; map_colors -> end of map data.
        SUB.L D1, D2
.2:     MOVE.L D2, mapptr

        ;; Actually draw the line.
        MOVEQ #0, D0
        MOVE.W drawptr, D0
        LSL.L #8, D0
        MOVE.L D0, A1
        MOVE.L D2, A2
        REPT 20                     ; twenty tiles to draw.
        MOVEQ #0, D0
        MOVE.W (A2)+, D0            ; fetch tile index.
        ;; Get tile data.
        MOVE.L map_tiles, A0
        ;; Multiply by 96 to get tile position.
        LSL.L #5, D0                ; *32
        MOVE.L D0, D1
        ADD.L D0, D0                ; *64
        ADD.L D1, D0                ; x*32 + x*64 = x*96
        ADD.L D0, A0
        ;; Draw tile.
        BSR sss_t163_blit
        ;; Next tilewidth to the right.
        SUBA.L #160*16-8, A1
        ENDR
        RTS

        ;: If it wrapped around, we want to fix up some values and /not/
        ;; draw a line this time, so we stay in sync with the vptr.
.wrap_and_skip:
        MOVE.W vptr_top, drawptr    ; wrap drawptr.
        MOVE.L mapptr, D0
        ADD.L #12*20*2, D0          ; adjust mapptr so we loop smoothly.
        CMP.L map_colors, D0
        BCS .3
        SUB.L map_colors, D0
        ADD.L map_data, D0
.3:     MOVE.L D0, mapptr
        RTS


 * Palette scrolling VBL handler.
palette_scroll_vbl:
        MOVEM.L D0-A6, -(SP)
        CMP.W #0, screen_ctr
        BNE .4
        MOVE.W #256, scroll_speed
.4:     SUB.W #1, screen_ctr

        ;; do "physics"
        MOVE.W scroll_ctr, D0
        ADD.W scroll_speed, D0 ; 8.8 fixed point.
        MOVE.W D0, scroll_ctr
        CMP.W #8<<8, D0
        BCS .no_chunky_scrolling
        SUB.W #8<<8, scroll_ctr

        BRA .no_chunky_scrolling

        ;; Update color pointer.
        MOVE.L colorptr_base, D0
        SUB.L #8*16, D0
        CMP.L map_colors, D0
        BCC .3
        SUB.L map_colors, D0
        ADD.L map_tiles, D0
.3:     MOVE.L D0, colorptr_base

        ;; Reposition vram.
        MOVE.W vptr, D0
        SUBQ.W #5, D0               ; Scroll up by 8 pixels (5 = 8*160>>8)
        CMP.W vptr_bottom, D0
        BCC .0
        MOVE.L #video_buffer_second_page, D0
        LSR.L #8, D0
.0:     MOVE.W D0, vptr
        MOVE.B D0, shifter_video_base_mid
        LSR.W #8, D0
        MOVE.B D0, shifter_video_base_high

        ;; Draw new tiles (note that we draw twice as much data as we
        ;; scroll, so that the bottom page gets fully painted before we
        ;; have to flip pages.
        BSR draw_new_tiles

.no_chunky_scrolling:
        MOVE.L colorptr_base, colorptr
        ;MOVEQ #0, D0
        ;ADD.L #7*16, colorptr
        MOVEQ #0, D0
        MOVE.W scroll_ctr, D0
        LSR.L #8, D0
        LSL.L #4, D0
        SUB.L D0, colorptr

        ;; Setup timer B.
        BCLR #5, mfp_interrupt_mask_b
        CLR.B mfp_timer_b_control		; Disable timer B.
        MOVE.B #1, mfp_timer_b_data
        MOVE.L #set_vram_during_timerb, timer_b_vector
        MOVE.B #8, mfp_timer_b_control	; Enable timer B (event mode).
        BSET #3, mfp_vector_register
        BSET #0, mfp_interrupt_enable_a	; intA enable timer B.
        BSET #0, mfp_interrupt_mask_a	; intA mask, unmask timer B.

        ;; XXX Update music.
        MOVEM.L (SP)+, D0-A6
        RTE


set_vram_during_timerb:
        MOVEM.L D0-D3/A0, -(SP)
        MOVE.L colorptr, A0
        MOVEM.L (A0)+, D0-D3
        MOVEM.L D0-D3, shifter_palette
        ;ADD.L #16, colorptr
        CMP.L map_tiles, A0
        BCS .1
        MOVE.L map_colors, A0
.1:     MOVE.L A0, colorptr
        MOVEM.L (SP)+, D0-D3/A0
        BCLR #0, mfp_interrupt_in_service_a ; Ack interrupt?
        RTE


        SECTION BSS
 * Current start of video buffer.  Stored as only high and med byte.
vptr: DS.W 1
vptr_bottom: DS.W 1             ; Bottom of video buffer, as per vptr.
vptr_top: DS.W 1                ; Top of video buffer, as per vptr.
drawptr: DS.W 1                 ; Next place to draw tiles.
 * Map pointers.
map_header: DS.L 1
map_data: DS.L 1
map_colors: DS.L 1
;map_data_end = map_colors
map_tiles: DS.L 1
map_width: DS.W 1
map_height: DS.W 1
 * Color buffer where palette changes are stored.
colorptr: DS.L 1
colorptr_base: DS.L 1
 * Scrolling counter, "event" counter, and pointer into map data.
mapptr: DS.L 1
scroll_ctr: DS.W 1
screen_ctr: DS.W 1
scroll_speed: DS.W 1

 * vim:syn=asm68k
