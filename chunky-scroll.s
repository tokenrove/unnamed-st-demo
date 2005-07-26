 *
 * Chunky scrolling.
 * Julian Squires / 2005
 *
 * It's important to note that while this uses the width parameter in
 * several cases, it's actually pretty much being ignored, and things
 * will break if width is less than 20 tiles.
 *
 * Also, if I were actually wise, I would have just changed my tilemap
 * tool to output map data in reverse order, you know.
 *
 * Provides: chunky_scroll_init, chunky_scroll_main

        SECTION TEXT


        INCLUDE "st-constants.inc"

        XDEF chunky_scroll_init, chunky_scroll_main

        ;; main.s
        XREF video_buffer, video_buffer_second_page, video_buffer_end
        XREF main
        ;; initutil.s
        XREF iu_IKBD_reset
        ;; t16-3blt.s
        XREF sss_t163_blit, sss_t163_clean_blit
        ;; font.s
        XREF plot_debug_dword
        ;; ymamoto.s
        XREF ymamoto_update

;;;; Chunky scrolling effect.

        ;; Takes the map data in A0.
chunky_scroll_init:
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
        ;; Since this currently uses static colors, let's just set the
        ;; palette now.  Ideally, I'd like to update this so that the
        ;; map generator adds info on where to change background colors in
        ;; the map, to support more vivid backgrounds.
        MOVE.W #16-1, D0
        MOVE.L #shifter_palette, A1
.0:     MOVE.W (A0)+, (A1)+
        DBF D0, .0

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

        MOVE.L map_data, mapptr     ; will cause wrap-around.
        REPT 13
        BSR draw_new_tiles
        ENDR

        ;; Spend 600 frames before changing scroll speed.
        MOVE.W #200, screen_ctr
        MOVE.W #128, scroll_speed
        MOVEM.L (SP)+, D0-D4/A0-A2
        MOVE.L map_colors, A0       ; palette to fade to
        RTS

chunky_scroll_main:
        MOVEM.L D0-D4/A0-A2, -(SP)
        MOVE.L #chunky_scroll_vbl, vbl_vector
        MOVE.W #$2300, SR       ; Unmask most interrupts.

        ;;; Wait for key press.
.5:     MOVE.B kbd_acia_data, D0
        CMP.B #$39, D0
        BEQ .exit

        ;; do something fun here?
        BRA .5

.exit:  MOVE.W #$2700, SR       ; Mask interrupts.
        CLR.B kbd_acia_data     ; Clear keypress.
        MOVEM.L (SP)+, D0-D4/A0-A2
        RTS

;; map pointer in A2, stomps on lots of registers.
draw_line:
        MOVEQ #0, D0
        MOVE.W drawptr, D0
        LSL.L #8, D0
        MOVE.L D0, A1
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
        BSR sss_t163_blit     ; XXX change back to "normal" blit
                                    ; once debugged
        ;; Next tilewidth to the right.
        SUBA.L #160*16-8, A1
        ENDR
        RTS

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
        MOVE.L D2, A2
        BRA draw_line               ; note tail call optimization.

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


 * Chunky scrolling VBL handler.
chunky_scroll_vbl:
        MOVEM.L D0-A6, -(SP)
        CMP.W #0, screen_ctr
        BNE .4
        MOVE.W #1024, scroll_speed
.4:     SUB.W #1, screen_ctr

        ;; do "physics"
        MOVE.W scroll_ctr, D0
        ADD.W scroll_speed, D0 ; 8.8 fixed point.
        MOVE.W D0, scroll_ctr
        CMP.W #8<<8, D0
        BCS .no_scrolling
        SUB.W #8<<8, scroll_ctr

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

.no_scrolling:
        ;; Update music.
        BSR ymamoto_update
        MOVEM.L (SP)+, D0-A6
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
drawptr: DS.W 1                 ; Next place to draw tiles.
 * Map pointers.
map_header: DS.L 1
map_data: DS.L 1
map_colors: DS.L 1
;map_data_end = map_colors
map_tiles: DS.L 1
map_width: DS.W 1
map_height: DS.W 1
 * Scrolling counter, "event" counter, and pointer into map data.
mapptr: DS.L 1
scroll_ctr: DS.W 1
screen_ctr: DS.W 1
scroll_speed: DS.W 1

 * vim:syn=asm68k
