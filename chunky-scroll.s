 *
 * Chunky scrolling.
 * Julian Squires / 2005
 *
 * It's important to note that while this uses the width parameter in
 * several cases, it's actually pretty much being ignored, and things
 * will break if width is less than 20 tiles.
 *
 * Provides: chunky_scroll_init, chunky_scroll_vbl

        SECTION TEXT


        INCLUDE "st-constants.s"

        XDEF chunky_scroll_init, chunky_scroll_vbl
        ;; main.s
        XREF video_buffer, video_buffer_second_page, video_buffer_end
        ;; t16-3blit.s
        XREF sss_t163_blit

;;;; Chunky scrolling effect.

        ;; Takes the map data in A0.
chunky_scroll_init:
        MOVEM.L A2-A3, -(SP)
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
        MOVE.W D0, drawptr
        MOVE.B D0, shifter_video_base_mid
        LSR.W #8, D0
        MOVE.B D0, shifter_video_base_high

        MOVE.L #video_buffer, D0
        LSR.L #8, D0
        MOVE.W D0, vptr_bottom
        MOVE.L #video_buffer_end, D0
        LSR.L #8, D0
        MOVE.W D0, vptr_top

        MOVE.W #0, scroll_ctr

        ;; Move the mapptr back by a full screen.
        MOVE.W #13-1, D0            ; 13 = 208 pixels
        MOVEQ #0, D1
        MOVE.W map_width, D1
        ADD.W D1, D1                ; width*2 (bytes)
        MOVE.L map_colors, D2
        ;MOVE.L map_data, D2
        ;ADD.L #29*20*2, D2
.1:     SUB.L D1, D2
        CMP.L map_data, D2
        BCC .2
        MOVE.L map_colors, D2
        SUB.L D1, D2
.2:     DBF D0, .1
        MOVE.L D2, mapptr

        ;; draw first screen plus one tile.
        MOVE.L mapptr, A2
        SUB.W #5, drawptr
        REPT 13
        BSR draw_line
        ADD.W #10, drawptr
        ENDR

        ;; set draw_ptr to vptr-5
        MOVE.W vptr, drawptr
        SUB.W #10, drawptr

        MOVEM.L (SP)+, A2-A3
        RTS

M_fetch_and_draw_tile MACRO
        MOVEQ #0, D0
        MOVE.W (A2)+, D0
        MOVE.L map_tiles, A0
        ;; Multiply by 96 to get tile position.
        LSL.W #5, D0                ; *32
        MOVE.W D0, D1
        ADD.W D0, D0                ; *64
        ADD.W D1, D0                ; x*32 + x*64 = x*96
        ADD.L D0, A0
        BSR sss_t163_blit
        ENDM

;; map pointer in A2, stomps on lots of registers.
draw_line:
        MOVEQ #0, D0
        MOVE.W drawptr, D0
        LSL.L #8, D0
        MOVE.L D0, A1
        REPT 20
        M_fetch_and_draw_tile
        SUBA.L #160*16-8, A1
        ENDR
        RTS

 * Chunky scrolling VBL handler.
chunky_scroll_vbl:
        MOVEM.L D0-A6, -(SP)
        ;; do physics
scroll_speed = 1024
        ADD.W #scroll_speed, scroll_ctr

        CMP.W #8<<8, scroll_ctr
        BCS .4
        SUB.W #8<<8, scroll_ctr

        ;; Reposition vram.
        MOVE.W vptr, D0
        SUBQ.W #5, D0
        CMP.W vptr_bottom, D0
        BCC .2
        MOVE.L #video_buffer_second_page, D0
        LSR.L #8, D0
.2:     MOVE.W D0, vptr
        MOVE.B D0, shifter_video_base_mid
        LSR.W #8, D0
        MOVE.B D0, shifter_video_base_high

        ;; Draw new tiles (note that we draw twice as much data as we
        ;; scroll, so that the bottom page gets fully painted before we
        ;; have to flip pages.
        MOVE.L mapptr, A2
        BSR draw_line

        ;; Move the draw pointer.
        MOVE.W drawptr, D0
        CMP.W vptr_bottom, D0
        BCC .5
        MOVE.W vptr_top, drawptr
.5:     SUB.W #10, drawptr          ; 10 = (16*160)>>8 = 1 tile

        ;; Move the mapptr a line.
        MOVE.W map_width, D1
        ADD.W D1, D1
        EXT D1
        MOVE.L mapptr, D2
.1:     SUB.L D1, D2
        CMP.L map_data, D2
        BCC .3
        MOVE.L map_colors, D2
        SUB.L D1, D2
.3:     MOVE.L D2, mapptr

.4:     ;; XXX Update music.
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
 * Scrolling counter and pointer into map data.
scroll_ctr: DS.W 1
mapptr: DS.W 1
        EVEN


 * vim:syn=asm68k
