 * Random generator; same one used in Convergence.
 *
 * From the comments there:
 * Our word generator is basically Marsaglia's modification of Mitchell and
 * Moore's additive generator, as discussed in Knuth's Seminumerical
 * Algorithms (section 3.2.2).  The formula for this generator is
 *
 *    \[ X_n = (X_{n-24} \cdot X_{n-55}) mod m,  n \geq 55, \]
 * where $m$ is our machine word size, in this case.
 *
 * Julian Squires / 2002, 2005

        SECTION TEXT

        ;; Constants
rand_len = 55
lag_j = 23*4
lag_k = 54*4

        XDEF random_init, random_long

random_init:
        MOVEM.L D0-D1/A0-A1, -(SP)
        LEA rand_table, A0
        ;; table position.
        MOVE.W #lag_j, (A0)+
        MOVE.W #lag_k, (A0)+
        ;; Fill table.
        LEA astfgl, A1
        MOVE.W #rand_len-1, D0
.0:     MOVE.L D1, (A1)+
        MOVE.L (A0)+, D1
        DBF D0, .0
        MOVEM.L (SP)+, D0-D1/A0-A1
        RTS

 * Returns a random longword in D0.
random_long:
        MOVEM.L D1-D2/A0, -(SP)

        LEA rand_table, A0
        MOVE.W (A0)+, D0
        MOVE.W (A0)+, D1
        MOVE.L (A0,D0), D0
        MOVE.L (A0,D1), D2
        ADD.L D0, D2
        MOVE.L D2, (A0,D2)

        ;; Update indices.
        SUBQ.W #1, D1
        BNE .0
        MOVE.W #lag_k, D1
.0:     MOVE.W D1, -2(A0)

        MOVE.W -4(A0), D0
        SUBQ.W #1, D0
        BNE .1
        MOVEQ #lag_k, D0
.1:     MOVE.W D0, -4(A0)

        MOVE.L D2, D0
        MOVEM.L (SP)+, D1-D2/A0
        RTS

        SECTION DATA

astfgl: INCBIN "astfgl.dat"

        SECTION BSS

rand_table: DS.L rand_len+1

 * vim:syn=asm68k
