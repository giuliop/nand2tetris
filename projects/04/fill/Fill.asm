// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

// 256 rows of 512 pixel/row represented by 32 16-bit words per row (each word
// a memory address)
// memory start at address 16384 (SCREEN) and ends at 16384 + 256 * 32 - 1 =
// 24575

// KBD is address 24576

(WHITE)
    @SCREEN
    D = A
    @address
    M = D       // address is now 16384 or SCREEN

    (WAIT_PRESS)
        @KBD
        D = M       // D = keyboard
        @BLACKEN
        D;JNE       // if key pressed goto BLACKEN
        @WAIT_PRESS
        0;JMP

(BLACKEN)
    @address
    A = M
    M = -1      // -1 is 1111111111111111 so we blacken all memory word pixel

    @address
    M = M + 1   // address moved to next 

    D = M
    @24575
    D = D - A
    @BLACKEN
    D;JLE       // until we reach the end we continue to whiten the screen

(BLACK)
    @SCREEN
    D = A
    @address
    M = D       // address is now 16384 or SCREEN

    (WAIT_DEPRESS)
        @KBD
        D = M           // D = keyboard
        @WHITEN
        D;JEQ           // if key pressed goto WHITEN
        @WAIT_DEPRESS
        0;JMP

(WHITEN)
    @address
    A = M
    M = 0       // we whiten all memory word pixel

    @address
    M = M + 1   // address moved to next 

    D = M
    @24575
    D = D - A
    @WHITEN
    D;JLE       // until we reach the end we continue to whiten the screen
    @WHITE
    0;JMP       // then we goto waiting for next keypress
