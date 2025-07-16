.include "nes.inc"
.include "macros.inc"

;*****************************************************************
; Define NES cartridge Header
;*****************************************************************
; NES ROM Header - tells emulator/hardware about the ROM
.segment "HEADER"
.byte 'N', 'E', 'S', $1a      ; "NES" followed by MS-DOS EOF marker
.byte $02                     ; 2 x 16KB PRG-ROM banks
.byte $01                     ; 1 x 8KB CHR-ROM bank
.byte $00, $00                ; Mapper 0, no special features

;*****************************************************************
; Define NES interrupt vectors
;*****************************************************************
; Interrupt vectors - tells CPU where to jump for each interrupt
.segment "VECTORS"
.addr nmi_handler         ; NMI vector ($FFFA-$FFFB)
.addr reset_handler       ; Reset vector ($FFFC-$FFFD)
.addr irq_handler         ; IRQ vector ($FFFE-$FFFF)

;*****************************************************************
; 6502 Zero Page Memory ($0000–$00FF)
;*****************************************************************
; Fast RAM accessible with 1-byte instructions (faster, smaller)
; Use this for variables accessed frequently (like gamepad, game variables, pointers)
.segment "ZEROPAGE"
; Zero Page Memory Map
; $00-$0F: General purpose variables and pointers
temp_var:               .res 1    ; General purpose temp variable
temp_var2:              .res 1    ; Second temp variable
temp_ptr_low:           .res 1    ; 16-bit pointer (2 bytes)
temp_ptr_high:          .res 1    ; 16-bit pointer (2 bytes)
random_num:             .res 1    ; Random number generator value

; Reserve remaining space in this section if needed
                        .res 11   ; Pad to $10 (optional - depends on your needs)

; $10-$1F: Controller input
controller_1:           .res 1    ; Current frame controller 1 state
controller_2:           .res 1    ; Current frame controller 2 state
controller_1_prev:      .res 1    ; Previous frame state for edge detection
controller_2_prev:      .res 1    ; Previous frame state for edge detection
controller_1_pressed:   .res 1    ; Check if pressed
controller_1_released:  .res 1    ; Check if released

; Reserve remaining space in this section if needed
                        .res 10   ; Pad to $20 (optional)

scroll:                 .res 1    ; Scroll screen
time:                   .res 1    ; Time (60hz = 60 FPS)
seconds:                .res 1    ; Seconds

.segment "OAM"
oam: .res 256

;*****************************************************************
; Code Segment (ROM)
;*****************************************************************
; Main program code section
.segment "CODE"

; Interrupt Request Handler - called when IRQ interrupt occurs
.proc irq_handler
  RTI                     ; Return from interrupt (we don't use IRQ)
.endproc

; Non-Maskable Interrupt Handler - called during VBlank
.proc nmi_handler
  ; send register values to stack
  PHA
  TXA
  PHA
  TYA
  PHA

  INC time  ; increment time by 1
  LDA time  ; load time into accumulator
  CMP #60   ; check if time is 60
  BNE skip  ; if not 60, skip
    INC seconds ; increment seconds by 1
    LDA #0      ; reset time to 0
    STA time
  skip:

  ; restore registers
  PLA
  TAY
  PLA
  TAX
  PLA

  RTI                     ; Return from interrupt (not using NMI yet)
.endproc

;******************************************************************************
; Procedure: set_palette
;------------------------------------------------------------------------------
; Writes 32 bytes of color data from palette_data into the PPU's palette memory
; at $3F00. This fills all 4 background palettes and all 4 sprite palettes.
;
; Assumes:
;   - palette_data is a 32-byte table in ROM.
;   - Rendering is off or you're in VBlank (writes to $2007 are safe).
;******************************************************************************
.proc set_palette

    vram_set_address PALETTE_ADDRESS  ; Set PPU VRAM pointer to $3F00 (palette memory start)

    LDX #$00                          ; Start index at 0

@loop:
    LDA palette_data, X              ; Load color byte from palette_data table
    STA PPU_VRAM_IO                  ; Write to PPU at $3F00 + X
    INX                              ; Move to next color
    CPX #$20                         ; Have we written all 32 bytes?
    BNE @loop                        ; Loop until done

    RTS                              ; Return from procedure

.endproc

;******************************************************************************
; Procedure: set_nametable
;------------------------------------------------------------------------------
; Transfers 960 bytes of tile data from `nametable_data` to the PPU's nametable 0
; at $2000. This fills the entire 32×30 background tilemap.
;
; Assumes:
;   - PPU is ready (called during or before VBlank)
;   - nametable_data is a 960-byte table in ROM
;   - $00/$01 are available as temporary zero-page pointer
;******************************************************************************
.proc set_nametable

  wait_for_vblank                        ; Wait for VBlank to safely write to PPU

  ; Set PPU address to start of nametable ($2000)
  vram_set_address NAME_TABLE_0_ADDRESS

  ; Set up 16-bit pointer to nametable_data
  LDA #<nametable_data + 0               ; Add pushes location loaded from nametable
  STA temp_ptr_low                       ; Store low byte of address
  LDA #>nametable_data
  STA temp_ptr_high                      ; Store high byte

  ; If your source data has rows wider than 32 tiles, define the actual width here
  ; For example, if your rows are 64 tiles wide, change this constant
  SOURCE_ROW_WIDTH = 96                  ; Adjust this to your actual row width

  ; Load 30 rows of 32 tiles each
  LDX #30                                ; 30 rows to process

load_row_wide:
  LDY #$00                               ; Start at beginning of row

load_tile_in_row_wide:
  LDA (temp_ptr_low),Y                   ; Load tile from current row
  STA PPU_VRAM_IO                        ; Write to PPU VRAM ($2007)
  INY
  CPY #32                                ; Have we loaded 32 tiles (full NES row)?
  BNE load_tile_in_row_wide              ; Continue loading tiles in this row

  ; Move to next row in source data (skip remaining tiles in wide row)
  CLC
  LDA temp_ptr_low
  ADC #SOURCE_ROW_WIDTH                  ; Add full row width to skip to next row
  STA temp_ptr_low                       ; Store new low byte
  LDA temp_ptr_high
  ADC #0                                 ; Add carry to high byte
  STA temp_ptr_high                      ; Store new high byte

  DEX                                    ; One less row to process
  BNE load_row_wide                      ; Continue until all 30 rows done

  ; Reset PPU address latch by reading PPU_STATUS
  LDA PPU_STATUS                         ; This resets the address latch

  ; Reset scroll registers to 0,0 (important after VRAM writes)
  LDA #$00
  STA PPU_SCROLL                         ; Write horizontal scroll = 0
  STA PPU_SCROLL                         ; Write vertical scroll = 0

  RTS                                    ; Done

.endproc

.proc init_sprites

  RTS
.endproc

.proc update_background
; Set up 16-bit pointer to nametable_data
  LDA #<nametable_data + 0               ; Add pushes location loaded from nametable
  STA temp_ptr_low                       ; Store low byte of address
  LDA #>nametable_data
  STA temp_ptr_high                      ; Store high byte

  ; If your source data has rows wider than 32 tiles, define the actual width here
  ; For example, if your rows are 64 tiles wide, change this constant
  SOURCE_ROW_WIDTH = 96                  ; Adjust this to your actual row width

  ; Load 30 rows of 32 tiles each
  LDX #30                                ; 30 rows to process

load_row_wide:
  LDY #$00                               ; Start at beginning of row

load_tile_in_row_wide:
  LDA (temp_ptr_low),Y                   ; Load tile from current row
  STA PPU_VRAM_IO                        ; Write to PPU VRAM ($2007)
  INY
  CPY #32                                ; Have we loaded 32 tiles (full NES row)?
  BNE load_tile_in_row_wide              ; Continue loading tiles in this row

  ; Move to next row in source data (skip remaining tiles in wide row)
  CLC
  LDA temp_ptr_low
  ADC #SOURCE_ROW_WIDTH                  ; Add full row width to skip to next row
  STA temp_ptr_low                       ; Store new low byte
  LDA temp_ptr_high
  ADC #0                                 ; Add carry to high byte
  STA temp_ptr_high                      ; Store new high byte

  DEX                                    ; One less row to process
  BNE load_row_wide                      ; Continue until all 30 rows done
  RTS
.endproc

.proc update_sprites
  INC scroll
  LDA scroll
  STA PPU_SCROLL                         ; Write horizontal scroll = 0
  LDA #$00
  STA PPU_SCROLL                         ; Write vertical scroll = 0
  RTS

.endproc

;******************************************************************************
; Procedure: main
;------------------------------------------------------------------------------
; Main entry point for the game loop.
; Initializes PPU control settings, enables rendering, and enters
; an infinite loop where it waits for VBlank and updates sprite data.
;******************************************************************************
.proc main
    ; seed the random number
    LDA #$45
    STA random_num
    ;--------------------------------------------------------------------------
    ; Configure PPU Control Register ($2000)
    ; - Enable NMI on VBlank (bit 7 = 1)
    ; - Use pattern table 1 ($1000) for background tiles (bit 4 = 1)
    ;--------------------------------------------------------------------------
    LDA #(PPUCTRL_ENABLE_NMI | PPUCTRL_BG_TABLE_1000)
    STA PPU_CONTROL

    ;--------------------------------------------------------------------------
    ; Configure PPU Mask Register ($2001)
    ; - Show background and sprites (bits 3 & 4 = 1)
    ; - Show background and sprites in leftmost 8 pixels (bits 1 & 2 = 1)
    ;--------------------------------------------------------------------------
    LDA #(PPUMASK_SHOW_BG | PPUMASK_SHOW_SPRITES | PPUMASK_SHOW_BG_LEFT | PPUMASK_SHOW_SPRITES_LEFT)
    STA PPU_MASK

forever:
    JSR get_random

    ; Wait for vertical blank before doing game logic and rendering updates
    wait_for_vblank

    ; Read controller
    JSR read_controller

    ; JSR update_background
    ; Update sprite data (DMA transfer to PPU OAM)
    JSR update_sprites

    ; Infinite loop — keep running frame logic
    JMP forever

.endproc

; ------------------------------------------------------------------------------
; Procedure: read_controller
; Purpose:   Reads the current state of NES Controller 1 and stores the button
;            states as a bitfield in the `controller_1` variable.
;
;            The routine strobes the controller to latch the current button
;            state, then reads each of the 8 button states (A, B, Select, Start,
;            Up, Down, Left, Right) serially from the controller port at $4016.
;            The result is built bit-by-bit into the `controller_1` variable
;            using ROL to construct the byte from right to left.
;
; Notes:     The final layout of bits in `controller_1` will be:
;            Bit 0 = A, Bit 1 = B, Bit 2 = Select, Bit 3 = Start,
;            Bit 4 = Up, Bit 5 = Down, Bit 6 = Left, Bit 7 = Right
; ------------------------------------------------------------------------------
.proc read_controller

  ; save current controller state into previous controller state
  LDA controller_1
  STA controller_1_prev

  ; Read controller state
  ; Controller 1 is at $4016 (controller 2 at $4017)
  LDA #$01
  STA JOYPAD1       ; Strobe joypad - write 1 to latch current button state
                    ; This tells the controller to capture the current button presses
  LDA #$00
  STA JOYPAD1       ; End strobe - write 0 to begin serial data output
                    ; Controller is now ready to send button data one bit at a time
                    ; Next 8 reads from JOYPAD1 will return buttons in sequence

  LDX #$08          ; Set loop counter to 8 (read 8 buttons)

read_loop:
   LDA JOYPAD1       ; Read one bit from joypad ($4016)
                     ; Returns $00 (not pressed) or $01 (pressed)
   LSR A             ; Shift accumulator right - bit 0 goes to carry flag
                     ; If button pressed: carry = 1, if not: carry = 0
   ROL controller_1  ; Rotate controller_1 left through carry
                     ; Shifts previous bits left, adds new bit from carry to bit 0
                     ; Building result byte from right to left
   DEX               ; Decrement loop counter (started at 8)
   BNE read_loop     ; Branch if X != 0 (still have bits to read)
                     ; Loop reads: A, B, Select, Start, Up, Down, Left, Right
                     ; Final controller_1 format: RLDUTSBA
                     ; (R=Right, L=Left, D=Down, U=Up, T=sTart, S=Select, B=B, A=A)

    ; Now controller_1 contains the button state
    ; Bit 0 = A, Bit 1 = B, Bit 2 = Select, etc.

    RTS

.endproc

; -----------------------------------------------------
; 8-bit Pseudo-Random Number Generator using LFSR-like bit mixing
; - Uses 'random_num' to hold and update the current pseudo-random value
; - 'temp' is used as a scratch byte
; - The routine generates a new random number in 'random_num' on each call
; -----------------------------------------------------
get_random:
    LDA random_num      ; Load current random value

    ; Test if we need to apply the feedback polynomial
    ; We check bit 7 (sign bit) - if set, we'll XOR with the tap pattern
    ASL                 ; Shift left, bit 7 -> Carry, bit 0 <- 0
    BCC no_feedback     ; If carry clear (original bit 7 was 0), skip XOR

    ; Apply feedback: XOR with $39 (binary: 00111001)
    ; This represents taps at positions 5,4,3,0 after the shift
    EOR #$39           ; XOR with tap pattern

no_feedback:
    STA random_num      ; Store new random value
    RTS                 ; Return with new random value in A

.segment "CHARS"
; Load CHR data
  .incbin "assets/set.chr"

;*****************************************************************
; Character ROM data (graphics patterns)
;*****************************************************************
.segment "RODATA"
; Load palette data
palette_data:
  .incbin "assets/background_palette.pal"
; Load nametable data
nametable_data:
  .incbin "assets/w1-1.map"
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$36,$37,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$25,$25,$38,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$36,$37,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$39,$3a,$3b,$3c,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$36,$37,$36,$37,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$39,$3a,$3b,$3a,$3b,$3c,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $35,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$25,$25,$25,$25,$38,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $39,$3a,$3b,$3c,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$39,$3a,$3b,$3a,$3b,$3a,$3b,$3c,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $53,$54,$24,$24,$24,$24,$24,$24,$45,$45,$53,$54,$45,$45,$53,$54
  ; .byte $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $55,$56,$24,$24,$24,$24,$24,$24,$47,$47,$55,$56,$47,$47,$55,$56
  ; .byte $47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $24,$24,$24,$24,$31,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $24,$24,$24,$30,$26,$34,$33,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $24,$24,$30,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$31,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $24,$30,$26,$34,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$36,$37,$36,$37,$24,$24
  ; .byte $24,$30,$26,$34,$33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
  ; .byte $36,$37,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $24,$24,$24,$24,$36,$37,$36,$37,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $30,$26,$26,$26,$26,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$25,$25,$25,$25,$38,$24
  ; .byte $30,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
  ; .byte $25,$25,$38,$24,$24,$24,$24,$24,$68,$69,$26,$6a,$24,$24,$24,$24
  ; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $24,$24,$24,$35,$25,$25,$25,$25,$38,$24,$24,$24,$68,$69,$26,$6a
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5,$b4,$b5
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7,$b6,$b7
  ; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$40,$10,$00,$00,$00,$00,$00
  ; .byte $ff,$ff,$5f,$df,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$55,$00,$00,$00
  ; .byte $00,$04,$01,$00,$00,$44,$55,$51,$ff,$ff,$f5,$f5,$ff,$ff,$ff,$ff
  ; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$00,$00,$00
  ; .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$00,$00,$00,$00,$00,$00,$00
  ; .byte $00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  ; .byte $00,$a0,$a0,$a0,$a0,$a0,$a0,$00,$20,$02,$80,$a0,$c3,$30,$00,$00
  ; .byte $ff,$ff,$f3,$ff,$ff,$ff,$ff,$ff,$c1,$fb,$aa,$aa,$aa,$22,$00,$00
  ; .byte $c0,$fc,$38,$08,$ff,$00,$f0,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  ; .byte $0f,$0f,$0b,$0a,$0a,$0e,$0f,$0f,$0f,$0f,$03,$0c,$0f,$00,$0f,$00
  ; .byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$00,$00,$00,$00,$00,$00,$00,$00
  ; .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Startup segment
.segment "STARTUP"

; Reset Handler - called when system starts up or resets
.proc reset_handler
  ; === CPU Initialization ===
  SEI                     ; Set interrupt disable flag (ignore IRQ)
  CLD                     ; Clear decimal mode flag (NES doesn't support BCD)

  ; === APU Initialization ===
  LDX #$40                ; Load X with $40
  STX $4017               ; Write to APU Frame Counter register
                          ; Disables APU frame IRQ

  ; === Stack Initialization ===
  LDX #$FF                ; Load X with $FF (top of stack page)
  TXS                     ; Transfer X to Stack pointer ($01FF)

  ; === PPU Initialization ===
  LDA #$00                ; Set A = $00
  STA PPU_CONTROL         ; PPUCTRL = 0 (disable NMI, sprites, background)
  STA PPU_MASK            ; PPUMASK = 0 (disable rendering)
  STA APU_DM_CONTROL      ; disable DMC IRQ

  ; First VBlank wait - PPU needs time to stabilize
:                         ; Anonymous label (used to branch to in BPL command)
  BIT PPU_STATUS          ; Read PPUSTATUS register
  BPL :-                  ; Branch if Plus (bit 7 = 0, no VBlank)
                          ; Loop until VBlank flag is set

  ; clear_ram
  clear_oam oam

  ; Second VBlank wait - ensures PPU is fully ready
:                         ; Anonymous label (used to branch to in BPL command)
  BIT PPU_STATUS          ; Read PPUSTATUS register again
  BPL :-                  ; Branch if Plus (bit 7 = 0, no VBlank)
                          ; Loop until second VBlank occurs

  JSR set_palette         ; Set palette colors
  JSR set_nametable       ; Set nametable tiles
  JSR init_sprites        ; Initialize sprites

  JMP main                ; Jump to main program
.endproc
