;****************** main.s ***************
;ECE3436 Fall 2017
;Lab 2+3
; Program written by: Liam Cauquil, Tahsin As Sami

; Brief description of the program
;   The LED toggles at 8 Hz and a varying duty-cycle
; Hardware connections (External: One button and one LED)
;  PE1 is Button input  (1 means pressed, 0 means not pressed)
;  PE0 is LED output (1 activates external9 LED on protoboard)
;  PF4 is builtin button SW1 on Launchpad (Internal) 
;        Negative Logic (0 means pressed, 1 means not pressed)
; Overall functionality of this system is to operate like this
;   1) Make PE0 an output and make PE1 and PF4 inputs.
;   2) The system starts with the the LED toggling at 8Hz,
;      which is 8 times per second with a duty-cycle of 20%.
;      Therefore, the LED is ON for (0.2*1/8)th of a second
;      and OFF for (0.8*1/8)th of a second.
;   3) When the button on (PE1) is pressed-and-released increase
;      the duty cycle by 20% (modulo 100%). Therefore for each
;      press-and-release the duty cycle changes from 20% to 40% to 60%
;      to 80% to 100%(ON) to 0%(Off) to 20% to 40% so on
;   4) Implement a "breathing LED" when SW1 (PF4) on the Launchpad is pressed:
;      a) Be creative and play around with what "breathing" means.
;         An example of "breathing" is most computers power LED in sleep mode
;         (e.g., https://www.youtube.com/watch?v=ZT6siXyIjvQ).
;      b) When (PF4) is released while in breathing mode, resume blinking at 8Hz.
;         The duty cycle can either match the most recent duty-
;         cycle or reset to 20%.
;      TIP: debugging the breathing LED algorithm and feel on the simulator is impossible.


SYSCTL_RCGCGPIO_R  EQU 0x400FE608
; PortE device registers
GPIO_PORTE_DATA_R  EQU 0x400243FC
GPIO_PORTE_DIR_R  EQU 0x40024400
GPIO_PORTE_AFSEL_R EQU 0x40024420
GPIO_PORTE_DEN_R  EQU 0x4002451C
; PortF device registers
GPIO_PORTF_DATA_R  EQU 0x400253FC
GPIO_PORTF_DIR_R  EQU 0x40025400
GPIO_PORTF_AFSEL_R EQU 0x40025420
GPIO_PORTF_PUR_R  EQU 0x40025510
GPIO_PORTF_DEN_R  EQU 0x4002551C
NVIC_ST_CTRL_R      EQU 0xE000E010
NVIC_ST_RELOAD_R  EQU 0xE000E014
NVIC_ST_CURRENT_R  EQU 0xE000E018
PF4                  EQU 0x40025040
PE0                  EQU 0x40024004
PE1                  EQU 0x40024008

ONTIME EQU 0
OFFTIME EQU 4
NEXT EQU 8
      IMPORT PLL_Init
      AREA    |.text|, CODE, READONLY, ALIGN=2
      THUMB
      EXPORT  Start
;state machine that loops back onto itself. Each state has is a factor of 25ms, increasing the frequency
U0 DCD 0, 5, U1
U1 DCD 1, 4, U2
U2 DCD 2, 3, U3
U3 DCD 3, 2, U4
U4 DCD 4, 1, U5
U5 DCD 5, 0, U0
    
PortE_F_Initialization

  LDR R0,=SYSCTL_RCGCGPIO_R  ; R0 points to SYSCTL_RCGCGPIO_R
  LDR R1,[R0]    ; read SYSCTL_RCGCGPIO_R into R1
  ORRS R1,#0x30  ;turn on clock Bin: 00110000 = 30; 00FEDCBA
  STR R1,[R0]    ; write back to SYSCTL_RCGCGPIO_R

  NOP            ; wait for clock to stabilize
  NOP
  NOP            ; Enough to wait 2 cycles but the simulation
  NOP            ; for PortF is a little finicky and wants 4 cycles 
  
  
  ;PORT F SHIT
  LDR R0,=GPIO_PORTF_DIR_R
  LDR R1, [R0]
  ORR R1, #0X10
  ;MOV R1,#0x0A                ;PF3 & PF1 outputs, PF0 & PF4 inputs 
  STR R1,[R0]
  
  LDR R0, = GPIO_PORTF_PUR_R  ;pull up register for PF4 
  LDR R1, [R0]
  ORR R1, #0x10                 ;enable weak pull up on PF4
  STR R1, [R0]

  LDR R0,=GPIO_PORTF_DEN_R
  LDR R1, [R0]
  ORR R1, #0X10
  ;MOV R1,#0x10                  ;enable PF4
  STR R1,[R0]
  
  
  ;PORT E SHIT
  LDR R0,=GPIO_PORTE_DIR_R
  LDR R1, [R0]
  ORR R1, #0x01                ;PE0 output, PE1 input
  BIC R1, #0X02
  STR R1,[R0]

  LDR R0,=GPIO_PORTE_DEN_R
  LDR R1, [R0]  
  ORR R1, #0x03                ;enable PE0 & PE1
  STR R1,[R0]
  
  BX LR 
  
SysTick_Init

; disable SysTick during setup

    LDR R1, =NVIC_ST_CTRL_R
    MOV R0, #0            ; Clear Enable        
    STR R0, [R1] 

; set reload to maximum reload value

    LDR R1, =NVIC_ST_RELOAD_R 
    LDR R0, =2000000;    ; Specify RELOAD value
    STR R0, [R1]            ; reload at maximum      

; writing any value to CURRENT clears it

    LDR R1, =NVIC_ST_CURRENT_R 
    MOV R0, #0              
    STR R0, [R1]            ; clear counter

; enable SysTick with core clock

    LDR R1, =NVIC_ST_CTRL_R    
    MOV R0, #0x0005    ; Enable but no interrupts (later)
    STR R0, [R1]      ; ENABLE and CLK_SRC bits set
    BX  LR 
;-------------------------------------------------------------------------------------------------------------------------------------------

Start
    BL PLL_Init
    BL PortE_F_Initialization
    BL SysTick_Init

    LDR R5, =U1
	LDR R0, [R1, #ONTIME]
	LDR R2, =GPIO_PORTE_DATA_R
	MOV R4, #0
	
    
    ;LDR R5, =W2_5ms ;time LED is on during breathing
    ;LDR R6, =W10ms  ;time LED is off during breathing
    
    MOV R7, #0        ;R7 is a counter for the amount of duty cycles that passed during breathing
    MOV R8, #0         ;R8 is a reverse counter for the amount of duty cycles that passed during breathing
    MOV R10, #0        ;R10 is a trigger for when PF4 is pressed and unpressed 
	MOV R11, #0
loop
    
    BL Check_PF4 ;subroutine that checks for button at port pf4
    
    BL Button_Input
	PUSH {R1} ; pushing r1 after led on overwrites r1
    
    ;BL LED_On        ;On portion of blinking takes place here
	ORR R3, #0X01
	STR R3, [R2]
	LDR R0, [R5, #ONTIME]
	BL SysTick_Wait25ms
	POP {R1}
	PUSH {R1}
    ;BL LED_Off        ;Off portion of blinking takes place here
	BIC R3, #0X01
	STR R3, [R2]
	LDR R0, [R5, #OFFTIME]
	BL SysTick_Wait25ms
	POP {R1}
    B loop
;-------------------------------------------------------------------------------------------------------------------------------------------    
    

;------------SysTick_Wait------------
; Time delay using busy wait.
; Input: R0  delay parameter in units of the core clock (units of 12.5 nsec for 80 MHz clock)
; Output: none
; Modifies: R0, R1, R3
SysTick_Wait
    LDR  R1, =NVIC_ST_RELOAD_R      ; R1 = &NVIC_ST_RELOAD_R
    SUB  R0, #1
    STR  R0, [R1]                   ;delay-1;  // number of counts to wait
    LDR  R1, =NVIC_ST_CTRL_R        ; R1 = &NVIC_ST_CTRL_R
SysTick_Wait_loop
    LDR  R3, [R1]                   ; R3 = NVIC_ST_CTRL_R
    ANDS R3, R3, #0x00010000       ; Count set?
    BEQ  SysTick_Wait_loop
    BX   LR                         ; return

;------------SysTick_Wait10ms------------
; Time delay using busy wait.  This assumes 50 MHz clock
; Input: R0  number of times to wait 10 ms before returning
; Output: none
; Modifies: R0
DELAY25MS             EQU 2000000    ; clock cycles in 10 ms (assumes 80 MHz clock)
SysTick_Wait25ms
	CMP R0, #0
	BXEQ LR
	
    PUSH {R4, LR}                   ; save current value of R4 and LR
    MOVS R4, R0                     ; R4 = R0 = remainingWaits
    BEQ SysTick_Wait25ms_done       ; R4 == 0, done
SysTick_Wait25ms_loop
    LDR R0, =DELAY25MS              ; R0 = DELAY10MS
    BL  SysTick_Wait                ; wait 10 ms
    SUBS R4, R4, #1                 ; R4 = R4 - 1; remainingWaits--
    BHI SysTick_Wait25ms_loop       ; if(R4 > 0), wait another 10 ms
SysTick_Wait25ms_done
    POP {R4, LR}                    ; restore previous value of R4 and LR
    BX  LR                          ; return
    
;-------------------------------------------------------------------------------------------------------------------------------------------
Button_Input    
    
    LDR R3, [R2]     ; read just PE1
    AND R3, #0x02     ; just bit 1
    CMP R3, R4     ; check if switch is pressed
    BMI Switch_duty ;changes duty cycle during blinking
    
Switch_duty_return
	MOV R4, R3
    BX LR
;-------------------------------------------------------------------------------------------------------------------------------------------    
Switch_duty
    
    LDR R5, [R5, #NEXT] 

    B Switch_duty_return
;-------------------------------------------------------------------------------------------------------------------------------------------



;Reset 
;
;    LDR R2, =W0ms      ; resets on time
;    SUB R3, R11, R2   
;    ;LDR R3, =W0ms    ; resets off time
;    
;    B Switch_duty_return
;-------------------------------------------------------------------------------------------------------------------------------------------    
LED_On
      
    LDR R3, [R2]
    ORR R3, #0x01    ; turns on bit 0
    STR R3, [R2]     ; affects just PE0
    PUSH {LR}          
    LDR R0, [R1, #ONTIME]       ; sets on time for the blinking, this is the clock's reload value 
    BL SysTick_Wait25ms
    POP {LR}
    BX LR
	
;-------------------------------------------------------------------------------------------------------------------------------------------    
LED_Off
      
    LDR R3, [R2]
    BIC R3, #0x01    ; turns on bit 0
    STR R3, [R2]     ; affects just PE0
    PUSH {LR}          
    LDR R0, [R1, #OFFTIME]       ; sets on time for the blinking, this is the clock's reload value 
    BL SysTick_Wait25ms
    POP {LR}
    BX LR
    
    ;--------------------------------------------------------------------------------------------------------------------------------------------   
    
Check_PF4

    LDR R1, =PF4
    LDR R0, [R1]         ; read just PF4
    AND R0, #0x10         ; clears all bits except bit 4
    ;B New_Switch_duty    ; checks if it is appropriate to change duty cycle for breathing
    CMP R0, #0x10         ; checks if switch (PF4) is pressed
    ;BNE Breathe          ; begins breathing if switch (PF4) is pressed
	;BEQ New_Switch_duty    ; checks if it is appropriate to change duty cycle for breathing

Return
    BX LR


    ALIGN      ; make sure the end of this section is aligned
    END        
----------------------------------------------------------------------

