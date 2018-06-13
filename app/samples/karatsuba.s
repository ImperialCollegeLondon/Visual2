                              ;       Demonstration Assembler Program for Visual2
                              ;       -------------------------------------------
                              ;
                              ;       Acknowledgements:
                              ;       EIE1 ls2617 (Karatsuba code).
                              ;       TJWC (testbench).
                              ;
                              ;       This program is approximately 1000 lines long and
                              ;       performs a recursive Karatsuba multiplication in
                              ;       approx 20,000 instructions, storing data on stack.
                              ;       It exercise most of the VisUAL2 functionality:
                              ;       see Memory and Symbols views after running the code.

                              MOV     R12, #10
                              ;       see TESTVECS below for key to numbers
                              ;       Main test code, R12 indicates test number 0 ... 10
TEST1                         BL      TESTRUNTEST
                              ;       Now R1 contains the test result. 0 => no error
                              MOVS    R1, R1 ; check whether test failed
                              BNE     TESTERROR
TESTSUCCESS                   END     ; SUCCESS! At this endpoint R1 = 0 indicating that the tested
                              ;       subroutine (number in R12) succeeded
                              ;
                              ;
                              ;
TESTERROR                     END     ; ERROR! R1 indicates error code (see below)
                              ;       Error Codes
TEST_SP_ERRNUM                EQU     1 ; subroutine corrupts the stack pointer with unbalanced push/pop
TEST_STKTYPE_ERRNUM           EQU     2 ; subroutine corrupts caller stack locations using wrong stack type
TEST_REGS_ERRNUM              EQU     3 ; subroutine does not preserve regs APCS says it should
TEST_OUTPUT_ERRNUM            EQU     4 ; subroutine output does not match that expcted by the testbench
                              ;
TEST_MAGIC                    EQU     0x90000007 ; recognisable number
                              ;
                              ;----------------------------------------------------------------------------------
                              ;       code to test a single subroutine
                              ;       most of the complexity is testing for possible stack or register use errors
                              ;----------------------------------------------------------------------------------
                              ;
TESTRUNTEST                   ;       on entry R12 must contain the number of the current test
                              LDR     R3, =TESTINFO0
                              LDR     R11, [R3,R12, LSL #2]
                              LDR     R3, =TEST_REGS_TMP
                              STMIA   R3,{R4-R12,R14} ; save everything used by testbench
                              ;       push a recognisable word onto the stack to detect if subroutine corrupts caller stack after subroutine return
                              MOV     R0, #TEST_MAGIC
                              STMFD   SP!, {R0}
                              STMFD   SP!, {R0}
                              LDR     R3, =TEST_SP_TMP ; word reserved for saved SP
                              STR     R13, [R3] ; save SP so we can check it is not corrupted by student
                              LDR     R4, =TESTSVALS
                              LDMIA   R4, {R5-R11} ; load random values into storage registers
                              BL      TEST_SETUP_INPUTS
                              LDR     R4, =TESTVECS
                              BL      TEST_BRANCH_VEC
                              LDR     R4, =TEST_SP_TMP
                              LDR     R4, [R4]
                              CMP     R13, R4
                              MOVNE   R1, #TEST_SP_ERRNUM
                              BNE     TESTRUN_EXIT ; If SP was corrupted terminate this test
                              LDMFD   SP!, {R4}
                              CMP     R4, #TEST_MAGIC
                              LDMFD   SP!, {R4}
                              CMPEQ   R4, #TEST_MAGIC
                              MOVNE   R1, #TEST_STKTYPE_ERRNUM
                              BNE     TESTRUN_EXIT
                              LDR     R3, =TESTSVALS_TMP
                              STMIA   R3, {R5-R11}
                              LDR     R4, =TESTSVALS
                              MOV     R5, #0
TESTRT_LP1                    LDR     R6, [R3,R5,LSL #2]
                              LDR     R7, [R4,R5, LSL #2]
                              CMP     R6, R7
                              MOVNE   R1, #TEST_REGS_ERRNUM
                              BNE     TESTRUN_EXIT ; If storage regs corrupted terminate this test
                              ADD     R5, R5, #1
                              CMP     R5, #7 ; number of storage registers to check (R5-R11)
                              BLT     TESTRT_LP1
                              ;       registers are OK
                              ;       check outputs
                              LDR     R3, =TEST_REGS_TMP
                              LDR     R12, [R3, #32] ; get back testbench R12 from saved copy
                              BL      TEST_CHECK_OUTPUTS
TESTRUN_EXIT                  ;       R1 contains info about test output error
                              LDR     R3, =TEST_REGS_TMP
                              LDMIA   R3, {R4-R12,R15} ; get back testbench regs again and return to main code
                              ;
TEST_BRANCH_VEC               ADD     PC, R4, R12, LSL #2 ; branch to test vector
                              ;
                              ;
                              ;
                              ;
                              ;--------------------------------------------------------------
                              ;       code to set up test inputs from data in TESTINFO
                              ;--------------------------------------------------------------
TEST_SETUP_INPUTS             LDR     R3, =TESTINFO0
                              ADD     R3, R3, R12, LSL #4 ; 2^4 bytes per TESTINFO entry
                              LDR     R2, [R3, #12] ; R2 := TESTTYPE
                              CMP     R2, #TEST_PW
                              BEQ     TEST_SETUP_PW
                              CMP     R2, #TEST_UPW
                              BEQ     TEST_SETUP_UPW
                              CMP     R2, #TEST_NORM
                              BNE     TEST_SETUP_MUL
TEST_SETUP_NORM               ;       default test inputs
                              MOV     R0, #0xB0000007 ; set R0 to some random number on input to subroutine
                              LDR     R1, [R3,#4]
                              LDR     R2, [R3,#8]
                              MOV     R3, #0xC0000003 ; set R3 to some random number on input to subroutine
                              MOV     PC, LR
                              ;
TEST_SETUP_PW                 ADD     R1, R3, #4
                              MOV     R0, #0xB0000007 ; set R0 to some random number on input to subroutine
                              MOV     R3, #0xC0000003 ; set R3 to some random number on input to subroutine
                              ;       LDR R0, [R3]
                              MOV     PC,LR
                              ;
TEST_SETUP_UPW                LDR     R0, [R3]
                              LDR     R1, =TEST_OUTPUTS_TMP
                              MOV     PC, LR
                              ;
TEST_SETUP_MUL                LDR     R0, [R3,#4]
                              LDR     R1, [R3,#8]
                              LDR     R3, [R3]
                              LDR     R2, =TEST_MUL_TMP
                              MOV     PC, LR
                              ;----------------------------------------------------------------------
                              ;       code to test whether outputs are correct given data in TESTINFO
                              ;       at start R12 = test number
                              ;       on exit R1 contains test error hex digit (0-15, 0 = no error)
                              ;------------------------------------------------------------------------
                              ;
TEST_CHECK_OUTPUTS            LDR     R3, =TESTINFO0
                              ADD     R3, R3, R12, LSL #4 ; 2^4 bytes per TESTINFO entry
                              LDR     R4, [R3, #12] ; R4 := TESTTYPE
                              CMP     R4, #TEST_UPW
                              BEQ     TEST_CO_UPW
                              CMP     R4, #TEST_PW
                              BEQ     TEST_CO_NORM
                              CMP     R4, #TEST_NORM
                              BEQ     TEST_CO_NORM
                              BNE     TEST_CO_MUL
TEST_CO_NORM                  ;
                              LDR     R5, [R3]
                              CMP     R0, R5
                              BNE     TEST_CO_ERR
                              BEQ     TEST_CO_OK
                              ;
TEST_CO_UPW                   LDR     R4, =TEST_OUTPUTS_TMP
                              LDR     R0, [R4]
                              LDR     R1, [R3,#4]
                              CMP     R0, R1
                              LDREQ   R0, [R4, #4]
                              LDREQ   R1, [R3,#8]
                              CMPEQ   R0, R1
                              BNE     TEST_CO_ERR ; branch if either of two output words is incorrect
                              B       TEST_CO_OK
                              ;
TEST_CO_MUL                   LDR     R4, =TEST_MUL_TMP
                              LDR     R2, [R3, #12]
                              LDR     R3, [R3]
                              LSL     R3, R3, #1
TEST_CO_MUL1                  LDR     R1, [R4],#4
                              LDR     R0, [R2],#4
                              CMP     R0, R1
                              BNE     TEST_CO_ERR
                              SUBS    R3, R3, #1
                              BNE     TEST_CO_MUL1
                              B       TEST_CO_OK
                              ;
TEST_CO_OK                    MOV     R1, #0
                              MOV     PC, LR

TEST_CO_ERR                   MOV     R1, #TEST_OUTPUT_ERRNUM
                              MOV     PC, LR
                              ;
TEST_MUL_TMP                  FILL    512
                              ;
TESTMUL1A                     DCD     30
TESTMUL1B                     DCD     30
TESTMUL1C                     DCD     900,0
TESTMUL1XA                    DCD     50
TESTMUL1XB                    DCD     50
TESTMUL1XC                    DCD     500, 2
TESTMUL4A                     DCD     240,779,879,509
TESTMUL4B                     DCD     277,808,660,300
TESTMUL4C                     DCD     480,769,724,396,550,866,300,153
TESTMUL16A                    DCD     418,118,456,453,954,589, 96,609
TESTMUL16A1                   DCD     476,391,341, 62, 33,942,385,608
TESTMUL16B                    DCD     783,838,537,512,470,560,251,974
TESTMUL16B1                   DCD     962,738, 13,256,551,139,357,913
TESTMUL16C                    DCD     294, 5,841,889,358,541,330,756
TESTMUL16C1                   DCD     2,715,781,615,250,379,665,950
TESTMUL16C2                   DCD     492,286,194,122,251,382,383,100
TESTMUL16C3                   DCD     930,761, 72,514,758,643,673,555
TESTMUL32A                    DCD     103,208,573,916,684,957,765, 84
TESTMUL32A1                   DCD     98,814,919,654,583,963,317,296
TESTMUL32A2                   DCD     394,271,724,437,270,147,829,857
TESTMUL32A3                   DCD     865,481,489,215,576,839, 57,337
TESTMUL32B                    DCD     34,427,957,823,369,467,923,825
TESTMUL32B1                   DCD     73,453,414,655,639,596,390,997
TESTMUL32B2                   DCD     421,715,861,280,413,919,642,955
TESTMUL32B3                   DCD     378,714, 51,202,467,550,507,258
TESTMUL32C                    DCD     502, 56,920,846,499,822,545, 94
TESTMUL32C1                   DCD     775, 51,508, 16,247,566, 13,336
TESTMUL32C2                   DCD     371,355, 93,650,995,473,249,124
TESTMUL32C3                   DCD     928,385,196,897,765,603,928,508
TESTMUL32C4                   DCD     854,565,281, 16,634,561,135,264
TESTMUL32C5                   DCD     291,122,413,566, 88,922, 23,393
TESTMUL32C6                   DCD     548,638,619,247,328,673,267,787
TESTMUL32C7                   DCD     431,654,618,614,474,996,131, 87
                              ;
                              ;
TEST_OUTPUTS_TMP              FILL    8
TEST_INPUTS_TMP               FILL    4*3
TEST_REGS_TMP                 FILL    13*4
TEST_SP_TMP                   FILL    4
TESTSVALS_TMP                 FILL    4*8
TESTSVALS                     DCD     0x3578,0x4512,0xaf64,0x6812,0x7631,0x7912,0x8300,0x71ff
                              ;
                              ;
                              ;
TESTVECS                      B       PACKTRIPLET ; 0
                              B       UNPACKTRIPLET ; 1
                              B       PACKTWORD ; 2
                              B       UNPACKTWORD ; 3
                              B       MULTRIPLE ; 4
                              B       MULTRIPLE ; 5
                              B       MULTIPLY ; 6
                              B       MULTIPLY
                              B       MULTIPLY
                              B       MULTIPLY
                              B       MULTIPLY
                              ;
                              ;
                              ;
TEST_NORM                     EQU     100 ; Most common type of test: R1, R2 are inputs, R0 is output
TEST_PW                       EQU     101 ; output R0 is output, input R1 is pointer to 2 word input
TEST_UPW                      EQU     102 ; input R0 is input, input R1 is pointer to 2 word output.
TEST_MUL                      EQU     103
                              ;
                              ;
                              ;       each of first 3 words is either an input or the correct value of the output data
                              ;       key to which, and where, is in comments
                              ;       Note that pointer R1 is always an input to subroutine
                              ;       Note that he data pointed to: [R1], [R1+4] may be either input or output.
TESTINFO0                     DCD     523,0x523,0,TEST_NORM ; (O) R0, (I) R1 (PACKTRIPLET)
TESTINFO1                     DCD     0x00000315,315,0,TEST_NORM ; (O) R0, (I) R1, (UNPACKTRIPLET)
TESTINFO2                     DCD     0xF6076377,0x84236887,0x000009,TEST_PW ; (O) R0, (I) [R1], (I) [R1+4] (PACKTWORD)
TESTINFO3                     DCD     0x22CC912F,0x39402303,0x000001,TEST_UPW ; (I) R0, (O) [R1], (O) [R1+4] (UNPACKTWORD)
TESTINFO4                     DCD     0x0002E268,0x00000088,0x000002A9,TEST_NORM ; (O) R0, (I) R1, (I) R2. (MULTRIPLE)
TESTINFO5                     DCD     0x0002E268,0x000002A9,0x00000088,TEST_NORM ; (O) R0, (I) R1, (I) R2. (MULTRIPLE)
TESTINFO6                     DCD     1, TESTMUL1A,TESTMUL1B,TESTMUL1C
TESTINFO7                     DCD     1, TESTMUL1XA,TESTMUL1XB,TESTMUL1XC
TESTINFO8                     DCD     4, TESTMUL4A,TESTMUL4B,TESTMUL4C
TESTINFO9                     DCD     16, TESTMUL16A,TESTMUL16B,TESTMUL16C
TESTINFO10                    DCD     32, TESTMUL32A,TESTMUL32B, TESTMUL32C
                              ;
PACKTRIPLET                   AND     R2,R1,#0xF00 ;8C
                              AND     R3,R1,#0x0F0 ;B
                              LSR     R2,R2,#5
                              ADD     R2,R2,R2,LSR #2 ;8C+2C
                              ADD     R2,R2,R3,LSR #4 ;10C+B
                              ;get    A+10(B+10C)
                              AND     R3,R1,#0x00F ;A
                              ADD     R3,R3,R2,LSL #1 ;A+2(8C+2C)
                              ADD     R0,R3,R2,LSL #3 ;A+10(8C+2C)
                              ;end
                              MOV     PC,LR
                              ;
                              ;====UNPACKTRIPLET====
                              ;In     R1: LS10 => TCD; MS22 => any
                              ;Out    R0: LS12 => 3BCD(C,B,A); MS20 => all0
                              ;Temp   R2,R3
                              ;
UNPACKTRIPLET                 LSL     R1,R1,#22 ;{!!!!!!!!!! NOT FULLY OPTIMISED)
                              LSR     R1,R1,#22
                              ;get    C by *1/100 est *0000001010 [2 line less than OP1 version]
                              MOV     R2,R1,LSR #7 ;*0.0000001
                              ADD     R2,R2,R1,LSR #9 ;*(0.0000001+0.000000001)
                              SUB     R1,R1,R2,LSL #6
                              SUB     R1,R1,R2,LSL #5
                              SUB     R1,R1,R2,LSL #2 ;R1=input-100*R2
                              CMP     R1,#100 ; check rounding error
UNPACKTRIPLET_L1              SUBHS   R1,R1,#100
                              ADDHS   R2,R2,#1
                              CMP     R1,#100 ; ***May required, not sure
                              BHS     UNPACKTRIPLET_L1
                              ;
                              MOV     R0,R2,LSL #8 ;put C in output
                              ;
                              ;
                              ;get    B&A by R1/10 est *0001100110
                              MOV     R2,R1,LSR #4
                              ADD     R2,R2,R1,LSR #5
                              ADD     R2,R2,R1,LSR #8
                              ADD     R2,R2,R1,LSR #9
                              SUB     R1,R1,R2,LSL #3
                              SUB     R1,R1,R2,LSL #1 ;R1=input-100*C-10R2
                              ;
                              CMP     R1,#10 ; check rounding error
UNPACKTRIPLET_L2              SUBHS   R1,R1,#10
                              ADDHS   R2,R2,#1
                              CMP     R1,#10 ; ***May required, not sure
                              BHS     UNPACKTRIPLET_L2
                              ;
                              ORR     R0,R0,R2,LSL #4 ;put B in output
                              ORR     R0,R0,R1 ;put A in output
                              ;end
                              MOV     PC,LR
                              ;
                              ;====PACKTWORD====
                              ;       not optimised
                              ;       USE PACKTRIPLET, UNROLL if assist on cycle
                              ;In     R1: Address of the 9BCD
                              ;Out    R0: TWORD
                              ;Temp   R2,R3,R4,R12
                              ;
PACKTWORD                     STMFD   R13!,{R4,LR}
                              LDR     R12,[R1,#4]
                              LDR     R1,[R1]
                              ;1st    TCD
                              BL      PACKTRIPLET ;call subroutine,R1 is unchanged
                              mov     R4,R0
                              ;2nd    TCD
                              LSR     R1,R1,#12
                              BL      PACKTRIPLET
                              ORR     R4,R4,R0,LSL #11
                              ;3nd    TCD (if UNROLL, caulcate it first, use 1 less reg and some cycle)
                              LSR     R1,R1,#12
                              ORR     R1,R1,R12,LSL #8
                              BL      PACKTRIPLET
                              ORR     R0,R4,R0,LSL #22
                              ;end
                              LDMFD   R13!,{R4,PC}
                              ;
                              ;====UNPACKTWORD====
                              ;       not optimised
                              ;       USE UNPACKTRIPLET, UNROLL if assist on cycle
                              ;In     R1: Address of the 9 BCD for output
                              ;       R0: TWORD
                              ;Out    NONE
                              ;Temp   r1,R2,R3,R4,R12
                              ;
UNPACKTWORD                   STMFD   R13!,{R4,R5,LR}
                              MOV     R12,R1
                              MOV     R4,R0
                              ;1st    3BCD
                              MOV     R1,R4
                              BL      UNPACKTRIPLET
                              MOV     R5,R0
                              ;2nd    3BCD
                              MOV     R1,R4,LSR #11
                              BL      UNPACKTRIPLET
                              ORR     R5,R5,R0,LSL #12
                              ;3rd    3BCD
                              MOV     R1,R4,LSR #22
                              BL      UNPACKTRIPLET
                              ORR     R5,R5,R0,LSL #24
                              STR     R5,[R12],#4
                              MOV     R5,R0,LSR #8
                              STR     R5,[R12]
                              ;store
                              LDMFD   R13!,{R4,R5,PC}
                              ;
                              ;====MULTRIPLE====
                              ;In     R1: 1TCD (STRCILY 10 bit)
                              ;       R2: 1TCD (May exceed 10 bit => binary, not TCD)
                              ;Out    R0:
                              ;Temp   R3,
                              ;
MULTRIPLE                     MOV     R0,#0
                              ;MUL
                              LSLS    R1,R1,#23 ;mov 1 for caucaltion & 22 for set up
                              ADDCS   R0,R0,R2,LSL #9
                              ADDMI   R0,R0,R2,LSL #8
                              LSLS    R1,R1,#2
                              ADDCS   R0,R0,R2,LSL #7
                              ADDMI   R0,R0,R2,LSL #6
                              LSLS    R1,R1,#2
                              ADDCS   R0,R0,R2,LSL #5
                              ADDMI   R0,R0,R2,LSL #4
                              LSLS    R1,R1,#2
                              ADDCS   R0,R0,R2,LSL #3
                              ADDMI   R0,R0,R2,LSL #2
                              LSLS    R1,R1,#2
                              ADDCS   R0,R0,R2,LSL #1
                              ADDMI   R0,R0,R2,LSL #0
                              ;split(METHOD_Y) max error:998000/1024 => 4 if loop
                              ;
                              ;split(METHOD_X) /1000 est *000000000100000110001
                              ;MOV    R1,R0,LSL #22 ;R1 => dec part from est, use to check rounding error. DOES not work (was ADC)
                              MOV     R2,R0,LSR #10 ;R2 => int part from est
                              ;ADDS   R1,R1,R0, LSL #28 ;check if dec overflow
                              ADD     R2,R2,R0,LSR #16 ;add overflow to carry
                              ;ADDS   R1,R1,R0, LSL #29
                              ADD     R2,R2,R0,LSR #17 ;*00000000010000011000
                              ;       R0*=R0-1000R2
                              SUB     R0,R0,R2,LSL #10
                              ADD     R0,R0,R2,LSL #4
                              ADD     R0,R0,R2,LSL #3
                              ;       check R0 less than 1000
                              CMP     R0,#1000 ; check rounding error
                              SUBHS   R0,R0,#1000
                              ADDHS   R2,R2,#1
                              CMP     R0,#1000 ; check rounding error
MULTRIPLE_L1                  SUBHS   R0,R0,#1000
                              ADDHS   R2,R2,#1
                              CMP     R0,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTRIPLE_L1 ;very unlikely to trigger, use BL reduce cycle by 1, triger in 999*999
                              ;gen    output
                              ORR     R0,R0,R2,LSL #11
                              ;
                              ;end
                              mov     PC,LR
MULTIPLY                      ;MULTIPLY wapper�� jump to T2? use as wapper?
                              CMP     R3,#1
                              BEQ     MULTIPLY_EXP

                              ;LDR    R12,=bufferMULT ; FA ;USE R13 TO OPT (consider stack allocation with R0123 & NOTE:FA=>FD)
                              STMFD   R13!,{R4,R5,R6,R7,R8,R9,R10,R11,LR} ; APCS compliant, all register can be used for scratch registers
                              ;
                              ;
                              BL      MULTIPLY_T2 ; no need to be APCS compliant
                              ;
                              LDMFD   R13!,{R4,R5,R6,R7,R8,R9,R10,R11,PC}
                              ;       end of code for MULTIPLY
                              ;
                              ;
MULTIPLY_EXP                  STMFD   R13!,{R0,R1,R2,R3,LR} ;strach not need to save
                              ;
                              ;MUL
                              LDR     R0,[R0]
                              LDR     R1,[R1]
                              ;
                              MOV     R3,#0
                              LSLS    R0,R0,#23 ;mov 1 for caucaltion & 22 for set up
                              ADDCS   R3,R3,R1,LSL #9
                              ADDMI   R3,R3,R1,LSL #8
                              LSLS    R0,R0,#2
                              ADDCS   R3,R3,R1,LSL #7
                              ADDMI   R3,R3,R1,LSL #6
                              LSLS    R0,R0,#2
                              ADDCS   R3,R3,R1,LSL #5
                              ADDMI   R3,R3,R1,LSL #4
                              LSLS    R0,R0,#2
                              ADDCS   R3,R3,R1,LSL #3
                              ADDMI   R3,R3,R1,LSL #2
                              LSLS    R0,R0,#2
                              ADDCS   R3,R3,R1,LSL #1
                              ADDMI   R3,R3,R1,LSL #0
                              ;
                              MOV     R0,R3,LSR #10 ;R0 => int part from est
                              ADD     R0,R0,R3,LSR #16 ;add overflow to carry
                              ADD     R0,R0,R3,LSR #17 ;*00000000010000011000
                              SUB     R3,R3,R0,LSL #10
                              ADD     R3,R3,R0,LSL #4
                              ADD     R3,R3,R0,LSL #3
                              CMP     R3,#1000 ; check rounding error
                              SUBHS   R3,R3,#1000
                              ADDHS   R0,R0,#1
                              CMP     R3,#1000 ; check rounding error caused by rounding error from decimal
MULTIPLY_EXP_L1               SUBHS   R3,R3,#1000
                              ADDHS   R0,R0,#1
                              CMP     R3,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTIPLY_EXP_L1
                              ;
                              STR     R3,[R2]
                              STR     R0,[R2,#4]
                              ;
                              ;end
                              LDMFD   R13!,{R0,R1,R2,R3,PC}
                              ;
                              ;
                              ;
                              ;
MULTIPLY_T2                   
                              CMP     R3,#2 ;CMP M,#2 ;BE 4 TO OPT
                              BLS     MULTIPLY_T1
                              STMFD   R13!,{R0,R1,R2,R3,r12,LR} ;str current R0,R1,R2,R3
                              ;
                              ;make   output as A1B1:A0B0
                              ;R3=0.5M
                              LSR     R3,R3,#1 ;get M/2
                              BL      MULTIPLY_T2 ;get A0B0 at LSW
                              ADD     R0,R0,R3,LSL #2
                              ADD     R1,R1,R3,LSL #2
                              ADD     R2,R2,R3,LSL #3 ; (*4*1) muti 2 as output is twice size of input
                              BL      MULTIPLY_T2 ;get A1B1 at MSW
                              ;
                              ;caculate (A0-A1)&(B1-B0) /CAN BE OPTMISED (use base 2^11)
                              ;X      CAN BE OPTMISED: add & mut less mome opt
                              ;X      set R0,R1,R2
                              ;X      R3,R12 USE AS TEMP REG FOR R0,R1
                              ;X      sub 2M , add 2M on exist, there is no stack operation in between

                              ;       R12:0000 0000 0000 0000 00CC CCCC CC00 0AON
                              ;       A=0 => IS B A=1 => IS A (ONLY FOR SUB4)
                              ;       N=0 => POSTIVE N=1 => NEGATIVE
                              ;       C=> COUNTER
                              ;       O =>dont care, overflow of N

                              CMP     R3,#4 ; to ADD2 if 0.5M=2, ADD4 if 0.5M=4, ADD8 if 0.5M>4
                              ;BHI    MULTIPLY_T2_Z1_SUB8 ; FOR OPTIMISE
                              ;BEQ    MULTIPLY_T2_Z1_SUB4
                              BGE     MULTIPLY_T2_Z1_SUB4
                              ;
MULTIPLY_T2_Z1_SUB2           
MULTIPLY_T2_Z1_SUB2_B         ADD     R1,R1,#8 ;+2
                              LDMDB   R1!,{R4,R5,R6,R7}
                              CMP     R7, R5
                              CMPEQ   R6,R4
                              ;B0=B1
                              ;TO     SPEICAL BRANCH, SKIP MULTIPLY_T2_Z1_MUL (0 ANYWAY)
                              ;B1<B0  negative
                              BCC     MULTIPLY_T2_Z1_SUB2_B_NEG
                              ;B1>B0  postive
                              MOV     R12,#0X0
                              SUBS    R6,R6,R4
                              ADDMI   R6,R6,#1000
                              SBC     R7,R7,R5
                              ;
MULTIPLY_T2_Z1_SUB2_A         ADD     R0,R0,#8 ;+2
                              LDMDB   R0!,{R8,R9,R10,R11}
                              CMP     R9, R11
                              CMPEQ   R8, R10
                              ;A0=A1
                              ;TO     SPEICAL BRANCH, SKIP MULTIPLY_T2_Z1_MUL (0 ANYWAY)
                              ;A0<A1  negative
                              BCC     MULTIPLY_T2_Z1_SUB2_A_NEG
                              ;A0>A1  postive
                              SUBS    R8,R8,R10
                              ADDMI   R8,R8,#1000
                              SBC     R9,R9,R11
                              ;
                              ;STORE
MULTIPLY_T2_Z1_SUB2_STR       STMDB   R13,{R6,R7,R8,R9}
                              SUB     R0,R13,#8 ;2*4
                              SUB     R1,R13,#16 ;2*4*2
                              SUB     R13,R13,#32 ;2*4*2+4*4 ;space for (A0-A1)*(B1-B0)
                              MOV     R2,R13
                              ;
MULTIPLY_T2_Z1_MUL            BL      MULTIPLY_T2 ; SPECIAL BRANCH TO MULTIPLY_T1_ADD
                              ;
                              TST     R12,#0X1 ;MASK FOR N
                              BNE     MULTIPLY_T2_Z1_ADD_NEG_WAPPER ; FOR OPTIMISE
                              ;
MULTIPLY_T2_Z1_ADD_POS_WAPPER ADD     R0,R13,R3,LSL #4 ;(1M+2*0.5M)*4=16*0.5M
                              LDR     R0,[R0,#8] ; R0 is R2 in parents branch,
                              ADD     R12,R0,R3,LSL #3
                              ;       R0=pointer of parent R2 A0B0, R12=pointer of A2B2, R2=pointer of current R2 (A1+A0)*(B1+B0)
                              ;       place result to back to R2
                              ;       no rounding
                              ;
MULTIPLY_T2_Z1_ADD_POS        LDMIA   R12!,{R8,R9,R10,R11}
                              LDMIA   R0!, {R4,R5,R6,R7}
                              ;
                              ;
                              ADD     R4,R4,R8
                              ADD     R5,R5,R9
                              ADD     R6,R6,R10
                              ADD     R7,R7,R11
                              ;
                              LDMIA   R2,{R8,R9,R10,R11}
                              ADD     R4,R4,R8
                              ADD     R5,R5,R9
                              ADD     R6,R6,R10
                              ADD     R7,R7,R11
                              ;
                              STMIA   R2!,{R4,R5,R6,R7}
                              CMP     R1,R2
                              BNE     MULTIPLY_T2_Z1_ADD_POS
                              ;
MULTIPLY_T2_Z1_FINAL_W        SUB     R2,R2,R3,LSL #3
                              SUB     R0,R0,R3,LSL #2
                              MOV     R7,#0
                              ;       R0=pointer for A1B1, R2=pointer for not rounded Z1, R12 not used
                              ;
MULTIPLY_T2_Z1_FINAL          LDMIA   R2!,{R8,R9,R10,R11}
                              ADD     R8,R8,R7 ;from previous carry
                              LDMIA   R0,{R4,R5,R6,R7}
                              ;
                              ADD     R9,R5,R9
                              ADD     R10,R6,R10
                              ADD     R11,R7,R11
                              ADDS    R8,R4,R8 ;set flag, if negative
                              ;
                              ;rounding, may be negative
                              ;
                              ;ROUNDING R8 (rounding part in MULTIPLY_T1, CHANGE R11 TO R4)
                              BMI     MULTIPLY_T2_Z1_FINAL_R8_N
MULTIPLY_T2_Z1_FINAL_R8       MOV     R4,R8,LSR #10 ;R4 => int part from est
                              ADD     R4,R4,R8,LSR #16 ;add overflow to carry
                              ADD     R4,R4,R8,LSR #17 ;*00000000010000011000
                              SUB     R8,R8,R4,LSL #10
                              ADD     R8,R8,R4,LSL #4
                              ADD     R8,R8,R4,LSL #3
                              CMP     R8,#1000 ; check rounding error
                              SUBHS   R8,R8,#1000
                              ADDHS   R4,R4,#1
                              CMP     R8,#1000 ; check rounding error caused by rounding error from decimal
MULTIPLY_T2_Z1_FINAL_R8_L     SUBHS   R8,R8,#1000
                              ADDHS   R4,R4,#1
                              CMP     R8,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTIPLY_T2_Z1_FINAL_R8_L
                              ADDS    R9,R9,R4
                              ;
                              ;ROUNDING R9 (rounding part in MULTIPLY_T1, CHANGE R11 TO R4)
                              BMI     MULTIPLY_T2_Z1_FINAL_R9_N
MULTIPLY_T2_Z1_FINAL_R9       MOV     R4,R9,LSR #10 ;R4 => int part from est
                              ADD     R4,R4,R9,LSR #16 ;add overflow to carry
                              ADD     R4,R4,R9,LSR #17 ;*00000000010000011000
                              SUB     R9,R9,R4,LSL #10
                              ADD     R9,R9,R4,LSL #4
                              ADD     R9,R9,R4,LSL #3
                              CMP     R9,#1000 ; check rounding error
                              SUBHS   R9,R9,#1000
                              ADDHS   R4,R4,#1
                              CMP     R9,#1000 ; check rounding error caused by rounding error from decimal
MULTIPLY_T2_Z1_FINAL_R9_L     SUBHS   R9,R9,#1000
                              ADDHS   R4,R4,#1
                              CMP     R9,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTIPLY_T2_Z1_FINAL_R9_L
                              ADDS    R10,R10,R4
                              ;
                              ;ROUNDING R10
                              BMI     MULTIPLY_T2_Z1_FINAL_R10_N
MULTIPLY_T2_Z1_FINAL_R10      MOV     R4,R10,LSR #10 ;R4 => int part from est
                              ADD     R4,R4,R10,LSR #16 ;add overflow to carry
                              ADD     R4,R4,R10,LSR #17 ;*00000000010000011000
                              SUB     R10,R10,R4,LSL #10
                              ADD     R10,R10,R4,LSL #4
                              ADD     R10,R10,R4,LSL #3
                              CMP     R10,#1000 ; check rounding error
                              SUBHS   R10,R10,#1000
                              ADDHS   R4,R4,#1
                              CMP     R10,#1000 ; check rounding error caused by rounding error from decimal
MULTIPLY_T2_Z1_FINAL_R10_L    SUBHS   R10,R10,#1000
                              ADDHS   R4,R4,#1
                              CMP     R10,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTIPLY_T2_Z1_FINAL_R10_L
                              ADDS    R11,R11,R4
                              ;
                              ;ROUNDING R11
                              BMI     MULTIPLY_T2_Z1_FINAL_R11_N
MULTIPLY_T2_Z1_FINAL_R11      MOV     R7,R11,LSR #10 ;R4 => int part from est
                              ADD     R7,R7,R11,LSR #16 ;add overflow to carry
                              ADD     R7,R7,R11,LSR #17 ;*00000000010000011000
                              SUB     R11,R11,R7,LSL #10
                              ADD     R11,R11,R7,LSL #4
                              ADD     R11,R11,R7,LSL #3
                              CMP     R11,#1000 ; check rounding error
                              SUBHS   R11,R11,#1000
                              ADDHS   R7,R7,#1
                              CMP     R11,#1000 ; check rounding error caused by rounding error from decimal
MULTIPLY_T2_Z1_FINAL_R11_L    SUBHS   R11,R11,#1000
                              ADDHS   R7,R7,#1
                              CMP     R11,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTIPLY_T2_Z1_FINAL_R11_L
                              ;
                              ;
MULTIPLY_T2_Z1_FINAL_R        CMP     R1,R2
                              STMIA   R0!,{R8,R9,R10,R11}
                              BNE     MULTIPLY_T2_Z1_FINAL
                              ;exit   loop if is last one
                              ;
                              LSL     R5,R3,#4 ;temp R5, as "ADD R13,R13,R3, LSL #4" is not vaild ; can be optmised
                              ADD     R13,R13,R5
                              CMP     R7,#0
                              LDMFDeq R13!,{R0,R1,R2,R3,r12,PC}
                              ;
MULTIPLY_T2_Z1_FINAL_OVERFLOW LDR     R4,[R0] ;get next MS TRIPLET OF CURRENT OUTPUT (start pointer for A2B2)
                              ADD     R4,R4,R7
                              SUBS    R5,R4, #1000 ; ANOTHER TEMP REG R5
                              ;
                              ;no     further overflow
                              STRlt   R4,[R0]
                              LDMFDlt R13!,{R0,R1,R2,R3,r12,PC}
                              ;
                              ;furtheroverflow
                              MOV     R7,#1
                              STR     R5,[R0],#4
                              B       MULTIPLY_T2_Z1_FINAL_OVERFLOW
                              ;
                              end     ; END of MULTIPLY, this line should not executed
                              ;
                              ;       ============ BELOW ARE FUNCTIONS
MULTIPLY_T2_Z1_SUB2_B_NEG     MOV     R12,#0X1
                              SUBS    R6,R4,R6
                              ADDMI   R6,R6,#1000
                              SBC     R7,R5,R7
                              B       MULTIPLY_T2_Z1_SUB2_A

MULTIPLY_T2_Z1_SUB2_A_NEG     add     R12,R12,#0X1
                              SUBS    R8,R10,R8
                              ADDMI   R8,R8,#1000
                              SBC     R9,R11,R9
                              B       MULTIPLY_T2_Z1_SUB2_STR
                              ;
                              ;
MULTIPLY_T2_Z1_ADD_NEG_WAPPER ADD     R0,R13,R3,LSL #4 ;(1M+2*0.5M)*4=16*0.5M
                              LDR     R0,[R0,#8] ; R0 is R2 in parents branch,
                              ADD     R12,R0,R3,LSL #3
                              MOV     R7,#0
                              ;       R0=pointer of parent R2 A0B0, R12=pointer of A2B2, R2=pointer of current R2 (A1-A0)*(B1-B0)
                              ;       place result to R0+R3 (LSL #2)
                              ;
MULTIPLY_T2_Z1_ADD_NEG        LDMIA   R12!,{R8,R9,R10,R11}
                              LDMIA   R0!, {R4,R5,R6,R7}
                              ;
                              ;
                              ADD     R4,R4,R8
                              ADD     R5,R5,R9
                              ADD     R6,R6,R10
                              ADD     R7,R7,R11
                              ;
                              LDMIA   R2,{R8,R9,R10,R11}
                              SUB     R4,R4,R8
                              SUB     R5,R5,R9
                              SUB     R6,R6,R10
                              SUB     R7,R7,R11
                              ;
                              STMIA   R2!, {R4,R5,R6,R7}
                              CMP     R1,R2
                              BNE     MULTIPLY_T2_Z1_ADD_NEG
                              B       MULTIPLY_T2_Z1_FINAL_W
                              ;
                              ;need   opt, use shift & est, see example:789741123456*852963410520, cause too much loop
MULTIPLY_T2_Z1_FINAL_R8_N     RSB     R8,R8,#0
                              MOV     R4,R8,LSR #10 ;R4 => BORROW
                              ADD     R4,R4,R8,LSR #16
                              ADD     R4,R4,R8,LSR #17
                              SUB     R8,R8,R4,LSL #10
                              ADD     R8,R8,R4,LSL #4
                              ADDS    R8,R8,R4,LSL #3
                              ADDGT   R4,R4,#1
                              SUBSGT  R8,R8,#1000 ;CHECK
MULTIPLY_T2_Z1_FINAL_R8_N_L   ADDGT   R4,R4,#1
                              SUBSGT  R8,R8,#1000 ;CHECK
                              BGT     MULTIPLY_T2_Z1_FINAL_R8_N_L
                              ;
                              RSB     R8,R8,#0
                              SUBS    R9,R9,R4
                              BGE     MULTIPLY_T2_Z1_FINAL_R9
                              ;
MULTIPLY_T2_Z1_FINAL_R9_N     RSB     R9,R9,#0
                              MOV     R4,R9,LSR #10 ;R4 => BORROW
                              ADD     R4,R4,R9,LSR #16
                              ADD     R4,R4,R9,LSR #17
                              SUB     R9,R9,R4,LSL #10
                              ADD     R9,R9,R4,LSL #4
                              ADDS    R9,R9,R4,LSL #3
                              ADDGT   R4,R4,#1
                              SUBSGT  R9,R9,#1000 ;CHECK
MULTIPLY_T2_Z1_FINAL_R9_N_L   ADDGT   R4,R4,#1
                              SUBSGT  R9,R9,#1000 ;CHECK
                              BGT     MULTIPLY_T2_Z1_FINAL_R9_N_L
                              ;
                              RSB     R9,R9,#0
                              SUBS    R10,R10,R4
                              BGE     MULTIPLY_T2_Z1_FINAL_R10
                              ;
MULTIPLY_T2_Z1_FINAL_R10_N    RSB     R10,R10,#0
                              MOV     R4,R10,LSR #10 ;R4 => BORROW
                              ADD     R4,R4,R10,LSR #16
                              ADD     R4,R4,R10,LSR #17
                              SUB     R10,R10,R4,LSL #10
                              ADD     R10,R10,R4,LSL #4
                              ADDS    R10,R10,R4,LSL #3
                              ADDGT   R4,R4,#1
                              SUBSGT  R10,R10,#1000 ;CHECK
MULTIPLY_T2_Z1_FINAL_R10_N_L  ADDGT   R4,R4,#1
                              SUBSGT  R10,R10,#1000 ;CHECK
                              BGT     MULTIPLY_T2_Z1_FINAL_R10_N_L
                              ;
                              RSB     R10,R10,#0
                              SUBS    R11,R11,R4
                              BGE     MULTIPLY_T2_Z1_FINAL_R11
                              ;
MULTIPLY_T2_Z1_FINAL_R11_N    RSB     R11,R11,#0
                              MOV     R7,R11,LSR #10 ;R7 => BORROW
                              ADD     R7,R7,R11,LSR #16
                              ADD     R7,R7,R11,LSR #17
                              SUB     R11,R11,R7,LSL #10
                              ADD     R11,R11,R7,LSL #4
                              ADDS    R11,R11,R7,LSL #3
                              ADDGT   R7,R7,#1
                              SUBSGT  R11,R11,#1000 ;CHECK
MULTIPLY_T2_Z1_FINAL_R11_N_L  ADDGT   R7,R7,#1
                              SUBSGT  R11,R11,#1000 ;CHECK
                              BGT     MULTIPLY_T2_Z1_FINAL_R11_N_L
                              ;
                              RSB     R7,R7,#0
                              RSB     R11,R11,#0
                              B       MULTIPLY_T2_Z1_FINAL_R
                              ;
                              ;
                              ;MULTIPLY_T2_Z1_SUB8
MULTIPLY_T2_Z1_SUB4           ;R2     => pointer for of B1 & A1
                              ;R0,R1  => pointer for of B0 & A0
                              ;R12[13..4] => for countdown, already shifted by 2 0.5M=R12[13..6]
                              ;can    be OPT (STR 1 REG, use it for pointer of A2B2)
                              ;
                              ;
                              ;
MULTIPLY_T2_Z1_SUB4_A_W       ADD     R2,R0,R3,LSL #2
                              MOV     R12,#0X4 ;set A=1,N=0
                              ADD     R12,R12,R3,LSL #6 ;set A=1,N=0,C=0.5M, as 0.5M in range of 0 - 32
                              SUB     R13,R13,R3,LSL #2 ;make space for |A0-A1|
                              ;
MULTIPLY_T2_Z1_SUB4_A         LDMDB   R0!,{R4,R5,R6,R7}
                              LDMDB   R2!,{R8,R9,R10,R11}
                              ;
                              CMP     R7,R11
                              CMPEQ   R6,R10
                              CMPEQ   R5,R9
                              CMPEQ   R4,R8
                              SUB     R12,R12,#0X100 ;counter-4
                              ;
                              ;B1<B0  negative
                              ADDCC   R12,R12,#0X1
                              BCC     MULTIPLY_T2_Z1_SUB4_1SUB0_W
                              ;B1>B0  postive
                              BHI     MULTIPLY_T2_Z1_SUB4_0SUB1_W
                              ;
                              ;B0=B1  check if is last one
                              TST     R12,#0X3FC0 ;get counter & test
                              BNE     MULTIPLY_T2_Z1_SUB4_A
                              ;if     is last, still equal => (A0-A1)(B1-B0)=0 => OPT
                              ;       temp soulstion
                              B       MULTIPLY_T2_Z1_SUB4_0SUB1_W
                              ;
MULTIPLY_T2_Z1_SUB4_B_W       ADD     R2,R1,R3,LSL #2
                              AND     R12,R12,#0X1 ;only copy N, A=0, , reset counter,
                              ADD     R12,R12,R3,LSL #6 ;A=0, C=0.5M, N=previous
                              SUB     R13,R13,R3,LSL #3 ;make space for |B0-B1|, was at MSW of |A0-A1|
                              ;
MULTIPLY_T2_Z1_SUB4_B         LDMDB   R1!,{R4,R5,R6,R7}
                              LDMDB   R2!,{R8,R9,R10,R11}
                              ;
                              CMP     R11,R7
                              CMPEQ   R10,R6
                              CMPEQ   R9,R5
                              CMPEQ   R8,R4
                              SUB     R12,R12,#0X100 ;counter-4
                              ;
                              ;B1<B0  negative
                              ADDCC   R12,R12,#0X1
                              BCC     MULTIPLY_T2_Z1_SUB4_0SUB1_W
                              ;B1>B0  postive
                              BHI     MULTIPLY_T2_Z1_SUB4_1SUB0_W
                              ;
                              ;B0=B1  check if is last one
                              TST     R12,#0X3FC0 ;get counter & test
                              BNE     MULTIPLY_T2_Z1_SUB4_B
                              ;if     is last, still equal => (A0-A1)(B1-B0)=0 => OPT
                              ;       temp soulstion
                              B       MULTIPLY_T2_Z1_SUB4_0SUB1_W
                              ;
                              ;
                              ;in     MULTIPLY_T2_Z1_SUB4_0SUB1 & _1SUB0,
                              ;R12    => R/W counter & R A
                              ;R2     => pointer for A1 & B1 (start LSW)
                              ;R0     => pointer for A0 & B0 (start LSW), can overwrite R0, as cacl A before B
                              ;R13    => pointer for output (start LSW)
MULTIPLY_T2_Z1_SUB4_0SUB1_W   TST     R12,#0X4 ;get A flag
                              SUBne   R0,R0,R12,LSR #4 ;A=1 for A, reset pointer
                              SUBeq   R0,R1,R12,LSR #4 ;A=0 for B, reset pointer
                              ADD     R2,R0,R3,LSL #2
                              AND     r12,R12,#0x5 ;reset counter
                              ADD     R12,R12,R3, LSL #6 ;set counter
                              mov     r7,r11 ;FORCE SET CARRY FLAG
                              ;
MULTIPLY_T2_Z1_SUB4_0SUB1     cmp     r7,r11
                              LDMIA   R0!,{R4,R5,R6,R7}
                              LDMIA   R2!,{R8,R9,R10,R11}
                              ;
                              SBCS    R4,R4,R8
                              ADDMI   R4,R4,#1000
                              SBCS    R5,R5,R9
                              ADDMI   R5,R5,#1000
                              SBCS    R6,R6,R10
                              ADDMI   R6,R6,#1000
                              SBCS    R8,R7,R11 ;preservie R7, for cmp r7,r11
                              ADDMI   R8,R8,#1000
                              STMIA   R13!,{R4,R5,R6,R8}
                              ;
                              SUB     R12,R12,#0X100
                              TST     R12,#0X3FC0
                              BNE     MULTIPLY_T2_Z1_SUB4_0SUB1
                              ;
                              TST     R12,#0X4
                              BNE     MULTIPLY_T2_Z1_SUB4_B_W ;A=1, form A, cal B next
                              ;       A=0, form B, do MULTIPLY_T2_Z1_MUL next
                              MOV     R0,R13 ;pointer at LSW of |A1-A0|
                              SUB     R1,R13,R3,LSL #2 ;move pointer to LSW of |B0-B1|
                              SUB     R2,R1,R3,LSL #3 ;move pointer to LSW of Z1
                              MOV     R13,R2
                              B       MULTIPLY_T2_Z1_MUL
                              ;
MULTIPLY_T2_Z1_SUB4_1SUB0_W   TST     R12,#0X4 ;get A flag
                              SUBne   R0,R0,R12,LSR #4 ;A=1 for A, reset pointer
                              SUBeq   R0,R1,R12,LSR #4 ;A=0 for B, reset pointer
                              ADD     R2,R0,R3,LSL #2
                              AND     r12,R12,#0x5 ;reset counter
                              ADD     R12,R12,R3, LSL #6 ;set counter
                              mov     r7,r11 ;FORCE SET CARRY FLAG
                              ;
MULTIPLY_T2_Z1_SUB4_1SUB0     cmp     r11,r7
                              LDMIA   R0!,{R4,R5,R6,R7}
                              LDMIA   R2!,{R8,R9,R10,R11}
                              ;
                              SBCS    R4,R8,R4
                              ADDMI   R4,R4,#1000
                              SBCS    R5,R9,R5
                              ADDMI   R5,R5,#1000
                              SBCS    R6,R10,R6
                              ADDMI   R6,R6,#1000
                              SBCS    R8,R11,R7
                              ADDMI   R8,R8,#1000
                              STMIA   R13!,{R4,R5,R6,R8}
                              ;
                              SUB     R12,R12,#0X100
                              TST     R12,#0X3FC0
                              BNE     MULTIPLY_T2_Z1_SUB4_1SUB0
                              ;
                              TST     R12,#0X4
                              BNE     MULTIPLY_T2_Z1_SUB4_B_W ;A=1 form A, cal B next
                              ;       A=0 form A, do MULTIPLY_T2_Z1_MUL next
                              MOV     R0,R13 ;pointer at LSW of |A1-A0|
                              SUB     R1,R13,R3,LSL #2 ;move pointer to LSW of |B0-B1|
                              SUB     R2,R1,R3,LSL #3 ;move pointer to LSW of Z1
                              MOV     R13,R2
                              B       MULTIPLY_T2_Z1_MUL
                              ;
                              ;
MULTIPLY_T1                   ;       R5|R4
                              ;       X R7|R6
                              ;result:R11|R10|R9|R8
                              ;R11:   TEMP FOR SHIFT & ROUNDING
                              LDR     R4,[R0]
                              LDR     R5,[R0,#4]
                              LDR     R6,[R1]
                              LDR     R7,[R1,#4]
                              ;
                              ;FIRST  R8=R4*R6
                              MOV     R8,#0
                              ;
                              LSLS    R11,R4,#23 ;mov 1 for caucaltion & 22 for set up
                              ADDCS   R8,R8,R6,LSL #9
                              ADDMI   R8,R8,R6,LSL #8
                              LSLS    R11,R11,#2
                              ADDCS   R8,R8,R6,LSL #7
                              ADDMI   R8,R8,R6,LSL #6
                              LSLS    R11,R11,#2
                              ADDCS   R8,R8,R6,LSL #5
                              ADDMI   R8,R8,R6,LSL #4
                              LSLS    R11,R11,#2
                              ADDCS   R8,R8,R6,LSL #3
                              ADDMI   R8,R8,R6,LSL #2
                              LSLS    R11,R11,#2
                              ADDCS   R8,R8,R6,LSL #1
                              ADDMI   R8,R8,R6,LSL #0
                              ;
                              ;LAST   R10=R5*R7
                              MOV     R10,#0
                              ;
                              LSLS    R11,R5,#23 ;mov 1 for caucaltion & 22 for set up
                              ADDCS   R10,R10,R7,LSL #9
                              ADDMI   R10,R10,R7,LSL #8
                              LSLS    R11,R11,#2
                              ADDCS   R10,R10,R7,LSL #7
                              ADDMI   R10,R10,R7,LSL #6
                              LSLS    R11,R11,#2
                              ADDCS   R10,R10,R7,LSL #5
                              ADDMI   R10,R10,R7,LSL #4
                              LSLS    R11,R11,#2
                              ADDCS   R10,R10,R7,LSL #3
                              ADDMI   R10,R10,R7,LSL #2
                              LSLS    R11,R11,#2
                              ADDCS   R10,R10,R7,LSL #1
                              ADDMI   R10,R10,R7,LSL #0
                              ;
                              ;MIDDLE R9=(R5+R4)(R7+R6)-FIRST-LAST
                              MOV     R9,#0
                              ADD     R5,R5,R4
                              ADD     R7,R6,R7
                              ;
                              LSLS    R11,R5,#22 ;mov 1 for caucaltion & 22 for set up
                              ADDCS   R9,R9,R7,LSL #10
                              LSLS    R11,R11,#1
                              ADDCS   R9,R9,R7,LSL #9
                              ADDMI   R9,R9,R7,LSL #8
                              LSLS    R11,R11,#2
                              ADDCS   R9,R9,R7,LSL #7
                              ADDMI   R9,R9,R7,LSL #6
                              LSLS    R11,R11,#2
                              ADDCS   R9,R9,R7,LSL #5
                              ADDMI   R9,R9,R7,LSL #4
                              LSLS    R11,R11,#2
                              ADDCS   R9,R9,R7,LSL #3
                              ADDMI   R9,R9,R7,LSL #2
                              LSLS    R11,R11,#2
                              ADDCS   R9,R9,R7,LSL #1
                              ADDMI   R9,R9,R7,LSL #0
                              ;
                              SUB     R9,R9,R8
                              SUB     R9,R9,R10
                              MOV     R11,#0
                              ;
                              ;EXIST  LOOP IF IS FROM (A0-A1)(B1-B0)
                              CMP     R13, R2
                              STMIAEQ R2,{R11,R10,R9,R8} ;OPT: dont store to memory, use directlly
                              MOVEQ   PC,LR
                              ;
                              ;ROUNDING R8
                              MOV     R11,R8,LSR #10 ;R11 => int part from est
                              ADD     R11,R11,R8,LSR #16 ;add overflow to carry
                              ADD     R11,R11,R8,LSR #17 ;*00000000010000011000
                              SUB     R8,R8,R11,LSL #10
                              ADD     R8,R8,R11,LSL #4
                              ADD     R8,R8,R11,LSL #3
                              CMP     R8,#1000 ; check rounding error
                              SUBHS   R8,R8,#1000
                              ADDHS   R11,R11,#1
                              CMP     R8,#1000 ; check rounding error caused by rounding error from decimal
MULTIPLY_T1_L1                SUBHS   R8,R8,#1000
                              ADDHS   R11,R11,#1
                              CMP     R8,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTIPLY_T1_L1
                              ADD     R9,R9,R11
                              ;
                              ;ROUNDING R9
                              MOV     R11,R9,LSR #10 ;R11 => int part from est
                              ADD     R11,R11,R9,LSR #16 ;add overflow to carry
                              ADD     R11,R11,R9,LSR #17 ;*00000000010000011000
                              SUB     R9,R9,R11,LSL #10
                              ADD     R9,R9,R11,LSL #4
                              ADD     R9,R9,R11,LSL #3
                              CMP     R9,#1000 ; check rounding error
                              SUBHS   R9,R9,#1000
                              ADDHS   R11,R11,#1
                              CMP     R9,#1000 ; check rounding error caused by rounding error from decimal
MULTIPLY_T1_L2                SUBHS   R9,R9,#1000
                              ADDHS   R11,R11,#1
                              CMP     R9,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTIPLY_T1_L2
                              ADD     R10,R10,R11
                              ;
                              ;ROUNDING R10 & R11
                              MOV     R11,R10,LSR #10 ;R11 => int part from est
                              ADD     R11,R11,R10,LSR #16 ;add overflow to carry
                              ADD     R11,R11,R10,LSR #17 ;*00000000010000011000
                              SUB     R10,R10,R11,LSL #10
                              ADD     R10,R10,R11,LSL #4
                              ADD     R10,R10,R11,LSL #3
                              CMP     R10,#1000 ; check rounding error
                              SUBHS   R10,R10,#1000
                              ADDHS   R11,R11,#1
                              CMP     R10,#1000 ; check rounding error caused by rounding error from decimal
MULTIPLY_T1_L3                SUBHS   R10,R10,#1000
                              ADDHS   R11,R11,#1
                              CMP     R10,#1000 ; check rounding error caused by rounding error from decimal
                              BHS     MULTIPLY_T1_L3
                              ;
                              STMIA   R2,{R11,R10,R9,R8}
                              MOV     PC,LR
                              ;
                              ;
                              ;
RECIPROCAL                    MOV     PC, LR
                              ;
SQUAREROOT                    MOV     PC,LR
                              MOV     R0, R0 ; added by marker to force remarking
