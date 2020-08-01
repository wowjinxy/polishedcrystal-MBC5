; Boolean checks
FALSE EQU 0
TRUE  EQU 1

; flag manipulation
RESET_FLAG EQU 0
SET_FLAG   EQU 1
CHECK_FLAG EQU 2

SAVE_CHECK_VALUE_1 EQU 99
SAVE_CHECK_VALUE_2 EQU 127

HMENURETURN_SCRIPT EQU %10000000
HMENURETURN_ASM    EQU %11111111

MORN_HOUR EQU 4
DAY_HOUR EQU 10
NITE_HOUR EQU 20

NO_RTC_SPEEDUP EQU 6

	const_def
	const ERR_RST_0
	const ERR_DIV_ZERO
	const ERR_EGG_SPECIES
	const ERR_EXECUTING_RAM
	const ERR_STACK_OVERFLOW
	const ERR_STACK_UNDERFLOW
FIRST_VBLANK_ERR EQU ERR_EXECUTING_RAM
