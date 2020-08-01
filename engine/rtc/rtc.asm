StartClock::
	; read the current clock time into the cache in HRAM
	call GetClock
	call Function1409b
	call FixDays
	ret nc
	jp RecordRTCStatus

Function1409b:
	ld hl, hRTCDayHi
	bit 7, [hl]
	jr nz, .set_bit_7
	bit 6, [hl]
	jr nz, .set_bit_7
	xor a
	ret

.set_bit_7
	; Day count exceeds 16383
	ld a, %10000000
	jp RecordRTCStatus ; set bit 7 on sRTCStatusFlags

Function140ae:
	call CheckRTCStatus
	ld c, a
	and %11000000 ; Day count exceeded 255 or 16383
	jr nz, .time_overflow

	ld a, c
	and %00100000 ; Day count exceeded 139
	jr z, .dont_update

	call UpdateTime
	ld a, [wRTC]
	ld b, a
	ld a, [wCurDay]
	cp b
	jr c, .dont_update

.time_overflow
	farjp ClearDailyTimers

.dont_update
	xor a
	ret

_InitTime::
	call GetClock
	call FixDays
	ld hl, hRTCSeconds
	ld de, wStartSecond
	ld bc, wStringBuffer2 + 3
; seconds
	ld a, [bc]
	sub [hl]
	dec hl
	jr nc, .ok_secs
	add 60
.ok_secs
	ld [de], a
	dec de
	dec bc
; minutes
	ld a, [bc]
	sbc [hl]
	dec hl
	jr nc, .ok_mins
	add 60
.ok_mins
	ld [de], a
	dec de
	dec bc
; hours
	ld a, [bc]
	sbc [hl]
	dec hl
	jr nc, .ok_hrs
	add 24
.ok_hrs
	ld [de], a
	dec de
	dec bc
; days
	ld a, [bc]
	sbc [hl]
	dec hl
	jr nc, .ok_days
	add 140
	ld c, 7
	call SimpleDivide
.ok_days
	ld [de], a
	ret
