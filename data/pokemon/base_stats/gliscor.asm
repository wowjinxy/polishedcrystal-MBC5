	db  75,  95, 125,  95,  45,  75 ; 510 BST
	;   hp  atk  def  spd  sat  sdf

	db GROUND, FLYING
	db 30 ; catch rate
	db 192 ; base exp
	db NO_ITEM ; item 1
	db NO_ITEM ; item 2
	dn FEMALE_50, 3 ; gender, step cycles to hatch
	INCBIN "gfx/pokemon/gliscor/front.dimensions"
	abilities_for GLISCOR, HYPER_CUTTER, SAND_VEIL, POISON_HEAL
	db MEDIUM_SLOW ; growth rate
	dn INSECT, INSECT ; egg groups

	; ev_yield
	ev_yield   0,   0,   2,   0,   0,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm CURSE, TOXIC, HIDDEN_POWER, SUNNY_DAY, HONE_CLAWS, HYPER_BEAM, PROTECT, RAIN_DANCE, BULLDOZE, IRON_TAIL, EARTHQUAKE, RETURN, DIG, ROCK_SMASH, DOUBLE_TEAM, SLUDGE_BOMB, SANDSTORM, SWIFT, AERIAL_ACE, SUBSTITUTE, FACADE, REST, ATTRACT, THIEF, STEEL_WING, ROCK_SLIDE, ROOST, FALSE_SWIPE, X_SCISSOR, DARK_PULSE, ACROBATICS, POISON_JAB, GIGA_IMPACT, U_TURN, STONE_EDGE, SWORDS_DANCE, CUT, FLY, STRENGTH, AQUA_TAIL, COUNTER, DOUBLE_EDGE, DREAM_EATER, EARTH_POWER, ENDURE, HEADBUTT, KNOCK_OFF, SLEEP_TALK, SWAGGER
	; end
