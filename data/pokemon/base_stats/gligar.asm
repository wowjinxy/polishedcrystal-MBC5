	db  65,  75, 105,  85,  35,  65 ; 430 BST
	;   hp  atk  def  spd  sat  sdf

	db GROUND, FLYING
	db 60 ; catch rate
	db 108 ; base exp
	db NO_ITEM ; item 1
	db RAZOR_FANG ; item 2
	dn FEMALE_50, 3 ; gender, step cycles to hatch
	INCBIN "gfx/pokemon/gligar/front.dimensions"
	abilities_for GLIGAR, HYPER_CUTTER, SAND_VEIL, IMMUNITY
	db MEDIUM_SLOW ; growth rate
	dn INSECT, INSECT ; egg groups

	; ev_yield
	ev_yield   0,   0,   1,   0,   0,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm CURSE, TOXIC, HIDDEN_POWER, SUNNY_DAY, HONE_CLAWS, PROTECT, RAIN_DANCE, BULLDOZE, IRON_TAIL, EARTHQUAKE, RETURN, DIG, ROCK_SMASH, DOUBLE_TEAM, SLUDGE_BOMB, SANDSTORM, SWIFT, AERIAL_ACE, SUBSTITUTE, FACADE, REST, ATTRACT, THIEF, STEEL_WING, ROCK_SLIDE, ROOST, FALSE_SWIPE, X_SCISSOR, DARK_PULSE, ACROBATICS, POISON_JAB, U_TURN, STONE_EDGE, SWORDS_DANCE, CUT, STRENGTH, AQUA_TAIL, COUNTER, DOUBLE_EDGE, DREAM_EATER, EARTH_POWER, ENDURE, HEADBUTT, KNOCK_OFF, SLEEP_TALK, SWAGGER
	; end
