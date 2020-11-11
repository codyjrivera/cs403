Object subclass: DynamicStack [
	| n cap arr |
	init [
		n := 0.
		cap := 2.
		arr := Array new: 2
	]
	isEmpty [
		^n==0
	]
        display [
		Transcript display: arr; cr
	]
]
