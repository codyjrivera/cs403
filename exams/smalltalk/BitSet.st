Collection subclass: BitSet [
        | arr |
        init: n [
		arr := Array new: n.
		1 to: n do: [ :k | arr at: k put: false ] 
	]
	init [ self init: 10 ]
	size [ ^arr size ]
	display [ Transcript display: arr; cr ]
	inRange: index [ ^(1 <= index) and: [ index <= self size ] ]
	member: x [ (self inRange: x)
			ifTrue:  [ ^arr at: x ]
			ifFalse: [ ^false ] 
	]
	insert: x [ (self inRange: x)
			ifTrue: [ arr at: x put: true ]
			ifFalse: [ Transcript display: 'Error: index out of range'; cr ] 
	]
	remove: x [ (self inRange: x)
			ifTrue: [ arr at: x put: false ]
			ifFalse: [ Transcript display: 'Error: index out of range'; cr ] 
	]
	do: aBlock [ 1 to: arr size do:
				[ :k | (arr at: k) ifTrue: [ aBlock value: k ] ] ]
	cardinality [ |n|
		n := 0.
		1 to: self size do: [ :k | (arr at: k) ifTrue: [ n := n+1 ] ].
		^n
	]
]
