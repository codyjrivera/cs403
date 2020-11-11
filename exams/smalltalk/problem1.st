BitSet extend [
	complement [
		| a |
		a := BitSet new init: (self size).
		1 to: (self size) do:
			[ :i | ((self member: i) ~= false) ifFalse: [ a insert: i ] ].
		^ a
	]
	union: t [
		| a |
		a := BitSet new init: (self size).
		1 to: (self size) do: [ :i | 
			((self member: i) ~= false or: [((t member: i) ~= false)])
				ifTrue: [ a insert: i ]
		].
		^ a
	]
	intersect: t [
		| a |
		a := BitSet new init: (self size).
		1 to: (self size) do: [ :i | 
			((self member: i) ~= false and: [((t member: i) ~= false)])
				ifTrue: [ a insert: i ]
		].
		^ a
	]
	difference: t [
		"Difference - If the bitset contains an element that t doesn't,
		 the element is true, otherwise, it is false"
		| a |
		a := BitSet new init: (self size).
		1 to: (self size) do: [ :i |
			((self member: i) ~= false and: [((t member: i) = false)])
				ifTrue: [ a insert: i ]
		].
		^ a
	]
]
