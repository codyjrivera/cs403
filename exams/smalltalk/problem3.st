OrderedCollection extend [
	zip: b with: oc [
		| a |
		a := OrderedCollection new.
		1 to: self size do: [ :i |
			a add: (b value: (self at: i) value: (oc at: i))
		].
		^ a
	]
	zip: b with: oc1 with: oc2 [
		| a |
		a := OrderedCollection new.
		1 to: self size do: [ :i |
			a add: (b value: (self at: i) value: (oc1 at: i) value: (oc2 at: i))
		].
		^ a
	]
	zip: b withAll: ocs [
		| a |
		a := OrderedCollection new.
		1 to: self size do: [ :i |
			ocs size = 0 ifTrue: [ a add: (self at: i) ]
						 ifFalse: [
							 | val |
							 val := (b value: (self at: i) value: ((ocs at: 1) at: i)).
						     2 to: (ocs size) do: 
								[ :j | val := (b value: val value: ((ocs at: j) at: i)) ].
							 a add: val
						 ]
		].
		^ a
	]
]
