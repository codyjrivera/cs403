Graph extend [
	init: n [
		directed := true.
		matrix := Array new: n.
		1 to: n do: [ :i | 
			| col |
			col := Array new: n.
			1 to: n do: [ :j | col at: j put: false ].
			matrix at: i put: col
		].
		^ self
	]
	addEdge: j to: k [
		(matrix at: j) at: k put: true.
		self isDirected ifFalse: [ (matrix at: k) at: j put: true ].
		^ self
	]
	removeEdge: j to: k [
		(matrix at: j) at: k put: false.
		self isDirected ifFalse: [ (matrix at: k) at: j put: false ].
		^ self
	]
	makeUndirected [
		1 to: (matrix size) do: [ :i |
			1 to: (matrix size) do: [ :j |
				((matrix at: i) at: j) ifTrue: [ (matrix at: j) at: i put: true ]
			]
		].

		directed := false.
		^ self
	]
	numEdges [
		| count |
		count := 0.
		1 to: (matrix size) do: [ :i |
			1 to: i do: [ :j |
				((matrix at: i) at: j) ifTrue: [ count := count + 1 ]
			]
		].
		
		self isDirected ifTrue: [ 
			1 to: (matrix size) do: [ :i |
				i + 1 to: (matrix size) do: [ :j |
					((matrix at: i) at: j) ifTrue: [ count := count + 1 ]
				]
			].
		].
		^ count
	]
]
