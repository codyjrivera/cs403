Object subclass: Graph [
	|matrix directed|
	hasEdge: j to: k [ ^ (matrix at: j) at: k ]
	isDirected [ ^ directed ]
	makeDirected [ directed := true ]
	display [
		Transcript display: (directed ifTrue: [''] ifFalse: ['un']);
			display: 'directed graph, ';
			display: matrix size; display: ' vertices, ';
			display: self numEdges; display: ' edges, ';
		 	display: matrix; cr
	]
]
