Object subclass: Matrix [
	|rows cols arr|
	rows [^rows]
	cols [^cols]
	at: j at: k [ ^(arr at: j) at: k ]
	at: j at: k put: v [ (arr at: j) at: k put: v ]
	display [Transcript display: arr; cr]
]
