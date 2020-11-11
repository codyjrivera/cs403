Matrix extend [
	rows: r cols: c init: b [
		rows := r.
		cols := c.
		arr := Array new: r.
		1 to: r do: [ :i |
			| col |
			col := Array new: c.
			1 to: c do: [ :j |
				col at: j put: (b value: i value: j).
			].
			arr at: i put: col
		].
		^ self
	]
	transpose [
		^ Matrix new 
		         rows: cols 
		         cols: rows 
				 init: [ :i :j | self at: j at: i ]
	]
	* x [
		^ Matrix new
				 rows: rows
				 cols: x cols
				 init: [ :i :j | 
					| c dp |
					c := cols. "Assume compatible"
					dp := (self at: i at: 1) * (x at: 1 at: j).
					2 to: c do:
						[ :k | dp := dp + ((self at: i at: k) * (x at: k at: j))].
					dp
				 ]
	]
]
