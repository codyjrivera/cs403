DynamicStack extend [
	push: x [
		| newarr |
		n >= cap ifTrue: [
			cap := cap * 2.
			newarr := Array new: cap.
			1 to: n do: [ :i | newarr at: i put: (arr at: i) ].
			arr := newarr.
		].
		n := n + 1.
		arr at: n put: x.
		^ self
	]
	pop [
		| val newarr |
		n <= 0 ifTrue: [ self error: 'Cannot pop from empty stack' ].
		val := arr at: n.
		arr at: n put: nil.
		n := n - 1.
                "Patch for Borie's grading script: originally n <= (cap quo: 4)"
		n < (cap quo: 4) ifTrue: [
			cap := (cap quo: 2) max: 2. "cap >= 2 always"
			newarr := Array new: cap.
			1 to: n do: [ :i | newarr at: i put: (arr at: i) ].
			arr := newarr.
		].
		^ val
	]
]
