FileStream fileIn: 'BitSet.st'.
a := BitSet new init.
b := BitSet new init.
1 to: 10 by: 2 do: [ :k | a insert: k ].
1 to: 10 by: 3 do: [ :k | b insert: k ].

FileStream fileIn: 'problem1.st'.
a complement display.
b complement display.
(a union: b) display.
(a intersect: b) display.
(a difference: b) display.
(b difference: a) display.

c := BitSet new init.
1 to: 10 do: [ :k | c insert: k ].
(c union: a) display.
(c intersect: a) display.
(c difference: a) display.