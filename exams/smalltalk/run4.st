FileStream fileIn: 'Matrix.st'.
FileStream fileIn: 'problem4.st'.
m1 := Matrix new rows: 3 cols: 2 init: [:j :k | (3*j)+k-1].
m1 display.
m2 := Matrix new rows: 2 cols: 3 init: [:j :k | j+(2*k)+1].
m2 display.
m3 := m1 transpose.
m3 display.
m4 := m2 transpose.
m4 display.
m3 := m1*m2.
m3 display.
m4 := m2*m1.
m4 display.

id2 := Matrix new rows: 2 cols: 2 init: [:j :k | j = k ifTrue: [ 1 ] ifFalse: [ 0 ]].
id3 := Matrix new rows: 3 cols: 3 init: [:j :k | j = k ifTrue: [ 1 ] ifFalse: [ 0 ]].
id2 display.
id3 display.

rd2 := id2 transpose.
rd3 := id3 transpose.
rd2 display.
rd3 display.

m1c := m1 * id2.
m1d := id3 * m1.
m1d display.
m1d display.

v1 := Matrix new rows: 1 cols: 3 init: [:j :k | k = 3 ifTrue: [ 1 ] ifFalse: [ 0 ]].
v2 := Matrix new rows: 3 cols: 1 init: [:j :k | j = 1 ifTrue: [ 1 ] ifFalse: [ 0 ]].

(v1 * v2) display.
((v2 transpose) * (v1 transpose)) display.


