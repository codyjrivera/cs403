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
