w := #( 1  2  3  4) asOrderedCollection.
x := #( 5  6  7  8) asOrderedCollection.
y := #( 9 10 11 12) asOrderedCollection.
z := #(13 14 15 16) asOrderedCollection.

FileStream fileIn: 'problem3.st'.

Transcript display: (w zip: [:a :b | a+b] with: x); cr.
Transcript display: (w zip: [:a :b | a*b] with: x); cr.

Transcript display: (w zip: [:a :b :c | a+b+c] with: x with: y); cr.
Transcript display: (w zip: [:a :b :c | a*b*c] with: x with: y); cr.

Transcript display: (w zip: [:a :b | a+b] withAll: {x . y . z}); cr.
Transcript display: (w zip: [:a :b | a*b] withAll: {x . y . z}); cr.
