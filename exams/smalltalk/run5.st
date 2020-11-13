FileStream fileIn: 'Graph.st'.
FileStream fileIn: 'problem5.st'.

g := Graph new init: 2.
g display.

g addEdge: 1 to: 2.
g display.

g makeUndirected.
g display.

g makeDirected.
g display.

g removeEdge: 1 to: 2.
g display.

g makeUndirected.
g display.

g removeEdge: 2 to: 1.
g display.

g addEdge: 2 to: 1.
g display.

g2 := Graph new init: 6.

g2 addEdge: 1 to: 5.
g2 display.

g2 addEdge: 2 to: 6.
g2 display.

g2 addEdge: 3 to: 1.
g2 display.

g2 makeUndirected.
g2 display.

g2 addEdge: 1 to: 6.
g2 display.

g2 makeDirected.
g2 display.

g3 := Graph new init: 1.
g3 display.

g3 addEdge: 1 to: 1.
g3 display.

g3 makeUndirected.
g3 display.
