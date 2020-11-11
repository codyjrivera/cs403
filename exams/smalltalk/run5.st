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
