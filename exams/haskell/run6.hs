:load problem5.hs
:load problem6.hs
createTree "BCEGHJ"
map createTree (map (\n -> take n [0..]) [0..10])