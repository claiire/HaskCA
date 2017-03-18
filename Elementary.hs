-- The basic element of CA, a cell which can be on (I) or off (O)
data Cell = I | O deriving Show

-- From a Rule' datatype, create a rule function
makeRule :: Cell -> Cell -> Cell -> Cell -> Cell -> Cell -> Cell -> Cell -> (Cell -> Cell -> Cell -> Cell)
makeRule iii iio ioi ioo oii oio ooi ooo = update
  where update I I I = iii 
        update I I O = iio 
        update I O I = ioi 
        update I O O = ioo 
        update O I I = oii 
        update O I O = oio 
        update O O I = ooi 
        update O O O = ooo 


evolve :: (Cell -> Cell -> Cell -> Cell) -> [Cell] -> [Cell]
evolve r []         = [r O O O]
evolve r (x:[])     = (r O O x) : (r O x O) : [r x O O]
evolve r (x:y:[])   = (r O O x) : (r O x y) : (r x y O) : [r y O O]
evolve r (x:y:z:cs) = (r O O x) : (r O x y) : (r x y z) : evolve' r (y:z:cs)
  where evolve' r (x:y:[])   = [r x y O]
        evolve' r (x:y:z:cs) = (r x y z) : evolve' r (y:z:cs)
