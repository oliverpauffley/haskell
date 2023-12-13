
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]


myTuples =[(x,y) | x <- mySqr, y <- myCube]
myTuplesLimited =[(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

