module boolean{

pure val myFalse = false
pure val myTrue = true

pure def myNot(x) = not(x)

pure def myEq(x, y) = x == y 

pure def myNeq(x, y) = x != y

pure def myNot2(x) = x.not()

pure def myAnd(x, y) = x and y

pure def myAnd2(x, y) = x.and(y)

pure def myAnd3(x, y, z) = x and y and z


pure def myAnd4(x, y ,z) = and {x, y, z}

pure def myOr(x, y) = x or y

pure def myImplies(x, y) = x implies y

pure def myIff(x, y) = x iff y

}