library(Rcpp)
Rcpp::sourceCpp("src/blackScholes.cpp")
Rcpp::sourceCpp("src/binomialTree.cpp")
Rcpp::sourceCpp("src/financialForward.cpp")
Rcpp::sourceCpp("src/physicalForward.cpp")
Rcpp::sourceCpp("src/irForward.cpp")

## PARAMS <START>

spot <- 100
strike <- 100
t2m <- 1
rf <- 0.05
sigma <- 0.2
costOfCarry <- 0.03
upFactor <- 1.2
downFactor <- 0.8
prob <- 0.5
steps <- 3
r1 <- 0.05
r2 <- 0.03
t1 <- 1
t2 <- 2


## PARAMS <END>

df.bs <- blackScholes(spot, strike, t2m, rf, sigma, costOfCarry)
df.bt <- binomialTree(spot, strike, t2m, rf, upFactor, downFactor, prob, steps)
df.ffwd <- finForwardContract(spot, t2m, rf)
df.pfwd <- physForwardContract(spot, t2m, rf, costOfCarry)
df.irfwd <- irForward(r1, r2, t1, t2, t1, t2)
