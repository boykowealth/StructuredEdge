library(Rcpp)
Rcpp::sourceCpp("src/blackScholes.cpp")
Rcpp::sourceCpp("src/binomialTree.cpp")
Rcpp::sourceCpp("src/financialForward.cpp")
Rcpp::sourceCpp("src/physicalForward.cpp")
Rcpp::sourceCpp("src/irForward.cpp")
Rcpp::sourceCpp("src/excForward.cpp")
Rcpp::sourceCpp("src/financialSwap.cpp")
Rcpp::sourceCpp("src/physicalSwap.cpp")

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
nominal <- 100000
fixedSpot <- 100
floatSpot <- 102


## PARAMS <END>

df.bs <- blackScholes(S=spot, K=strike, T=t2m, r=rf, sigma=sigma, b=costOfCarry, option_type = "Call", nominal=nominal, position_str = "Long") ## Black-Scholes
df.bt <- binomialTree(S=spot, K=strike, T=t2m, r=rf, u=upFactor, d=downFactor, p=prob, steps=steps, option_type = "Put", nominal=nominal, position_str = "Long") ## Binomial Tree
df.ffwd <- finForwardContract(S=spot, T=t2m, r=rf, position_str="Long", nominal=nominal) ## Financial Forward
df.pfwd <- physForwardContract(S=spot, T=t2m, r=rf, c=costOfCarry, position_str="Long", nominal=nominal) ## Physical Forward (Commodity, Real Asset)
df.irfwd <- irForward(r1=r1, r2=r2, t1=t1, t2=t2, position_str="Long", nominal=nominal) ## Interest Rate Forward
df.excfwd <- exchangeForward(S=spot, T=t2m, rd=r1, rf=r2, position_str="Long", nominal=nominal) ## Exchange Rate Forward
df.fswap <- interestRateSwap(fixed_rate=r1, T=t2m, floating_rate=r2, discount_rate=rf, position_str="Long", nominal=nominal) ## Standard Interest Rate Swap
df.pswap <- physicalSwap(fixed_price=fixedSpot, period_length=t2m, spot_price=floatSpot, discount_rate=rf, position_str="Long", nominal=nominal) ## Physical Swap 

