# VREDNOTENJE EKSOTICNIH OPCIJ
# Financni praktikum 2023/2024
# Anej Rozman

# 1. Naloga

library(combinat)
library(Rlab)

#a.) Define Variables

S0 = 50
u = 1.05
d = 0.95
T = 5
R = 0.03
W = c(1, 2, 3, 4, 5, 6)

# All possible turn outs of ST
periods = hcube(rep(2, T))
periods[periods == 1] = u
periods[periods == 2] = d
periods = cbind(S0, periods)
periods = t(apply(periods, 1, cumprod))

#b.) Calculating the return of an exotic option 
# (call = max(ST - weighted average price (weights W)))
# (put = max(wap - ST))

izplacilo = function(row, W, type){
  wap = sum(row * W)/sum(W)
  if(type == 'call'){
    return(max((row[length(row)] - wap), 0))
  }
  else{
    return(max((wap - row[length(row)]), 0))
  }
}


# Testing our function on data calculated 'by hand'
izplacilo(c(50,52.5,49.88,52.37,49.75,52.24),c(1,0,1,0,1,0),"call")
# expected output: 2.36333

izplacilo(c(50,52.5,55.12,57.88,60.78,63.81),1:6,"put")
# expected output: 0

izplacilo(c(60,61.2,59.98,58.78,57.6,58.75,57.58),rep(1,7),"put")
# expected output: 1.547143

izplacilo(c(60,58.8,57.62,58.78,59.95,61.15,62.37),7:1,"call")
# expected output: 3.065

izplacilo(c(70,66.5,69.83,73.32,76.98,73.13,69.48),c(0,1,2,1,3,2,3),"put")
# expected output: 2.613333

# Calculate all returns for a set of parameters S0, u, d, T, W

resCall = apply(periods, 1, function(row){
  izplacilo(row, W, 'call')
})

resPut = apply(periods, 1, function(row){
  izplacilo(row, W, 'put')
})

periods = as.data.frame(periods)
periods$call = resCall
periods$put = resPut

# 2. Naloga

#a.) Function for calculating the option premium with the binomial 
# model from '1. Naloga'

binomski = function(S0, u, d, R, T, W, type){
  # Risk neutral probability
  q = (1 + R - d) / (u - d)
  
  # All payouts
  periods = hcube(rep(2, T))
  periods[periods == 1] = u
  periods[periods == 2] = d
  periods = cbind(S0, periods)
  periods = t(apply(periods, 1, cumprod))
  
  payout = apply(periods, 1, function(row){
    izplacilo(row, W, type)
  })
  
  # All probabilities 
  prob = hcube(rep(2, T))
  prob[prob == 1] = q
  prob[prob == 2] = 1 - q
  prob = t(apply(prob, 1, cumprod))
  prob = prob[, ncol(prob)]

  # Calculate the premium
  premium = sum(payout * prob)/(1 + R)^T

  return(premium)
}

# Testing our function on data calculated 'by hand'
binomski(50,1.05,0.95,0.03,5,rep(1,6),"put")
# expected output: 0.1271746

binomski(50, 1.05, 0.9 , 0.03, 10,0:10 , "call")
# expected output: 4.359636

binomski(60, 1.05, 0.95, 0.01, 8,c(0,rep(1,8)),"put" )
# expected output: 0.9115613

binomski(70, 1.05, 1, 0, 7, rep(1,8), "call")
# expected output: 0

binomski(80, 1.1 , 0.95, 0.05, 9, 12:3, "put" )
# expected output: 0.1221909

binomski(90, 1.15, 0.8 , 0.01, 11,rep(c(1,0),6), "call")
# expected output: 15.03903

# Using our function on variables from the top of the document
binomski(S0, u, d, R, T, W, 'call')
binomski(S0, u, d, R, T, W, 'put')

#b.) Function for calculating the option premium from '1. Naloga' with a Monte 
# Carlo simulation model

monte = function(S0, u, d, R, T, W, type, N){
  # Risk neutral probability
  q = (1 + R - d) / (u - d)
  
  # Generating random matrix with N rows
  paths = matrix(
    rbinom(N * T, 1, q), 
    nrow = N, 
    ncol = T
  )

  # All payouts
  periods = paths
  periods[periods == 1] = u
  periods[periods == 0] = d
  periods = cbind(S0, periods)
  periods = t(apply(periods, 1, cumprod))
  
  payout = apply(periods, 1, function(row){
    izplacilo(row, W, type)
  })

  # Calculate the premium
  premium = sum(payout)/((1 + R)^T * N)
  
  return(premium) 
}

# Testing our function on data calculated 'by hand'
monte(50, 1.05 ,0.9 ,0.03 ,10 ,0:10, "call" ,100)
# expected output: 4.70097

monte(70, 1.05, 1, 0, 7, c(0,rep(1,7)), "put", 2000)
# expected output: 0

monte(90, 1.15, 0.8, 0.01, 10, 11:1, "call",50000)
# expected output: 16.79661

# Let's try to increase the simulations for a given set of parameters
monte(60, 1.05, 0.95, 0.01, 15, rep(1,16), 'put', 10)
monte(60, 1.05, 0.95, 0.01, 15, rep(1,16), 'put', 100)
monte(60, 1.05, 0.95, 0.01, 15, rep(1,16), 'put', 1000)

# 3. Naloga

#a.) Determinig the accuracy of our method

# Number of simulations
M = 100 

# Vectors for our premium
m1 = rep(0, M)
m2 = rep(0, M)
m3 = rep(0, M)

# Simulations
for (i in 1:M){
  m1[i] = monte(60, 1.05, 0.95, 0.01, 15, rep(1,16), 'put', 10)
  m2[i] = monte(60, 1.05, 0.95, 0.01, 15, rep(1,16), 'put', 100)
  m3[i] = monte(60, 1.05, 0.95, 0.01, 15, rep(1,16), 'put', 1000)
}

# Calcuate the mean and std od our simulations
mean1 = mean(m1)
mean2 = mean(m2)
mean3 = mean(m3)

std1 = sd(m1)
std2 = sd(m2)
std3 = sd(m3)

# Calculate the premium with binomial model
binPremium = binomski(60, 1.05, 0.95, 0.01, 15, rep(1,16), 'put')

#b.) Drawing histograms of our simulation along with mean and std

# Histogram for N=10
hist(m1, 
     main = "Monte Carlo: N=10",
     xlab = "Premija",              
     ylab = "Frequency",          
     col = "yellow",                  
     border = "black",               
     breaks = 8,
     xlim = c(0,5)
)
abline(
  v = c(mean1, binPremium),
  col = c('green', 'red'),
  lty = c(1, 3),
  lwd = 2
)
legend(
  'topright',
  legend = c('Monte Carlo', 'analiza modela'),
  box.lty = 0,
  col = c('green', 'red'), 
  lty = c(1, 3),
  lwd = 2
)
arrows(
  mean1,
  0,
  mean1 + std1,
  0,
  col = 'green',
  lwd = 2,
  length = 0.1
)
arrows(
  mean1,
  0,
  mean1 - std1,
  0,
  col = 'green',
  lwd = 2,
  length = 0.1
)


# Histogram for N=100
hist(m2, 
     main = "Monte Carlo: N=100",     
     xlab = "Premija",                
     ylab = "Frequency",            
     col = "yellow",                   
     border = "black",               
     breaks = 8,
     xlim = c(0,5)
)
abline(
  v = c(mean2, binPremium),
  col = c('green', 'red'),
  lty = c(1, 3),
  lwd = 2
)
legend(
  'topright',
  legend = c('Monte Carlo', 'analiza modela'),
  box.lty = 0,
  col = c('green', 'red'), 
  lty = c(1, 3),
  lwd = 2
)
arrows(
  mean2,
  0,
  mean2 + std2,
  0,
  col = 'green',
  lwd = 2,
  length = 0.1
)
arrows(
  mean2,
  0,
  mean2 - std2,
  0,
  col = 'green',
  lwd = 2,
  length = 0.1
)

#Histogram for N=1000
hist(m3, 
     main = "Monte Carlo: N=1000",
     xlab = "Premija",                
     ylab = "Frequency",            
     col = "yellow",                   
     border = "black",               
     breaks = 8,
     xlim = c(0,5)
)
abline(
  v = c(mean3, binPremium),
  col = c('green', 'red'),
  lty = c(1, 3),
  lwd = 2
)
legend(
  'topright',
  legend = c('Monte Carlo', 'analiza modela'),
  box.lty = 0,
  col = c('green', 'red'), 
  lty = c(1, 3),
  lwd = 2
)
arrows(
  mean3,
  0,
  mean3 + std3,
  0,
  col = 'green',
  lwd = 2,
  length = 0.1
)
arrows(
  mean3,
  0,
  mean3 - std3,
  0,
  col = 'green',
  lwd = 2,
  length = 0.1
)







