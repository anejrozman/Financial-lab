# KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM
# Financni praktikum 2023/2024
# Anej Rozman

# 1. Naloga

library(actuar)

#a.) Histogram of claims

# Import data
d = t(read.delim('vzorec2.txt'))

# Histogram
hist1 = hist(d,breaks = 10,
                  main = "Histogram odskodnin",
                  xlab = "Visina odskodnine",
                  ylab ="Frequency",
                  col ="darkslategray3")

#b.) Modeling with Pareto distribution

params = mde(d, pweibull, start= list(shape=5,scale =1), measure = "CvM")
shape = params$estimate[1] 
scale = params$estimate[2] 

#c.) Compare data with Weibull approximation

# Histogram and density function
hist2 = hist(d, breaks = 10,
                main = "Histogram odskodnin",
                xlab = "Visina odskodnine",
                ylab ="Density",
                col ="darkslategray3", 
                probability = TRUE)

curve(dweibull(x, shape = shape, scale = scale ),
      add = TRUE, 
      col = 'blue', 
      lwd = 1.5)

legend("topright", 
       legend = "Weibullova porazdelitev", 
       col = "blue", 
       lwd = 1.5, 
       cex = 0.8)

# Comparison of CDF's
plot(ecdf(d),main = "Porazdelitvena funkcija odskodnin",
     ylab = "Porazdelitvena funkcija",
     xlab="Visina odskodnine", 
     cex = 0.5)

curve(pweibull(x, shape = shape, scale = scale),
      add = TRUE, 
      col= "blue",
      lwd = 2)

legend('right',
       c("Empiricna porazdelitev","Weibullova porazdelitev"),
       col = c("black","blue"),
       lwd = c(1, 2),
       pch = c(16, NA),
       bty = "n",
       cex = 0.7)

#d.) Calc of E(S) and Var(S) 

# Distribution of number of claims N is Poi(10) so E(N) = Var(N) = 10
eN = 10 
varN = 10

# Weibull distribution Y has a formula
eY = scale * gamma(1 + 1/shape)
varY = (scale^2) * (gamma(1 + 2/shape) - (gamma(1 + 1/shape))^2)

# Using Wald's identities
eS = eN*eY # = 43.62956 
varS = varY*eN + (eY^2)*varN # = 230.5949 

# 2. Naloga

#a.) Discretization of Y = Weibull 

disY = discretize(pweibull(x, shape = shape, scale = scale),
                  method = 'rounding', 
                  from = 0, 
                  to = 10, 
                  step = 0.5)

# CPF of disY
cmY = stepfun(seq(0, 9.5, 0.5), diffinv(disY))

#b.) Plot of CPF of Y and its discretization

plot(cmY, 
     main = "Weibullova porazdelitev",
     pch = NA,
     lwd = 1.5,
     xlab = "x",
     ylab = "Porazdelitvena funkcija",
     col = "orange")

curve(pweibull(x, shape = shape, scale = scale ),
      add = TRUE, 
      lwd = 1.5)

#c.) Approximation of distribution of S using Panjer recursion

disYPanjer = discretize(pweibull(x, shape = shape, scale = scale),
                        method = 'rounding', 
                        from = 0, 
                        to = 100000, 
                        step = 0.5)

pdfPanjer = aggregateDist(method = 'recursive',
                          model.freq = 'poisson',
                          lambda = 10,
                          model.sev = disYPanjer,
                          x.scale = 0.5, 
                          tol = 0.001,
                          maxit = 100000000)


#d.) Calc of E(cdfPanjer) and Var(cdfPanjer)

eP = mean(pdfPanjer) # = 43.52655

varP = sum((knots(pdfPanjer) - eP)^2 * diff(pdfPanjer))# = 227.097


# 3. Naloga

#a.) Simulation of S using Monte Carlo 

nSim = 10000
valS <- numeric(nSim)
N = rpois(nSim, 10)

# Generating S values
for (i in 1:nSim) {
  
  n = N[i]
  
  if (n > 0) {
    Y = rweibull(n, shape, scale)
    valS[i] = sum(Y)
  } 
  else {
    valS[i] = 0  # If N = 0, the sum is 0
  }
}

#b.) Calc of mean and variance

eSm = mean(valS) # = 43.55653
varSm = var(valS) # = 235.984

# The values for E(S) and Var(S) from Monte Carlo simulation are approximately the
# same as the values from 1.d and 2.d, so we cannot really derive the better 
# method from our approaches.

#c.) Comparison of CPF's from Panjer and Monte Carlo approximations

simGraph = plot(pdfPanjer,
                cex = 0.1, 
                pch = 16)

plot(ecdf(t(as.matrix(valS))),
     add = TRUE,
     col = 'green')


legend('bottomright', 
       legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       lwd = 1:1,
       col = c('black', 'green'),
       cex=0.8,
       bty = 'n')

# From the graph we can see that both methods are very similar in aproximating
# the distribution.



