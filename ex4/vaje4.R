# GLAJENJE CASOVNIH VRST
# Financni praktikum 2023/2024
# Anej Rozman

# 1. Naloga

# a.) Import data

d = read.csv('srebro 22.csv')
d = head(d, 120)

# Invert row order
d = d[order(nrow(d):1),] 

# Set column to numeric
d$Close <- as.numeric(gsub("\\$", "", d$Close))

# Set dates as index (not necessary for plot)
d$Date <- as.Date(d$Date, format = "%b %d, %Y")
rownames(d) <- d$Date
d <- d[, -1]

# Convert to time series
d = ts(d[, 4], start = 1)

# b.) Graph of time series

ts.plot(d,
        main = 'Srebro',
        xlab = 'Time',
        ylab = 'EUR')

points(d, pch = 20, cex = 0.7)

# 2. Naloga

# a.) Function for moving average of order k

G = function(vrsta, k){
  
  t <- filter(vrsta, 
              rep(1/k, k), 
              method = 'convolution', 
              sides = 1)
  
  return(t)
}

# b.) Calculate moving average for data in 1. excercise
# and make a next day prediction

ma5 = G(d, 5)

pred = tail(ma5, 1)

ma5 = ts(c(ma5,rep(pred,10)))

# c.) Plot moving average next to data

ts.plot(d, ma5,
        main = "Drsece povprecje",
        ylab = "EUR",
        col = c("black","red"),
        lwd = c(1, 1.5))

points(d, pch = 20, cex = 0.7)


# d.) Mean squared error of moivng average

mse <- function(vrsta,k){
  l = length(vrsta)
  MSE = 0
  ma = G(vrsta,k)
  for(i in k:(l-1)){
    MSE = MSE + (vrsta[i+1] - ma[i+1])^2
  }
  MSE / (l-k)
}

mse(d, 5)

# c.) Other moving averages

# Order 10 moving average
ma10 = G(d, 10)
ma10 = ts(c(ma10, rep(tail(ma10,1),10)))
mse(d, 10)

# Order 20 moving average
ma20 = G(d, 20)
ma20 = ts(c(ma20, rep(tail(ma20,1),10)))
mse(d, 20)

# d.) Plot of multiple moving avergaes

par(mfrow=c(2,2))

# Graph 1
g1 = ts.plot(d, ma5,
             main = "Drsece povprecje reda 5",
             ylab = "EUR", 
             col = c("black","red"),
             lwd = c(1, 1.5))

points(d, pch = 20, cex = 0.7)

# Graph 2
g2 = ts.plot(d, ma10,
             main = "Drsece povprecje reda 10",
             ylab = "EUR", 
             col = c("black","red"),
             lwd = c(1, 1.5))

points(d, pch = 20, cex = 0.7)

# Graph 3
g3 = ts.plot(d, ma20,
             main = "Drsece povprecje reda 20",
             ylab = "EUR", 
             col = c("black","red"),
             lwd = c(1, 1.5))

points(d, pch = 20, cex = 0.7)

par(mfrow=c(1,1))


# 3. Naloga

# a.) Function for exponential moving average 

EG <- function(vrsta, alpha){
  l = length(vrsta)
  ex = c()
  ex[1] = vrsta[1]
  for(i in 2:l){
    ex[i] = alpha * vrsta[i] + (1 - alpha) * ex[i-1]
  }
  return(ex)
}

# b.) Calculate ema for alpha = 0.1 and make a prediction for next day
ema1 = EG(d, 0.1)
ema1 = ts(c(ema1, rep(tail(ema1,1),10)))

ts.plot(d, ema1,
        main = "Eksponentno glajenje, alpha = 0.1",
        ylab = "EUR", 
        col = c("black","red"),
        lwd = c(1, 1.5))

points(d, pch = 20, cex = 0.7)

# c.) Same as b.) but we set aplha to bo 0.9 to compare the results

ema2 = EG(d, 0.7)
ema2 = ts(c(ema2, rep(tail(ema2,1),10)))

ts.plot(d, ema2,
        main = "Eksponentno glajenje, alpha = 0.9",
        ylab = "EUR", 
        col = c("black","red"),
        lwd = c(1, 1.5))

points(d, pch = 20, cex = 0.7)

# We can clearly see that the closer alpha is to 1
# the less it deviates from our graph (obvius from formula), but that is not
# the objective of moving averages, so we would like
# to find an alpha that gives us some information of general 
# direction of the price movement.
