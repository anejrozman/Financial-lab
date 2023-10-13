# OBRESTNE KRIVULJE IN HIPOTEZA PRICAKOVANJ
# Financni praktikum 2023/2024
# Anej Rozman

# 1. Naloga

#a.) Import data

t1 = read.csv('hist_EURIBOR_2014.csv', row.names = 1)
t2 = read.csv('hist_EURIBOR_2015.csv', row.names = 1)
t3 = read.csv('hist_EURIBOR_2016.csv', row.names = 1)

#b.) Filter and merge data

t1 = t1[c(1, 23, 43, 64, 84, 105, 126, 149, 170, 192, 215, 235)]
t1 = t(t1)

t2 = t2[c(1, 22, 42, 64, 84, 104, 126, 149, 170, 192, 214, 235)]
t2 = t(t2)

t3 = t3[c(1, 21, 42, 63, 84, 106, 128, 149, 172, 194, 215, 237)]
t3 = t(t3)

t = rbind(t1, t2, t3)

#c.) Plot of the term interest rate

A = ts(t[, 6], start = c(2014, 1), frequency = 12)
B = ts(t[, 8], start = c(2014, 1), frequency = 12)


ts.plot(A, B, xlab = 'Time',
              ylab = '%',
              col = c('cyan', 'pink'), 
              main = 'Euribor')
legend('topright', 
       c('6m', '12m'), 
       col = c('cyan', 'pink'), 
       lwd = 1)

#2.Naloga

#a.) Interesting dates

d1 = 'X2.05.2014'
d2 = 'X1.07.2014'
d3 = 'X1.10.2014'

#b.) Time structure of interest rates

time = c(1/4, 1/2, 1, 2, 3, 6, 9, 12)

plot(time, t[d1,], xlab = 'Dospetje[mesec]',
                   ylab = '%',
                   col = 'cyan', 
                   main = 'Casovna struktura Euribor', 
                   type = 'o', 
                   ylim = c(-0.1, 0.7))

points(time, t[d2,], col = 'pink', 
                     type = 'o')

points(time, t[d3,], col = 'orange',
                     type = 'o')

text(time[length(time)],
     t[d1,][length(time)],
     "2.05.2014",
     pos = 2, offset = 1,
     col = 'cyan',
     cex = 0.7)
text(time[length(time)],
     t[d2,][length(time)],
     "1.07.2014",
     pos = 2,
     offset = 1,
     col = 'pink',
     cex = 0.7)
text(time[length(time)],
     t[d3,][length(time)],
     "1.10.2014",
     pos = 2,
     offset = 1,
     col = 'orange',
     cex = 0.7)

# 3.Naloga 

#a.) Calculation of 6x12 term interest rates

t = as.data.frame(t)
trates = t[,c('6m', '12m')]
trates$trate_pred = 2*(((1 + trates[,'12m'])/(1 + (trates[,'6m'])/2)) - 1)

#b.) Adding predictions to original data

temp = c(NA, NA, NA, NA, NA, NA, trates$trate_pred[1:30])
trates$trate_pred = temp

#c.) Scatter plot of interest rates
tpred1 = trates[7:12,]
tpred2 = trates[13:24,]
tpred3 = trates[25:36,]

plot(tpred1[,'trate_pred'], tpred1[,'6m'], 
     xlab = "Napoved", 
     ylab = "Opazovano", 
     main = "6m Euribor 2014-2016",
     col = "red",  
     pch = 19,
     xlim = c(-0.25, 0.8),
     ylim = c(-0.25, 0.8)
)

points(tpred2[,'trate_pred'], tpred2[,'6m'], 
       col = 'blue', 
       pch = 19)

points(tpred3[,'trate_pred'], tpred3[,'6m'], 
       col = 'cyan', 
       pch = 19)

abline(a = 0, b = 1, lty = 2)

regression = lm(trates[7:36, 1]~trates[7:36,3])
abline(regression)

legend('topleft', 
       c('2014', '2015', '2016'), 
       col = c('red', 'blue', 'cyan'), 
       pch = 19)

# d.) Scatter plot of individual years

# 2014
plot(tpred1[,'trate_pred'], tpred1[,'6m'], 
     xlab = "Napoved", 
     ylab = "Opazovano", 
     main = "6m Euribor 2014",
     col = "red",  
     pch = 19,
     xlim = c(-0.25, 0.8),
     ylim = c(-0.25, 0.8)
)

abline(a = 0, b = 1, lty = 2)

regression = lm(tpred1[,'6m']~tpred1[,'trate_pred'])
abline(regression, col = 'red')

# 2015
plot(tpred2[,'trate_pred'], tpred2[,'6m'], 
     xlab = "Napoved", 
     ylab = "Opazovano", 
     main = "6m Euribor 2015",
     col = "blue",  
     pch = 19,
     xlim = c(-0.1, 0.7),
     ylim = c(-0.1, 0.7)
)

abline(a = 0, b = 1, lty = 2)

regression = lm(tpred2[,'6m']~tpred2[,'trate_pred'])
abline(regression, col = 'blue')

# 2016
plot(tpred3[,'trate_pred'], tpred3[,'6m'], 
     xlab = "Napoved", 
     ylab = "Opazovano", 
     main = "6m Euribor 2016",
     col = "cyan",  
     pch = 19,
     xlim = c(-0.25, 0.4),
     ylim = c(-0.25, 0.4)
)

abline(a = 0, b = 1, lty = 2)

regression = lm(tpred3[,'6m']~tpred3[,'trate_pred'])
abline(regression, col = 'cyan')

#e.)
# Ce bi hipoteza pricakovanj trga velja bi morale vrednosti/razmerja med 
# napovedjo terminske obrestne mere in dejansko biti enake 1. Morale bi lezati
# na simetrali lihih kvadrantov. Ocitno empiricni podatki te hipoteze ne potrjujejo.
