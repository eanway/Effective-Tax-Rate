## name: taxes
## author: Evan Anway
## created: 1/3/2018
## modified: 1/3/2018
## dependencies: none
## description: graphs effective tax rate
## patch notes: none

rm(list=ls())

marginal_new = c(10, 12, 22, 24, 32, 35, 37)
brackets_new = c(0, 9525, 38700, 82500, 157500, 200000, 500000)

marginal_old = c(10, 15, 25, 28, 33, 35, 39.6)
brackets_old = c(0, 9525, 38700, 93700, 195450, 424950, 426700)

income = seq(1000,600000, by = 1000)

# income is the expected income
# marginal is the vector of marginal tax rates between 0 and 100
# brackets is the vector of minimum value of each tax bracket
effective <- function(income, marginal, brackets) { 
  
  marginal <- marginal / 100
  diff_brackets <- c(diff(brackets), Inf)
  
  maximum = marginal[1:(length(marginal))] * diff_brackets
  maximum = c(maximum, Inf)
  
  tax = 0
  remaining = income
  for(i in 1:length(marginal)){
    marginal_tax = remaining * marginal[i]
    if(marginal_tax > maximum[i]) {
      tax = tax + maximum[i]
      remaining = remaining - diff_brackets[i]
    } else {
      tax = tax + marginal_tax
      break
    }
  }
  
  return(tax)
}

taxes_new = unlist(lapply(income, effective, marginal = marginal_new, brackets = brackets_new))
taxes_old = unlist(lapply(income, effective, marginal = marginal_old, brackets = brackets_old))

#####
{
filename = "Effective Tax Rate 2018.png"
pathname = "E:/Personal/Experiments/taxes/results"
png(file = file.path(pathname, filename), width = 1600, height = 1000)
par(las = 1, cex = 2)
options(scipen=10)
plot(income, taxes_old/income*100, 
     type = "l", col = "blue",
     bty = "n",
     xaxt = "n",
     lwd = 2,
     xlab = "Income ($)", ylab = "Effective Tax Rate (%)")
lines(income, taxes_new/income*100, 
     type = "l", col = "red",
     lwd = 2)
axis(1, at=pretty(income), labels = format(pretty(income), big.mark = ","))

legend("bottomright", 
       legend = c("Previous 2018 Tax Plan", "New 2018 Tax Plan"), 
       col = c("blue", "red"), 
       lty = 1, lwd = 2,
       bty = "n")

dev.off()
}
#####
#taxes_change = taxes_new/income - taxes_old/income

#plot(income, taxes_change,
#     type = "l", col = "green",
#     ylim = c(-0.05, 0.05))
#abline(h=0)
