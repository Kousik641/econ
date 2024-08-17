econ <- read.csv("https://raw.githubusercontent.com/Kousik641/econ/main/econ.csv",sep=",",header=T)

dim(econ)
summary(econ[ ,-1])

par(mfrow = c(1,2))
hist(econ$pop, prob = T, xlab = "Population", main = "Histogram of population")
hist(econ$pcgmp, prob = T, xlab = "Per-capita GMP", main = "Histogram of pcgmp")

plot(econ$pop, econ$pcgmp, xlab = "Population", ylab = "per-capita GMP", main = "Population vs per-capita gmp of US cities")
abline(lm(pcgmp~pop, data=econ), col = "red")

x <- econ$pop
y <- econ$pcgmp
beta.hat <- cov(x,y) / (var(x) * (1-1/length(x)))
alpha.hat <- mean(y) - beta.hat*mean(x)
beta.hat
alpha.hat

coefficients(lm(y~x))

pitts.index <- grep("Pittsburgh", econ$MSA)
unlist(econ[pitts.index, 1:3])
cat("per-capita GMP for Pittsburgh predicted by model = ",
    predict(lm(pcgmp~pop,data=econ), newdata=data.frame(pop = econ$pop[pitts.index])), "\n")
cat("residual for Pittsburgh = ", residuals(lm(pcgmp~pop, data=econ))[pitts.index], "\n")

resid<-residuals(lm(pcgmp~pop, data=econ))
cat("Mean square error of the regression model = ", mean((resid-mean(resid))^2), "\n")

unname(predict(lm(y~x), newdata = data.frame(x = econ$pop[pitts.index]+1e5)))

unname(predict(lm(pcgmp~., data=econ[ ,-1]), newdata=data.frame(
  pop = econ$pop[pitts.index]+1e5, econ[pitts.index,4:7])))
