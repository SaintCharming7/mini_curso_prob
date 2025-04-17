# Exercício aula 4
# criando um va aleatória discreta - binomial
n <- 10
p <- 0.5
x_values <- 0:n
head(x_values)

# calculando a PMF
pmf <- dbinom(x_values, size=n, prob=p)
pmf

# calculando a CDF
cdf <- pbinom(x_values, size=n, prob=p)

# gráfico da PMF
par(mfrow=c(1,2))
barplot(pmf, names.arg=x_values, col="skyblue", 
        main="PMF",
        xlab="x", ylab="P(X=x)")

# gráfico da CDF
plot(x_values, cdf, type="s", lwd=2, col="red", pch=16,
     main="CDF",
     xlab="x", ylab="F(x)")
points(x_values, cdf, pch=16, col="red")

# Demonstrando que a soma do PMF é igual 1 e CDF chegar a 1
cat("Sum of PMF:", sum(pmf), "\n")
cat("Final CDF value:", cdf[length(cdf)], "\n")


# variável continua com média 0 e desvio pad. 1
x <- seq(-4, 4, length.out=1000)
x
# calculando o PDF
pdf <- dnorm(x)

# Calculando o cdf
cdf <- pnorm(x)

# gráficos pdf e cdf
par(mfrow=c(1,2))
plot(x, pdf, type="l", lwd=2, col="blue",
     main="PDF",
     xlab="x", ylab="f(x)")
abline(h=0, col="gray")

plot(x, cdf, type="l", lwd=2, col="darkgreen",
     main="CDF of Standard Normal",
     xlab="x", ylab="F(x)")

# demonstrando que a integral do pdf é aprox. 1
integral <- integrate(dnorm, lower=-Inf, upper=Inf)$value
cat("Integral of PDF over all x:", integral, "\n")
cat("CDF at +infinity:", pnorm(Inf), "\n")
cat("CDF at -infinity:", pnorm(-Inf), "\n")
