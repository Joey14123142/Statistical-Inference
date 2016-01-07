
# 1 -----------------------------------------------------------------------

n <- 9
mu <- 1100
sigma <- 30
x <- 0.95

ci <- mu + c(-1,1) * qt(x+(1-x)/2, n-1) * sigma / sqrt(n)


# 2 -----------------------------------------------------------------------

n <- 9
mu <- -2
p <- 0.95
upper <- 0
(upper-mu)*sqrt(n)/qt(p+(1-p)/2,n-1)


# 4 -----------------------------------------------------------------------

n <- 10

mnew <- 3
varnew <- 0.6

mold <- 5
varold <- 0.68

p <- 0.95

pair.var <- (varnew + varold)/2
ci <- (mnew - mold) + c(-1, 1) * qt(p + (1-p)/2, 2*n-2) * sqrt(pair.var) * sqrt(2/n)


# 7 -----------------------------------------------------------------------

diet.m <- -3
diet.var <- 1.5
place.m <- 1
place.var <- 1.8
p <- 0.9
n <- 9
pair.var <- (diet.var^2 + place.var^2)/2
ci <- diet.m - place.m +c(-1,1)*qt(p + (1-p)/2, df=(n+n - 2))*sqrt(pair.var)*sqrt(2/n)

mt <- -3
mp <- 1
st <- 1.5
sp <- 1.8
nt <- 9
np <- 9
p <- 0.9
pooled_variance <- ((nt-1)*st^2 + (np-1)*sp^2)/(nt + np - 2)
ci <- mt - mp + c(-1,1) * qt(p + (1-p)/2, df=(nt+np-2)) * sqrt(pooled_variance) * sqrt(1/nt + 1/np)
ci

