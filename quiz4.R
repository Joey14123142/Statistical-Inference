
# 1 -----------------------------------------------------------------------

pharm <- data.frame(baseline=c(140,138,150,148,135), week2=c(132,135,151,146,130))
t.test(pharm$baseline,pharm$week2, alternative = "two.sided", paired=TRUE)


# 2 -----------------------------------------------------------------------

n = 9
m = 1100
s = 30
a = 0.05
m+c(-1,1)*qt(1-(a/2),n-1)*s/sqrt(n)


# 3 -----------------------------------------------------------------------

binom.test(x = 3, n = 4, p=.5, alternative="greater")


# 4 -----------------------------------------------------------------------

p = 1/100
p_ = 10/1787
n=1787
serror = sqrt(p*(1-p)/n)
test_z = (p-p_)/serror

pnorm(test_z, lower.tail=FALSE)


# 5 -----------------------------------------------------------------------

mean.diff = -3-1
df = (9 + 9 - 2)
m_tr = -3
m_pb = 1
s_tr = 1.5
s_pb = 1.8
pooled.var = (s_tr^2 * 9 + s_pb^2 * 9)/df
se.diff = sqrt(pooled.var/9 + pooled.var/9)
t.obt = mean.diff / se.diff
t.obt

p.value = 2*pt(t.obt, df=df) #two tailed
p.value



# 6, 7 ---------------------------------------------------------------------

power.t.test(n=100, delta = .01, sd = .04, type = "one.sample", alt = "one.sided")$power
power.t.test(power = .9, delta = .01, sd = .04, type="one.sample", alt="one.sided")$n
