x = c(-1, 0, 1)
y = c(1, 5, 3)
lm1 = lm(y ~ x)
summary(lm1)
lm2 = lm(y ~ 1)
summary(lm2)
SSE_lm1 = sum(lm1$residuals^2)
SSE_lm2 = sum(lm2$residuals^2)
delta_SSE = SSE_lm2 - SSE_lm1
delta_p = 1
MSE_lm1 = 1/3*SSE_lm1
F_stat = (delta_SSE/delta_p)/MSE_lm1
nu_1 = delta_p
nu_2 = 3 - 2
(p_val = 1 - pf(q = F_stat, df1 = nu_1, df2 = nu_2))
anova(lm2, lm1)
