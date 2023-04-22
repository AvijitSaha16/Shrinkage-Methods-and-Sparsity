Call:
lm(formula = y ~ bvn1[, 1] + bvn1[, 2])

Residuals:
     Min       1Q   Median       3Q      Max 
-2.70163 -0.76391 -0.03572  0.65812  2.71077 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.1474     0.1043  -1.413   0.1610  
bvn1[, 1]    -0.3091     2.5356  -0.122   0.9032  
bvn1[, 2]     5.2319     2.5305   2.068   0.0413 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.032 on 97 degrees of freedom
Multiple R-squared:  0.9602,	Adjusted R-squared:  0.9593 
F-statistic:  1169 on 2 and 97 DF,  p-value: < 2.2e-16