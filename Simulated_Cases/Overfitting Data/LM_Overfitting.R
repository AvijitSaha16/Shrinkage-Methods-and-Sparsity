Summary of model fitted on training dataset:

Call:
lm(formula = train[, 1] ~ train[, 2] + train[, 3])

Residuals:
     Min       1Q   Median       3Q      Max 
-15.1864  -2.4250  -0.1719   2.5141  15.6878 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.08391    0.32184   0.261    0.795    
train[, 2]   1.61003    0.04794  33.583   <2e-16 ***
train[, 3]   2.58998    0.05361  48.307   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.54 on 197 degrees of freedom
Multiple R-squared:  0.9482,	Adjusted R-squared:  0.9477 
F-statistic:  1803 on 2 and 197 DF,  p-value: < 2.2e-16




Residual Mean Square on test dataset: 17.54472