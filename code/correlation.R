measure_overall <- left_join(hrr, effort_overall)
scatter.smooth(measure_overall$BorgRPE, measure_overall$pHRR)
scatter.smooth(measure_overall$OmniRPE, measure_overall$pHRR)

# preliminary test for correlations
cor.test(sqrt(measure_overall$pHRR), measure_overall$BorgRPEdiff)
# t = = 1.2873, df = 70, p-value = 0.2022
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:  -0.08250322  0.37068129
# sample estimates: 0.1520719 
cor.test(sqrt(measure_overall$pHRR), measure_overall$OmniRPEdiff)
# t = 1.4335, df = 70, p-value = 0.1562
# 95 percent confidence interval:  -0.06535017  0.38546315
# sample estimates: 0.1688751 

# regression test, adjusting confounders
anova(lm(sqrt(pHRR) ~ BorgRPEdiff, data = measure_overall))
# Response: sqrt(pHRR)
#             Df  Sum Sq  Mean Sq F value Pr(>F)
# BorgRPEdiff  1 0.05265 0.052646  1.6571 0.2022
# Residuals   70 2.22386 0.031769
anova(lm(sqrt(pHRR) ~ BorgRPEdiff + Activity + Work.Period, data = measure_overall))
# Response: sqrt(pHRR)
#             Df  Sum Sq Mean Sq F value    Pr(>F)    
# BorgRPEdiff  1 0.05265 0.05265  4.0422  0.048465 *  
# Activity     2 0.26595 0.13298 10.2099  0.000137 ***
# Work.Period  2 1.09832 0.54916 42.1645 1.594e-12 ***
# Residuals   66 0.85960 0.01302
lm(sqrt(pHRR) ~ BorgRPEdiff + Activity + Work.Period, data = measure_overall)
# Coefficients:
#  (Intercept)                       BorgRPEdiff  
#     0.547963                         -0.006299  
anova(lm(sqrt(pHRR) ~ OmniRPEdiff, data = measure_overall))
# Response: sqrt(pHRR)
#             Df  Sum Sq  Mean Sq F value Pr(>F)
# OmniRPEdiff  1 0.06492 0.064923  2.0549 0.1562
# Residuals   70 2.21159 0.031594  
anova(lm(sqrt(pHRR) ~ OmniRPEdiff + Activity + Work.Period, data = measure_overall))
# Response: sqrt(pHRR)
#             Df  Sum Sq Mean Sq F value    Pr(>F)    
# OmniRPEdiff  1 0.06492 0.06492  5.1107 0.0270822 *  
# Activity     2 0.23271 0.11636  9.1595 0.0003086 ***
# Work.Period  2 1.14045 0.57022 44.8875 4.926e-13 ***
# Residuals   66 0.83842 0.01270   
lm(sqrt(pHRR) ~ OmniRPEdiff + Activity + Work.Period, data = measure_overall)
# Coefficients:
# (Intercept)                       OmniRPEdiff  
#     0.54059                          -0.01272

# stratify by harvesting methods
measure_overall_ground <- measure_overall %>% filter(Activity=="Ground")
anova(lm(sqrt(pHRR) ~ BorgRPEdiff + Work.Period, data = measure_overall_ground))
anova(lm(sqrt(pHRR) ~ OmniRPEdiff + Work.Period, data = measure_overall_ground))
measure_overall_ladder <- measure_overall %>% filter(Activity=="Ladder")
anova(lm(sqrt(pHRR) ~ BorgRPEdiff + Work.Period, data = measure_overall_ladder))
anova(lm(sqrt(pHRR) ~ OmniRPEdiff + Work.Period, data = measure_overall_ladder))
measure_overall_platform <- measure_overall %>% filter(Activity=="Platform")
anova(lm(sqrt(pHRR) ~ BorgRPEdiff + Work.Period, data = measure_overall_platform))
anova(lm(sqrt(pHRR) ~ OmniRPEdiff + Work.Period, data = measure_overall_platform))

# stratify by work period
measure_overall_t1 <- measure_overall %>% filter(Work.Period=="T1 after 90-min work")
anova(lm(sqrt(pHRR) ~ BorgRPEdiff + Activity, data = measure_overall_t1))
anova(lm(sqrt(pHRR) ~ OmniRPEdiff + Activity, data = measure_overall_t1))
measure_overall_t2 <- measure_overall %>% filter(Work.Period=="T2 after 30-min break")
anova(lm(sqrt(pHRR) ~ BorgRPEdiff + Activity, data = measure_overall_t2))
anova(lm(sqrt(pHRR) ~ OmniRPEdiff + Activity, data = measure_overall_t2))
measure_overall_t3 <- measure_overall %>% filter(Work.Period=="T3 end of work day")
anova(lm(sqrt(pHRR) ~ BorgRPEdiff + Activity, data = measure_overall_t1))
anova(lm(sqrt(pHRR) ~ OmniRPEdiff + Activity, data = measure_overall_t1))
