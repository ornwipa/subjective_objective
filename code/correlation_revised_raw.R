#### Correlations overall ####
measure_overall <- left_join(hrr, effort_overall)

# preliminary test for correlations
cor.test(sqrt(measure_overall$pHRR), measure_overall$BorgRPE)
# t = 1.2053, df = 70, p-value = 0.2321
# sample estimates:
#   cor 
# 0.1425945
cor.test(sqrt(measure_overall$pHRR), measure_overall$OmniRPE)
# t = 2.8137, df = 70, p-value = 0.006352
# sample estimates:
#   cor 
# 0.3187598  

# regression test, adjusting confounders
anova(lm(sqrt(pHRR) ~ BorgRPE + Activity + Work.Period, data = measure_overall))
# Response: sqrt(pHRR)
#             Df  Sum Sq Mean Sq F value    Pr(>F)    
# BorgRPE      1 0.04629 0.04629  3.8461 0.0540844 .  
# Activity     2 0.22999 0.11500  9.5549 0.0002268 ***
# Work.Period  2 1.20589 0.60295 50.0980 5.814e-14 ***
# Residuals   66 0.79433 0.01204 
lm(sqrt(pHRR) ~ BorgRPE + Activity + Work.Period, data = measure_overall)
# Coefficients:
#  (Intercept)                       BorgRPE 
#     0.67126                       -0.01579  
anova(lm(sqrt(pHRR) ~ OmniRPE + Activity + Work.Period, data = measure_overall))
# Response: sqrt(pHRR)
#             Df  Sum Sq Mean Sq F value    Pr(>F)    
# OmniRPE      1 0.23131 0.23131  17.467 8.782e-05 ***
# Activity     2 0.19157 0.09579   7.233  0.001445 ** 
# Work.Period  2 0.97961 0.48980  36.986 1.681e-11 ***
# Residuals   66 0.87402 0.01324    
lm(sqrt(pHRR) ~ OmniRPE + Activity + Work.Period, data = measure_overall)
# Coefficients:
# (Intercept)                       OmniRPE 
#     0.545238                    -0.004528

# stratify by harvesting methods
measure_overall_ground <- measure_overall %>% filter(Activity=="Ground")
anova(lm(sqrt(pHRR) ~ BorgRPE + Work.Period, data = measure_overall_ground))
anova(lm(sqrt(pHRR) ~ OmniRPE + Work.Period, data = measure_overall_ground)) # 0.0140903 *
measure_overall_ladder <- measure_overall %>% filter(Activity=="Ladder")
anova(lm(sqrt(pHRR) ~ BorgRPE + Work.Period, data = measure_overall_ladder))
anova(lm(sqrt(pHRR) ~ OmniRPE + Work.Period, data = measure_overall_ladder)) # 0.01543 *
measure_overall_platform <- measure_overall %>% filter(Activity=="Platform")
anova(lm(sqrt(pHRR) ~ BorgRPE + Work.Period, data = measure_overall_platform))
anova(lm(sqrt(pHRR) ~ OmniRPE + Work.Period, data = measure_overall_platform)) # 0.085635 .

# stratify by work period
measure_overall_t1 <- measure_overall %>% filter(Work.Period=="T1 after 150-min work")
anova(lm(sqrt(pHRR) ~ BorgRPE + Activity, data = measure_overall_t1))
anova(lm(sqrt(pHRR) ~ OmniRPE + Activity, data = measure_overall_t1))
measure_overall_t2 <- measure_overall %>% filter(Work.Period=="T2 after 30-min break")
anova(lm(sqrt(pHRR) ~ BorgRPE + Activity, data = measure_overall_t2)) # 0.004149 **
anova(lm(sqrt(pHRR) ~ OmniRPE + Activity, data = measure_overall_t2))
measure_overall_t3 <- measure_overall %>% filter(Work.Period=="T3 end of work day")
anova(lm(sqrt(pHRR) ~ BorgRPE + Activity, data = measure_overall_t1))
anova(lm(sqrt(pHRR) ~ OmniRPE + Activity, data = measure_overall_t1))

# Visualize data
ggplot(aes(x=BorgRPE, y=sqrt(pHRR)), 
           data = measure_overall_t2) + 
  geom_point() + geom_smooth(method = "lm") +
  ggtitle("Association between % HRR and Borg RPE after a 30-minute break") +
  xlab("Borg RPE") + ylab("Square Root of % HRR")
ggplot(aes(x=OmniRPE, y=pHRR, col=Activity), 
      data = measure_overall) + 
  geom_point() + geom_smooth(method = "lm") +
  scale_color_manual(values=c("#1B9E77", "#D95F02", "#7570B3")) +
  ggtitle("Association between % HRR and Omni RPE by Harvesting Method") +
  xlab("Omni RPE") + ylab("Square Root of % HRR")

lm(sqrt(pHRR) ~ BorgRPE + Work.Period, data = measure_overall_ladder) # -0.0205
lm(sqrt(pHRR) ~ BorgRPE + Activity, data = measure_overall_t2) # -0.03627

#### Correlations local ####
names(slope) <- c("Subject","Activity","Side","SlopeMPFtime")
slope <- slope %>% mutate(Subject = substr(Subject,5,7))
slope <- transform(slope, Side = ifelse(Side=="DM","Dominant","Nondominant"))
measure_local <- left_join(slope, effort_local)

scatter.smooth(measure_local$Value, measure_local$SlopeMPFtime, 
               xlab = "Borg CR10 difference between start and end of work",
               ylab = "Slope of the MPF-time regression", 
               main = "Association between Borg CR10 and EMG")

anova(lm(SlopeMPFtime ~ Value, data = measure_local))
anova(lm(SlopeMPFtime ~ Value + Side, data = measure_local))
anova(lm(SlopeMPFtime ~ Value + Activity, data = measure_local))
anova(lm(SlopeMPFtime ~ Value + Activity + Side, data = measure_local))

measure_local %>% ggplot(aes(x=Value, y=SlopeMPFtime, col=Activity)) + 
  geom_point() + scale_color_manual(values=c("#1B9E77", "#D95F02", "#7570B3")) +
  labs(x = "Borg CR10 difference between start and end of work",
       y = "Slope of the MPF-time regression",
       title = "Association between Borg CR10 and EMG") #+
  # geom_smooth(method = "lm", fill = NA)
