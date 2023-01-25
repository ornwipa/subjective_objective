library(readr)
library(tidyverse)
library(ggpubr)
library(dunn.test)

# Import effort survey data
effort <- read_csv("data/EA14_EffortSurveyResults_2014.csv", 
                   col_types = cols(participant_id_number = col_character()))
effort <- effort %>%
  mutate(Subject = participant_id_number,
         Activity = harvesting_method,
         `Work Period` = work_period) %>%
  select(Subject, Activity, `Work Period`, 
         RightShoulder, LeftShoulder, BorgRPE, OmniRPE)

# Fix wrong data entry Subject710 double End_Lunch, no End_Work
effort$`Work Period`[40] <- "End_Work"

# Extract baseline overall effort (BorgRPE and OmniRPE)
effort_overall_baseline <- effort %>%
  filter(`Work Period` == "Start_Work") %>%
  mutate(BorgRPEbaseline = BorgRPE, OmniRPEbaseline = OmniRPE) %>%
  select(Subject, Activity, BorgRPEbaseline, OmniRPEbaseline)

# Extract overall effort through work day, subtract the baseline effort
effort_overall <- effort %>%
  filter(`Work Period` != "Start_Work") %>%
  select(Subject, Activity, `Work Period`, BorgRPE, OmniRPE)
effort_overall <- left_join(effort_overall, effort_overall_baseline) %>%
  mutate(BorgRPEdiff = BorgRPE-BorgRPEbaseline, OmniRPEdiff = OmniRPE-OmniRPEbaseline)
effort_overall <- transform(effort_overall, 
                     `Work Period` = ifelse(`Work Period`=="Start_Lunch", 
                                            "T1 after 150-min work",
                                            ifelse(`Work Period`=="End_Lunch", 
                                                   "T2 after 30-min break",
                                                   "T3 end of work day")))

# Examine data
hist(effort_overall$BorgRPEdiff, breaks = 15)
hist(effort_overall$OmniRPEdiff, breaks = 10)
ggqqplot(effort_overall$BorgRPEdiff)
ggqqplot(effort_overall$OmniRPEdiff)
shapiro.test(effort_overall$BorgRPEdiff) # W = 0.94719, p-value = 0.004427
shapiro.test(effort_overall$OmniRPEdiff) # W = 0.92331, p-value = 0.0003018

# shapiro.test(effort_overall$BorgRPEdiff[effort_overall$Work.Period=="Time1_after90minWork"])
# W = 0.93282, p-value = 0.1127
# shapiro.test(effort_overall$BorgRPEdiff[effort_overall$Work.Period=="Time2_after30minBreak"])
# W = 0.88968, p-value = 0.01309
# shapiro.test(effort_overall$BorgRPEdiff[effort_overall$Work.Period=="Time3_endWorkDay"])
# W = 0.91589, p-value = 0.04741
# shapiro.test(effort_overall$OmniRPEdiff[effort_overall$Work.Period=="Time1_after90minWork"])
# W = 0.88644, p-value = 0.01123
# shapiro.test(effort_overall$OmniRPEdiff[effort_overall$Work.Period=="Time2_after30minBreak"])
# W = 0.87846, p-value = 0.007739
# shapiro.test(effort_overall$OmniRPEdiff[effort_overall$Work.Period=="Time3_endWorkDay"])
# W = 0.94746, p-value = 0.2385

# Visualize data
ggboxplot(effort_overall, x = "Work.Period", y = "BorgRPE", color = "Activity",
       xlab = "Work Period", ylab = "Borg RPE",
       title = "Borg RPE through the work shift",
       add = c("mean_se", "dotplot"), palette = c("#1B9E77", "#D95F02", "#7570B3")) +
  grids(linetype = "dashed", color = "grey")
ggboxplot(effort_overall, x = "Work.Period", y = "OmniRPE", color = "Activity",
       xlab = "Work Period", ylab = "Omni RPE",
       title = "Omni RPE through the work shift",
       add = c("mean_se", "dotplot"), palette = c("#1B9E77", "#D95F02", "#7570B3")) +
  grids(linetype = "dashed", color = "grey")

# Statistical tests
anova(lm(BorgRPE ~ Activity + Work.Period, data = effort_overall))
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# Activity     2   1.86   0.931  0.1882    0.8289    
# Work.Period  2 181.86  90.931 18.3912 4.299e-07 ***
# Residuals   67 331.26   4.944  

poshoc <- aov(BorgRPE ~ Activity + Work.Period, data = effort_overall)
TukeyHSD(poshoc, "Work.Period") # p adj < 0.0001

anova(lm(OmniRPE ~ Activity + Work.Period, data = effort_overall))
#             Df  Sum Sq Mean Sq F value    Pr(>F)    
# activity     2  13.194   6.597  3.1364   0.04988 *  
# Work.Period  2 166.861  83.431 39.6638 4.316e-12 ***
# Residuals   67 140.931   2.103

poshoc <- aov(OmniRPE ~ Activity + Work.Period, data = effort_overall)
TukeyHSD(poshoc, "Work.Period") # p adj < 0.0001
TukeyHSD(poshoc, "Activity")
#                      diff        lwr      upr     p adj
# Ladder-Ground   0.6250000 -0.3785087 1.628509 0.3008179
# Platform-Ground 1.0416667  0.0381580 2.045175 0.0401574
# Platform-Ladder 0.4166667 -0.5868420 1.420175 0.5824390

# Revise post hoc nonparametric tests to Dunn's test of multiple comparison using rank sums
attach(effort_overall)
dunn.test(BorgRPEdiff,Activity)
# Kruskal-Wallis rank sum test
# 
# data: BorgRPEdiff and Activity
# Kruskal-Wallis chi-squared = 5.2793, df = 2, p-value = 0.07
# 
# Comparison of BorgRPEdiff by Activity                     
# (No adjustment)                                
# Col Mean-|
# Row Mean |     Ground     Ladder
# ---------+----------------------
#   Ladder |   0.732040
#          |     0.2321
#          |
# Platform |  -1.520122  -2.252163
#          |     0.0642    0.0122*
#   
# alpha = 0.05
# Reject Ho if p <= alpha/2
dunn.test(BorgRPEdiff,Work.Period)
# Kruskal-Wallis rank sum test
# 
# data: BorgRPEdiff and Work.Period
# Kruskal-Wallis chi-squared = 16.7758, df = 2, p-value = 0
# 
# Comparison of BorgRPEdiff by Work.Period                    
# (No adjustment)                                
# Col Mean-|
# Row Mean |   T1 after   T2 after
# ---------+----------------------
# T2 after |   0.633968
#          |     0.2631
#          |
# T3 end o |  -3.187354  -3.821322
#          |    0.0007*    0.0001*
#   
# alpha = 0.05
# Reject Ho if p <= alpha/2
dunn.test(OmniRPEdiff,Activity)
# Kruskal-Wallis rank sum test
# 
# data: OmniRPEdiff and Activity
# Kruskal-Wallis chi-squared = 4.7113, df = 2, p-value = 0.09
# 
# Comparison of OmniRPEdiff by Activity                     
# (No adjustment)                                
# Col Mean-|
# Row Mean |     Ground     Ladder
# ---------+----------------------
#   Ladder |   0.270003
#          |     0.3936
#          |
# Platform |  -1.730154  -2.000157
#          |     0.0418    0.0227*
#   
#   alpha = 0.05
# Reject Ho if p <= alpha/2
dunn.test(OmniRPEdiff,Work.Period)
# Kruskal-Wallis rank sum test
# 
# data: OmniRPEdiff and Work.Period
# Kruskal-Wallis chi-squared = 21.8908, df = 2, p-value = 0
# 
# Comparison of OmniRPEdiff by Work.Period                    
# (No adjustment)                                
# Col Mean-|
# Row Mean |   T1 after   T2 after
# ---------+----------------------
# T2 after |   0.142107
#          |     0.4435
#          |
# T3 end o |  -3.978999  -4.121106
#          |    0.0000*    0.0000*
#   
# alpha = 0.05
# Reject Ho if p <= alpha/2
detach(effort_overall)

# Extract local discomfort at the beginning and end of work, subtract for difference
effort_local_beg <- effort %>%
  filter(`Work Period` == "Start_Work") %>% 
  mutate(RightBeg = RightShoulder, LeftBeg = LeftShoulder) %>%
  select(Subject, Activity, RightBeg, LeftBeg)
effort_local_end <- effort %>%
  filter(`Work Period` == "End_Work") %>%
  mutate(RightEnd = RightShoulder, LeftEnd = LeftShoulder) %>%
  select(RightEnd, LeftEnd)
effort_local <- cbind.data.frame(effort_local_beg, effort_local_end) %>%
  mutate(Dominant = RightEnd-RightBeg, Nondominant = LeftEnd-LeftBeg) %>%
  select(Subject, Activity, Dominant, Nondominant)

# Swap left and right for 'mano izquierda': 706, 712, 716
effort_local$Dominant[12] <- 4
effort_local$Nondominant[12] <- 0

# Examine data
hist(effort_local$Dominant)
hist(effort_local$Nondominant)
ggqqplot(effort_local$Dominant)
ggqqplot(effort_local$Nondominant)
shapiro.test(effort_local$Dominant) # W = 0.84357, p-value = 0.001665
shapiro.test(effort_local$Nondominant) # W = 0.77655, p-value = 0.0001248

# Gather from wide format to long format, examine data, and run preliminary tests
effort_local <- effort_local %>% gather(key = Side, value = Value, -c(Subject, Activity))
hist(effort_local$Value)
ggqqplot(effort_local$Value)
kruskal.test(Value ~ Activity, data = effort_local) # chi-squared = 8.6515, df = 2, p-value = 0.01322
kruskal.test(Value ~ Side, data = effort_local) # chi-squared = 0.42581, df = 1, p-value = 0.5141

# Visualize data
ggboxplot(effort_local, x = "Activity", y = "Value", color = "Side",
       xlab = "Harvesting Method", ylab = "Borg CR10 Difference", 
       order = c("Ground", "Ladder", "Platform"), 
       palette = c("#0072B2", "#D55E00"),
       title = "Borg CR10 increases or decreases through the work shift") +
  grids(linetype = "dashed", color = "grey")
