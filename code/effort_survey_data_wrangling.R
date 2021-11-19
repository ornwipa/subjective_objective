library(readr)
library(tidyverse)
library(ggpubr)

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
                                            "Time1_after90minWork",
                                            ifelse(`Work Period`=="End_Lunch", 
                                                   "Time2_after30minBreak",
                                                   "Time3_endWorkDay")))

# Examine data
hist(effort_overall$BorgRPEdiff, breaks = 15)
hist(effort_overall$OmniRPEdiff, breaks = 10)
ggqqplot(effort_overall$BorgRPEdiff)
ggqqplot(effort_overall$OmniRPEdiff)
shapiro.test(effort_overall$BorgRPEdiff) # W = 0.94719, p-value = 0.004427
shapiro.test(effort_overall$OmniRPEdiff) # W = 0.92331, p-value = 0.0003018

shapiro.test(effort_overall$BorgRPEdiff[effort_overall$Work.Period=="Time1_after90minWork"])
# W = 0.93282, p-value = 0.1127
shapiro.test(effort_overall$BorgRPEdiff[effort_overall$Work.Period=="Time2_after30minBreak"])
# W = 0.88968, p-value = 0.01309
shapiro.test(effort_overall$BorgRPEdiff[effort_overall$Work.Period=="Time3_endWorkDay"])
# W = 0.91589, p-value = 0.04741
shapiro.test(effort_overall$OmniRPEdiff[effort_overall$Work.Period=="Time1_after90minWork"])
# W = 0.88644, p-value = 0.01123
shapiro.test(effort_overall$OmniRPEdiff[effort_overall$Work.Period=="Time2_after30minBreak"])
# W = 0.87846, p-value = 0.007739
shapiro.test(effort_overall$OmniRPEdiff[effort_overall$Work.Period=="Time3_endWorkDay"])
# W = 0.94746, p-value = 0.2385

# Visualize data
ggline(effort_overall, x = "Work.Period", y = "BorgRPE", color = "Activity",
       xlab = "Work Period", ylab = "Borg RPE Difference",
       title = "Borg RPE increases or decreases over work shift",
       add = c("mean_se", "dotplot"), palette = c("#1B9E77", "#D95F02", "#7570B3"))
ggline(effort_overall, x = "Work.Period", y = "OmniRPE", color = "Activity",
       xlab = "Work Period", ylab = "Omni RPE Difference",
       title = "Omni RPE increases or decreases over work shift",
       add = c("mean_se", "dotplot"), palette = c("#1B9E77", "#D95F02", "#7570B3"))

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
friedman.test(Value ~ Activity|Side, ddata = effort_local)

# Visualize data
ggboxplot(effort_local, x = "Activity", y = "Value", color = "Side",
       xlab = "Harvesting Method", ylab = "Borg CR10 Difference", 
       order = c("Ground", "Ladder", "Platform"), 
       palette = c("#0072B2", "#D55E00"),
       title = "Borg CR10 increases or decreases through the work shift")
