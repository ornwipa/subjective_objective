library(readr)
library(tidyverse)
library(ggpubr)

# Import age data
edad <- read_csv("data/edad.csv", 
                 col_types = cols(Subject = col_character()))
View(edad)
summary(edad)
hist(edad$Edad) # skewed

# Impute one missing age with median age
edad <- transform(edad, Edad = ifelse(is.na(Edad), # checking condition
                                      median(Edad, na.rm=TRUE), # value if true
                                      Edad)) # value if false
summary(edad$Edad)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   24.00   27.00   28.38   30.00   47.00 

# Calculate maximum heart rate
hr_max <- edad %>% 
  mutate(MaxHR = 200 - Edad) %>% select(Subject, MaxHR)

# Import heart rate data
Polar <- read_csv("data/Polar_HR_2014_Analysis.csv", 
                  col_types = cols(Subject = col_character()))
View(Polar)
summary(Polar)

# Extract sitting heart rate for estimating resting heart rate
# https://journals.lww.com/jhypertension/Abstract/2019/10000/Resting_heart_rate_in_the_supine_and_sitting.16.aspx
# https://www.sciencedirect.com/science/article/abs/pii/S1360859204000245
hr_rest <- Polar %>% 
  filter(`Work Period` == "Start_Work") %>% 
  mutate(RestingHR = Median - 4) %>% select(Subject, RestingHR)
summary(hr_rest$RestingHR)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 55.65   66.62   71.63   71.51   77.00   86.46 

# Extract working heart rate and rename work period
hr_work <- Polar %>%
  filter(`Work Period` != "Start_Work") %>%
  mutate(WorkingHR = Median) %>% select(Subject, Activity, `Work Period`, WorkingHR)
hr_work <- transform(hr_work, 
                     `Work Period` = ifelse(`Work Period`=="Start_Lunch", 
                                            "Time1_after90minWork",
                                            ifelse(`Work Period`=="End_Lunch", 
                                                   "Time2_after30minBreak",
                                                   "Time3_endWorkDay")))
summary(hr_work$WorkingHR)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 62.25   89.70  102.32  103.04  117.64  162.65

# Calculate percent heart rate reserve
hrr <- left_join(left_join(hr_work, 
                           hr_rest, by = "Subject", all = TRUE), 
                 hr_max, by = "Subject", all = TRUE) %>%
  mutate(pHRR = (WorkingHR-RestingHR)/(MaxHR-RestingHR))

# Impute errors (2 data points of negative percent) with 0.01% or 0.0001
# hrr <- transform(hrr, pHRR = ifelse(pHRR < 0, 0.0001, pHRR))

summary(hrr$pHRR)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02292 0.16553 0.29725 0.31610 0.42997 0.94577 
hist(hrr$pHRR)
ggqqplot(hrr$pHRR)
shapiro.test(hrr$pHRR) # W = 0.95614, p-value = 0.01344
