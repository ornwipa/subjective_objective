library(readr)
library(ggpubr)

# Import and preliminary analyze MPF changes over time
medianPF <- read_csv("data/medianPF_roll10min_removedbreak.csv")

anova(lm(Value ~ TimeAdjusted + DominantSide + Subject, data = medianPF))
# Response: Value
#                Df Sum Sq Mean Sq  F value    Pr(>F)    
# TimeAdjusted    1    797   797.2  50.3930 2.142e-12 ***
# DominantSide    1     88    88.4   5.5912   0.01821 *  
# Subject        20  64825  3241.3 204.9011 < 2.2e-16 ***
# Residuals    1208  19109    15.8 

lm(Value ~ TimeAdjusted + DominantSide + Subject, data = medianPF)$coef[1:2]
#  (Intercept) TimeAdjusted 
# 52.774188197 -0.005589083 

hist(medianPF$Value, breaks = 20) # bimodal in all equipment and dominant side
shapiro.test(log(medianPF$Value)) # W = 0.99457, p-value = 0.0001958

# Proof of concept in one subject and one muscle
y <- medianPF$Value[medianPF$Subject=="Subj701" & medianPF$DominantSide=="ND"]
t <- 1:length(y)
lm(y ~ t)
# (Intercept)            t  
#    51.97315     -0.09291

extractCoef <- function()

# Extract coefficient or slope as changes in median power frequency
SubjList <- unique(medianPF$Subject) # missing Subj 707, 710, 712
MuscleList <- unique(medianPF$DominantSide) # "DM" "ND"

for (subj in SubjList) {
  for (muscle in MuscleList) {
    y <- medianPF$Value[medianPF$Subject==subj & medianPF$DominantSide==muscle]
    if (length(y) != 0) {
      t <- 1:length(y)
      print(lm(y ~ t)$coef[2])
    }
  }
}
