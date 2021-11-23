library(readr)
library(ggpubr)
library(tidyverse)

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

# Extract coefficient or slope as changes in median power frequency
SubjList <- unique(medianPF$Subject) # missing Subj 707, 710, 712
MuscleList <- unique(medianPF$DominantSide) # "DM" "ND"

slope <- medianPF %>% 
  select(Subject, Equipment, DominantSide) %>% 
  group_by(Subject, Equipment, DominantSide) %>% 
  unique() %>% mutate(slope = NA)

for (subj in SubjList) {
  for (muscle in MuscleList) {
    y <- medianPF$Value[medianPF$Subject==subj & medianPF$DominantSide==muscle]
    if (length(y) != 0) {
      t <- 1:length(y)
      slope$slope[slope$Subject==subj & slope$DominantSide==muscle] <- lm(y ~ t)$coef[2]
    }
  }
}

hist(slope$slope, breaks = 5)
shapiro.test(slope$slope) # W = 0.94684, p-value = 0.05909

# Histogram showing data distribution
library(ggplot2)
ggplot(medianPF, aes(x=Value)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=2) +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram of electromyography median power frequency (MPF)", 
       x="Median Power Frequency (Hz)", y="Density") +
  theme_classic()
ggplot(slope, aes(x=slope)) + 
  geom_histogram(aes(y=..density..), color="black", fill="grey", 
                 breaks=seq(-0.6, 0.6, by=.2)) +
  # geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram of MPF-time regression slope by participant (N=20)", 
       x="Slope", y="Density") +
  theme_classic()
