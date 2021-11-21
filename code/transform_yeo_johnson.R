# install.packages("VGAM")
library(VGAM)

hist(effort_overall$BorgRPEdiff, breaks = 5)
hist(yeo.johnson(effort_overall$BorgRPEdiff, lambda = 0.85), breaks = 10)
ggqqplot(yeo.johnson(effort_overall$BorgRPEdiff, lambda = 0.85))
shapiro.test(yeo.johnson(effort_overall$BorgRPEdiff, lambda = 0.85))

hist(effort_overall$OmniRPEdiff, breaks = 5)
hist(yeo.johnson(effort_overall$OmniRPEdiff, lambda = 0.85), breaks = 10)
ggqqplot(yeo.johnson(effort_overall$OmniRPEdiff, lambda = 0.85))
shapiro.test(yeo.johnson(effort_overall$OmniRPEdiff, lambda = 0.85))

hist(effort_local$Value, breaks = 5)
hist(yeo.johnson(effort_local$Value, lambda = 0.05), breaks = 10)
ggqqplot(yeo.johnson(effort_local$Value, lambda = 0.05))
shapiro.test(yeo.johnson(effort_local$Value, lambda = 0.05))
