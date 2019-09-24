##### The SIMR package - ExMeth3, Class4 #####

library(pacman)
p_load(lme4, tidyverse, simr)


#create model (simdata is part of the simr package)
model1 <- glmer(z ~ x + (1|g), family="poisson", data=simdata)

summary(model1)


#cheking the B for x
fixef(model1)["x"]

#change the B for x from -0.1 to -0.05
fixef(model1)["x"] <- -0.05

#run power analysis
powerSim(model1) #power around 33% (not enough)


#make model that increases the study with 20 years
model2 <- extend(model1, along = "x", n = 20)
powerSim(model2) #power around 96% (80% is what's necessary)



#identifying minimum sample size required
pc2 <- powerCurve(model2)
print(pc2) # the analysis shows that the experiment has to run for 16 years to have 80% power


#if we cannot extend with extra study years, we can extend study sites (g)
model3 <- extend(model1, along = "g", n = 15)
pc3 <- powerCurve(model3, along = "g") #to have 80% statistical power, we need 11 sites
plot(pc3)


#extend model to 5 observations per site per year (before = 1 site per year)
model4 <- extend(model1, within = "x+g", n = 5)
pc4 <- powerCurve(model4, within = "x+g", breaks = 1:5) #4 obervations per site per year gives 80%
print(pc4)



