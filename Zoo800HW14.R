#####Zoology800 - Homework 14 - Maddy Teter 12/2/25

#Objective 1 

install.packages("lmtest")
library(lmtest)
#Load in classmates data from HW 11

cqdata <- read.csv("codyquirozsimulated_data_csq.csv")

#Part A - Run model with all terms including interaction
cq_model_1 <- lm(DOC_mgL ~ CH4_flux_umol * ReachType, data = cqdata)
summary(cq_model_1)
#Results: ReachTypeImpounded not significant

#Part B- Model with interaction term removed
cq_model_2 <- lm(DOC_mgL ~ CH4_flux_umol + ReachType, data = cqdata)
summary(cq_model_2)

#Part C- calculate NLL of each model 

LL_model1 <- logLik(cq_model_1)
NLL_model1 <- -as.numeric(LL_model1) #81.45

LL_model2 <- logLik(cq_model_2)
NLL_model2 <- -as.numeric(LL_model2) #124.60

#Model 1 has a smaller NLL value 

#Part D - use lmtest::lrtest

lrtest(cq_model_1, cq_model_2)
#Interaction model is significantly preferred 

#Objective 2

#Full model- DOC (y), CH4 x variable cont, reachtype x variable factor, and interaction
cq_model_1 <- lm(DOC_mgL ~ CH4_flux_umol * ReachType, data = cqdata)
summary(cq_model_1)

#Main effects - DOC (y), CH4 x variable cot, reachtype x variable factor
cq_model_2 <- lm(DOC_mgL ~ CH4_flux_umol + ReachType, data = cqdata)
summary(cq_model_2)

#X-variable 1 only model
cq_model_3 <- lm(DOC_mgL ~ CH4_flux_umol, data = cqdata)
summary(cq_model_3)

#x-variable 2 only model
cq_model_4 <- lm(DOC_mgL ~ ReachType, data = cqdata)
summary(cq_model_4)

#Intercept only model
cq_model_5 <- lm(DOC_mgL ~ 1, data = cqdata)
summary(cq_model_5)

#Calculate AIC for every model
AIC_table <- AIC(
  cq_model_1, cq_model_2, cq_model_3, cq_model_4, cq_model_5)

#Create row name in AIC_table to match model names
row.names(AIC_table) <- c(
  "cq_model_1", "cq_model_2", "cq_model_3", "cq_model_4", "cq_model_5")

#View table
AIC_table
#Model 1 (interaction and both x variables included) has the lowest AIC, models continually increase AIC as
#model becomes more simple. This means that both methane and reach type (location of stream) are important
#influencers of methane production, and the relationship varies significantly between the different reach
#types (free flowing and impounded).