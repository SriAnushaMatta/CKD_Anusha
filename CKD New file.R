#Load long data
longdata <- read.csv("/Users/srianushamatta/Desktop/Others/Career/dataScienceTask/Longdata.csv", header = T)
longdata2 <- longdata
#Change anything required to factors:
longdata$Gender <- factor(longdata$Gender, levels = c(1,2), labels = c("Male","Female") )
longdata$Race <- factor(longdata$Race, levels = c(1,2,3,4,9), labels = c("Asian","Black", "Hispanic","White","Unknown") )
#View to make sure nothing is wiped out
table(longdata$Gender)
table(longdata$Race)

# Change to integer

longdata$Glucose <- as.numeric(longdata$Glucose)
longdata$Creatinine <- as.numeric(longdata$Creatinine)
longdata$LDL <- as.numeric(longdata$LDL)
longdata$HBG <- as.numeric(longdata$HBG)
longdata$SBP <- as.numeric(longdata$SBP)
longdata$DBP <- as.numeric(longdata$DBP)
longdata$ID <- as.factor(longdata$ID)
str(longdata)


summary(longdata) 
#Check for outliers
#check correlation
#Correlation among numerical columns only (not agr, gender etc)
correlation_table <- cor(longdata[ , -c(1,2,3,10,11,12,13)], use = "pairwise.complete.obs")
correlation_table
symnum(correlation_table)

library(ggplot2)
#Draw panel plots, example codes 

ggplot(longdata, aes(x = Glucose, y = Time))+ geom_point() + facet_wrap( ~ ID)
ggplot(longdata, aes(x = Glucose, y = Time))+ geom_point() + facet_wrap( ~ Race)
ggplot(longdata, aes(x = Glucose, y = Time))+ geom_point() + facet_wrap( ~ Status)


#Plot the data against Gender and Status 

ggplot(longdata, aes(x = Gender, y = Glucose))+ geom_point() + facet_wrap( ~Status )
ggplot(longdata, aes(x = Gender, y = Creatinine))+ geom_point() + facet_wrap( ~Status )
ggplot(longdata, aes(x = Gender, y = LDL))+ geom_point() + facet_wrap( ~Status )
ggplot(longdata, aes(x = Gender, y = HBG))+ geom_point() + facet_wrap( ~Status )
ggplot(longdata, aes(x = Gender, y = SBP))+ geom_point() + facet_wrap( ~Status )
ggplot(longdata, aes(x = Gender, y = DBP))+ geom_point() + facet_wrap( ~Status )


#Plot the data against time and race 
ggplot(longdata, aes(x = Creatinine, y = Time)) + facet_wrap( ~ Race) + geom_point()
ggplot(longdata, aes(x = Glucose, y = Time)) + facet_wrap( ~ Race) + geom_point()
ggplot(longdata, aes(x = LDL, y = Time)) + facet_wrap( ~ Race) + geom_point()
ggplot(longdata, aes(x = HBG, y = Time)) + facet_wrap( ~ Race) + geom_point()



#Draw a  panel plot against time
ggplot(longdata, aes(x = Time, y = Glucose),plot.title = element_text(size=2))+ geom_point() + facet_wrap( ~ ID) 

ggplot(longdata, aes(x = Time, y = Creatinine))+ geom_point() + facet_wrap( ~ ID)
ggplot(longdata, aes(x = Time, y = LDL))+ geom_point() + facet_wrap( ~ ID)
ggplot(longdata, aes(x = Time, y = HBP))+ geom_point() + facet_wrap( ~ ID)
ggplot(longdata, aes(x = Time, y = SBP))+ geom_point() + facet_wrap( ~ ID)


#Descriptive statistics in a grouped manner
install.packages("psych") # Package to make group statistics easier
library(psych)
describeBy(longdata,group = "Gender")
describeBy(longdata,group = "ID")
describeBy(longdata,group = "Race")
describeBy(longdata,group = "Status")

#Correlation among numerical columns only (not agr, gender etc)
correlation_table <- cor(longdata[ , -c(1,2,3,10,11,12,13)], use = "pairwise.complete.obs")
correlation_table
symnum(correlation_table)
plot(correlation_table)

#Using the aggregate function to describe the mean of data by a factor (Status in this case)
aggregate(x = longdata[ ,4:9], by = list(longdata$Status), mean, na.rm = TRUE)
aggregate(x = imp_com3[ ,4:9], by = list(imp_com3$Status), mean, na.rm = TRUE)

aggregate(x = longdata[ ,4:9], by = list(longdata$Status, longdata$Gender), mean, na.rm = TRUE)
aggregate(x = imp_com3[ ,4:9], by = list(imp_com3$Status, imp_com3$Gender), mean, na.rm = TRUE)

aggregate(x = longdata[ ,4:9], by = list(longdata$Status, longdata$Gender, longdata$Race), mean, na.rm = TRUE)
aggregate(x = imp_com3[ ,4:9], by = list(imp_com3$Status, imp_com3$Gender, imp_com3$Race), mean, na.rm = TRUE)



#Attempting imputation of data
#Imputing data
#How to impute data for a longitudinal dataset?
# https://nerler.github.io/EP16_Multiple_Imputation/practical/07_Imputation_of_Longitudinal_Data.html
# FCS LMM Het
# Repeated measurements of time-dependent variables are imputed using hierarchical models.
# Assumes a conditional LMM for each incomplete variable.
# Binary and categorical variables are imputed as continuous variables.
# The model assumes a subject-specific residual error variance

library(mice)

(mice.impute.2l.norm)
#Older method used

longdataimp <- longdata
longdataimp$ID <- as.integer(longdataimp$ID) #For mice, variable ID should remain an integer


ini <- mice(longdataimp,maxit = 0)
pred <- ini$pred
pred["Glucose", ] <- c(-2,0,2,0,0,0,0,0,0,2,2,2,0) #Don't use 2 for the column to be imputed)
#Change ID to integer for the MICE imputation
#longdataimp$ID <- as.integer(longdataimp$ID)

imp_testG <- mice(longdataimp, meth = c("","","","2l.norm","","","","","","","","",""), pred = pred, maxit = 1)
imp_com <- complete(imp_testG)


pred["Creatinine", ] <- c(-2,0,2,0,0,0,0,0,0,2,2,2,0) #Don't use 2 for the column to be imputed)

imp_testGC <- mice(imp_com, meth = c("","","","","2l.norm","","","","","","","",""), pred = pred, maxit = 1)
imp_com <- complete(imp_testGC)

pred["LDL", ] <- c(-2,0,2,0,0,0,0,0,0,2,2,2,0) #Don't use 2 for the column to be imputed)

imp_testGCL <- mice(imp_com, meth = c("","","","","","2l.norm","","","","","","",""), pred = pred, maxit = 1)
imp_com <- complete(imp_testGCL)

pred["HBG", ] <- c(-2,0,2,0,0,0,0,0,0,2,2,2,0) #Don't use 2 for the column to be imputed)

imp_testGCLH <- mice(imp_com, meth = c("","","","","","","2l.norm","","","","","",""), pred = pred, maxit = 1)
imp_com <- complete(imp_testGCLH)

pred["SBP", ] <- c(-2,0,2,0,0,0,0,0,0,2,2,2,0) #Don't use 2 for the column to be imputed)

imp_testGCLHS <- mice(imp_com, meth = c("","","","","","","","2l.norm","","","","",""), pred = pred, maxit = 1)
imp_com <- complete(imp_testGCLHS)


pred["DBP", ] <- c(-2,0,2,0,0,0,0,0,0,2,2,2,0) #Don't use 2 for the column to be imputed)
imp_testGCLHSD <- mice(imp_com, meth = c("","","","","","","","","2l.norm","","","",""), pred = pred, maxit = 1)
imp_com <- complete(imp_testGCLHSD)

imp_com1 <- imp_com

View(imp_com)

#Iterate and identify the best imputation

#imp_com3 was selected  and duplicated as imp_com4
imp_com3[imp_com3 < 0] <- 0 #only non negative values

#Loading packages required for modelling the data
library(nlme)
library(lme4)


#Using the nlme package

lmemod <-lme(Status ~ 1, data=longdata, random=~1|ID, na.action = na.omit, method = "ML")
summary(lmemod)
intervals(lmemod)


lmemod2_G <-lme(Status ~ Glucose , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")
lmemod2_C <-lme(Status ~ Creatinine , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")
lmemod2_L <-lme(Status ~ LDL , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")
lmemod2_H <-lme(Status ~ HBG , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")
lmemod2_S <-lme(Status ~ SBP , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")
lmemod2_D <-lme(Status ~ DBP , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")
lmemod2_Age <-lme(Status ~ Age , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")
lmemod2_Gender <-lme(Status ~ Gender , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")
lmemod2_Race <-lme(Status ~ Race , random=~1|ID, data=longdata, na.action = na.omit, method = "ML")

#Random effects model
lmemod2_G <-lme(Status ~ Glucose , random=~Glucose|ID, data=longdata, na.action = na.omit, method = "ML")



#Imputed data

lmemod_imputed <-lme(Status ~ 1, data=imp_com3, random=~1|ID, na.action = na.omit, method = "ML")
summary(lmemod_imputed)
intervals(lmemod_imputed)


lmemod_imputed_G <-lme(Status ~ Glucose , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")
lmemod_imputed_C <-lme(Status ~ Creatinine , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")
lmemod_imputed_L <-lme(Status ~ LDL , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")
lmemod_imputed_H <-lme(Status ~ HBG , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")
lmemod_imputed_S <-lme(Status ~ SBP , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")
lmemod_imputed_D <-lme(Status ~ DBP , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")
lmemod_imputed_Age <-lme(Status ~ Age , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")
lmemod_imputed_Gender <-lme(Status ~ Gender , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")
lmemod_imputed_Race <-lme(Status ~ Race , random=~1|ID, data=imp_com3, na.action = na.omit, method = "ML")

#Random effects model
lmemod_imputed_G <-lme(Status ~ Glucose , random=~Glucose|ID, data=imp_com3, na.action = na.omit, method = "ML")



#gmle model

logmod1 <-glmer(Status ~ Age + Race + Glucose + Creatinine + HBG + (1|ID), data=longdata, na.action = na.omit, family = binomial)

#glme model with control

logmod1con <-glmer(Status ~ Age + Race + Glucose + Creatinine + HBG + (1|ID), data=longdata, na.action = na.omit, family = binomial,control=glmerControl(optimizer="bobyqa"))

#glme with control and nAGQ

logmod1connAGQ <-glmer(Status ~ Age + Race + Gender + Glucose + Creatinine + HBG + (1|ID), data=longdata, na.action = na.omit, family = binomial,nAGQ=10, control=glmerControl(optimizer="bobyqa"))
summary(logmod1con)
se <- sqrt(diag(vcov(logmod1con)))

#Confidence Intervals of the SE
tab <- cbind(Est = fixef(logmod1), LL = fixef(logmod1con) - 1.96 * se, UL = fixef(logmod1con) + 1.96 * se)

# Odds ratios instead of coefficients on the logit scale, we could exponentiate the estimates and CIs
exp(tab)




#Bootstrapping

sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}


#Now we will resample our data and take 100 replicates. 
#Again in practice you would probably take thousands. 
#We set the seed so that our results are reproducible. 
#It is also likely that you will need to sample more replicates than you ultimately want because many samples may not converge so you do not get estimates from them.

set.seed(20)
tmp <- sampler(longdata, "ID", reps = 10)
bigdata <- cbind(tmp, longdata[tmp$RowID, ])


#Next we refit the model on the resampled data.
#First we store the estimates from our original model, which we will use as start values for the bootstrap models. 
# Then we make a local cluster with 4 nodes (the number of processors on our machine; set to the number of processors you have on yours).
#Next, we export the data and load the lme4 package on the cluster. 
#Finally, we write a function to fit the model and return the estimates. 
#The call to glmer() is wrapped in try because not all models may converge on the resampled data. 
#This catches the error and returns it, rather than stopping processing.

f <- fixef(logmod1)
r <- getME(logmod1, "theta")

#OR
#set.seed(20)
#tmp <- sampler(imp_com3, "ID", reps = 10)
#bigdataimp <- cbind(tmp, imp_com3[tmp$RowID, ])
#f <- fixef(Glmermod_imputed1)
#r <- getME(Glmermod_imputed1, "theta")



library(parallel)
cl <- makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))


#Next 
myboot <- function(i) {
  object <- try(glmer(Status ~ Age + Race + Glucose + Creatinine + HBG + SBP + DBP (1|ID), data = bigdata#bigdataimp#
                      , subset = Replicate == i, family = binomial,
                      nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
  if (class(object) == "try-error")
    return(object)
  c(fixef(object), getME(object, "theta"))
}
##Now that we have the data, the local cluster, and the fitting function setup,we are ready to actually do the bootstrapping. 
#To do this, we use the parLapplyLB function, which loops through every replicate, giving them out to each node of the cluster to estimate the models.
#The “LB” stands for load balancing, which means replicates are distributed as a node completes its current job. 
#This is valuable because not all replicates will converge, and if there is an error and it happens early on, one node may be ready for a new job faster than another node.
#There is some extra communication overhead, but this is small compared to the time it takes to fit each model. 
#The results from all nodes are aggregated back into a single list, stored in the object res.
#Once that is done, we can shut down the local cluster, which terminates the additional R instances and frees memory.

start <- proc.time()
res <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = myboot)
#res <- parLapplyLB(cl, X = levels(bigdataimp$Replicate), fun = myboot)
end <- proc.time()

# shut down the cluster
stopCluster(cl)

#Now that we have the bootstrap results, we can summarize them. 
#First, we calculate the number of models that successfully converged. 
# We do this by checking whether a particular result is numeric or not. 
# Errors are not numeric, so they will be skipped. 
# We can calculate the mean of the successes to see the proportion of 
# replicates that converged and that we have results for.



# calculate proportion of models that successfully converged
success <- sapply(res, is.numeric)
mean(success)

# combine successful results
bigres <- do.call(cbind, res[success])

#bigresimp <- do.call(cbind, res[success])

# calculate 2.5th and 97.5th percentiles for 95% CI
(ci <- t(apply(bigres, 1, quantile, probs = c(0.025, 0.975))))
(ci <- t(apply(bigresimp, 1, quantile, probs = c(0.025, 0.975))))

# All results
finaltable <- cbind(Est = c(f, r), SE = c(se, NA), BootMean = rowMeans(bigres),
                    ci)
# round and print
round(finaltable, 3)

#Failed with imputed data as well.

library(ggeffects)


m1_com <- glmer(
  Status ~ Glucose + Gender + (1 | ID), 
  data = imp_com, 
  family = binomial(link = "logit")
)


# Trying with all imputed data
m1_imp<- glmer(
  Status ~ Glucose + Creatinine + LDL + HBG + SBP + DBP + Gender + Race + (1 | ID),
  data = imp_com3,
  family = binomial(link = "logit")
)




ggpredict(m1, "Gender") #%>% plot()
ggpredict(m1, "Glucose") %>% plot()
ggpredict(m1, "Creatinine") %>% plot()


ggpredict(m1, c("Glucose", "Gender")) %>% plot()
ggpredict(m1, c("Gender", "Glucose")) %>% plot()


ggpredict(m1_com, "Gender")
ggpredict(m1_com, "Glucose")

ggpredict(m1_com, c("Glucose", "Gender")) %>% plot()
ggpredict(m1_com, c("Creatinine", "Gender")) %>% plot()
ggpredict(m1_com, c("LDL", "Gender")) %>% plot()
ggpredict(m1_com, c("HBG", "Gender")) %>% plot()
ggpredict(m1_com, c("SBP", "Gender")) %>% plot()
ggpredict(m1_com, c("DBP", "Gender")) %>% plot()

#Plot by Status
ggplot(longdata, aes(x = ID, y = LDL))+ geom_point() + facet_wrap( ~ Status)
ggplot(longdata, aes(x = ID, y = Glucose))+ geom_point() + facet_wrap( ~ Status)
ggplot(imp_com3, aes(x = ID, y = Glucose))+ geom_point() + facet_wrap( ~ Status)


#Subset the data to run t tests
subset_Case <- subset(longdata, Status == "0")
subset_Control <- subset(longdata, Status == "1")

subset_Case_Male <- subset(subset_Case, Gender == "Male")
subset_Case_Female <- subset(subset_Case, Gender == "Female")
subset_Control_Male <- subset(subset_Control, Gender == "Male")
subset_Control_Female <- subset(subset_Control, Gender == "Female")

subset_Case_Male_Asian <- subset(subset_Case_Male, Race == "Asian")
subset_Case_Female_Asian <- subset(subset_Case_Female, Race == "Asian")
subset_Control_Female_Asian <- subset(subset_Control_Female, Race == "Asian")
subset_Control_Male_Asian <- subset(subset_Control_Male, Race == "Asian")

subset_Case_Male_Black <- subset(subset_Case_Male, Race == "Black")
subset_Case_Female_Black <- subset(subset_Case_Female, Race == "Black")
subset_Control_Female_Black <- subset(subset_Control_Female, Race == "Black")
subset_Control_Male_Black <- subset(subset_Control_Male, Race == "Black")

subset_Case_Male_White <- subset(subset_Case_Male, Race == "White")
subset_Case_Female_White <- subset(subset_Case_Female, Race == "White")
subset_Control_Female_White <- subset(subset_Control_Female, Race == "White")
subset_Control_Male_White <- subset(subset_Control_Male, Race == "White")

subset_Case_Male_Hispanic <- subset(subset_Case_Male, Race == "Hispanic")
subset_Case_Female_Hispanic <- subset(subset_Case_Female, Race == "Hispanic")
subset_Control_Female_Hispanic <- subset(subset_Control_Female, Race == "Hispanic")
subset_Control_Male_Hispanic <- subset(subset_Control_Male, Race == "Hispanic")

subset_Case_Male_Unknown <- subset(subset_Case_Male, Race == "Unknown")
subset_Case_Female_Unknown <- subset(subset_Case_Female, Race == "Unknown")
subset_Control_Female_Unknown <- subset(subset_Control_Female, Race == "Unknown")
subset_Control_Male_Unknown <- subset(subset_Control_Male, Race == "Unknown")

#t-tests

t.test(subset_Case_Male_Asian$Glucose, subset_Control_Male_Asian$Glucose, conf.level = 0.95)
t.test(subset_Case_Male_Asian$Creatinine, subset_Control_Male_Asian$Creatinine, conf.level = 0.95)
t.test(subset_Case_Male_Asian$LDL, subset_Control_Male_Asian$LDL, conf.level = 0.95)
t.test(subset_Case_Male_Asian$HBG, subset_Control_Male_Asian$HBG, conf.level = 0.95)
t.test(subset_Case_Male_Asian$SBP, subset_Control_Male_Asian$SBP, conf.level = 0.95)
t.test(subset_Case_Male_Asian$DBP, subset_Control_Male_Asian$DBP, conf.level = 0.95)

t.test(subset_Case_Female_Asian$Glucose, subset_Control_Female_Asian$Glucose, conf.level = 0.95)
t.test(subset_Case_Female_Asian$Creatinine, subset_Control_Female_Asian$Creatinine, conf.level = 0.95)
t.test(subset_Case_Female_Asian$LDL, subset_Control_Female_Asian$LDL, conf.level = 0.95)
t.test(subset_Case_Female_Asian$HBG, subset_Control_Female_Asian$HBG, conf.level = 0.95)
t.test(subset_Case_Female_Asian$SBP, subset_Control_Female_Asian$SBP, conf.level = 0.95)
t.test(subset_Case_Female_Asian$DBP, subset_Control_Female_Asian$DBP, conf.level = 0.95)

t.test(subset_Case_Male_Black$Glucose, subset_Control_Male_Black$Glucose, conf.level = 0.95)
t.test(subset_Case_Male_Black$Creatinine, subset_Control_Male_Black$Creatinine, conf.level = 0.95)
t.test(subset_Case_Male_Black$LDL, subset_Control_Male_Black$LDL, conf.level = 0.95)
t.test(subset_Case_Male_Black$HBG, subset_Control_Male_Black$HBG, conf.level = 0.95)
t.test(subset_Case_Male_Black$SBP, subset_Control_Male_Black$SBP, conf.level = 0.95)
t.test(subset_Case_Male_Black$DBP, subset_Control_Male_Black$DBP, conf.level = 0.95)

t.test(subset_Case_Female_Black$Glucose, subset_Control_Female_Black$Glucose, conf.level = 0.95)
t.test(subset_Case_Female_Black$Creatinine, subset_Control_Female_Black$Creatinine, conf.level = 0.95)
t.test(subset_Case_Female_Black$LDL, subset_Control_Female_Black$LDL, conf.level = 0.95)
t.test(subset_Case_Female_Black$HBG, subset_Control_Female_Black$HBG, conf.level = 0.95)
t.test(subset_Case_Female_Black$SBP, subset_Control_Female_Black$SBP, conf.level = 0.95)
t.test(subset_Case_Female_Black$DBP, subset_Control_Female_Black$DBP, conf.level = 0.95)

t.test(subset_Case_Male_White$Glucose, subset_Control_Male_White$Glucose, conf.level = 0.95)
t.test(subset_Case_Male_White$Creatinine, subset_Control_Male_White$Creatinine, conf.level = 0.95)
t.test(subset_Case_Male_White$LDL, subset_Control_Male_White$LDL, conf.level = 0.95)
t.test(subset_Case_Male_White$HBG, subset_Control_Male_White$HBG, conf.level = 0.95)
t.test(subset_Case_Male_White$SBP, subset_Control_Male_White$SBP, conf.level = 0.95)
t.test(subset_Case_Male_White$DBP, subset_Control_Male_White$DBP, conf.level = 0.95)

t.test(subset_Case_Female_White$Glucose, subset_Control_Female_White$Glucose, conf.level = 0.95)
t.test(subset_Case_Female_White$Creatinine, subset_Control_Female_White$Creatinine, conf.level = 0.95)
t.test(subset_Case_Female_White$LDL, subset_Control_Female_White$LDL, conf.level = 0.95)
t.test(subset_Case_Female_White$HBG, subset_Control_Female_White$HBG, conf.level = 0.95)
t.test(subset_Case_Female_White$SBP, subset_Control_Female_White$SBP, conf.level = 0.95)
t.test(subset_Case_Female_White$DBP, subset_Control_Female_White$DBP, conf.level = 0.95)

t.test(subset_Case_Male_Hispanic$Glucose, subset_Control_Male_Hispanic$Glucose, conf.level = 0.95)
t.test(subset_Case_Male_Hispanic$Creatinine, subset_Control_Male_Hispanic$Creatinine, conf.level = 0.95)
t.test(subset_Case_Male_Hispanic$LDL, subset_Control_Male_Hispanic$LDL, conf.level = 0.95)
t.test(subset_Case_Male_Hispanic$HBG, subset_Control_Male_Hispanic$HBG, conf.level = 0.95)
t.test(subset_Case_Male_Hispanic$SBP, subset_Control_Male_Hispanic$SBP, conf.level = 0.95)
t.test(subset_Case_Male_Hispanic$DBP, subset_Control_Male_Hispanic$DBP, conf.level = 0.95)

t.test(subset_Case_Female_Hispanic$Glucose, subset_Control_Female_Hispanic$Glucose, conf.level = 0.95)
t.test(subset_Case_Female_Hispanic$Creatinine, subset_Control_Female_Hispanic$Creatinine, conf.level = 0.95)
t.test(subset_Case_Female_Hispanic$LDL, subset_Control_Female_Hispanic$LDL, conf.level = 0.95)
t.test(subset_Case_Female_Hispanic$HBG, subset_Control_Female_Hispanic$HBG, conf.level = 0.95)
t.test(subset_Case_Female_Hispanic$SBP, subset_Control_Female_Hispanic$SBP, conf.level = 0.95)
t.test(subset_Case_Female_Hispanic$DBP, subset_Control_Female_Hispanic$DBP, conf.level = 0.95)

t.test(subset_Case_Male_Unknown$Glucose, subset_Control_Male_Unknown$Glucose, conf.level = 0.95)
t.test(subset_Case_Male_Unknown$Creatinine, subset_Control_Male_Unknown$Creatinine, conf.level = 0.95)
t.test(subset_Case_Male_Unknown$LDL, subset_Control_Male_Unknown$LDL, conf.level = 0.95)
t.test(subset_Case_Male_Unknown$HBG, subset_Control_Male_Unknown$HBG, conf.level = 0.95)
t.test(subset_Case_Male_Unknown$SBP, subset_Control_Male_Unknown$SBP, conf.level = 0.95)
t.test(subset_Case_Male_Unknown$DBP, subset_Control_Male_Unknown$DBP, conf.level = 0.95)

t.test(subset_Case_Female_Unknown$Glucose, subset_Control_Female_Unknown$Glucose, conf.level = 0.95)
t.test(subset_Case_Female_Unknown$Creatinine, subset_Control_Female_Unknown$Creatinine, conf.level = 0.95)
t.test(subset_Case_Female_Unknown$LDL, subset_Control_Female_Unknown$LDL, conf.level = 0.95)
t.test(subset_Case_Female_Unknown$HBG, subset_Control_Female_Unknown$HBG, conf.level = 0.95)
t.test(subset_Case_Female_Unknown$SBP, subset_Control_Female_Unknown$SBP, conf.level = 0.95)
t.test(subset_Case_Female_Unknown$DBP, subset_Control_Female_Unknown$DBP, conf.level = 0.95)

t.test(subset_Male$HBG, subset_Female$HBG, conf.level = 0.95)
t.test(subset_Case_Male$LDL, subset_Case_Female$LDL, conf.level = 0.95)
t.test(subset_Control_Male$LDL, subset_Control_Female$LDL, conf.level = 0.95)



