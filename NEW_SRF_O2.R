###############################################
## NEW SRF O2
###############################################

##### Libraries ######
library(dplyr)
library(nlme)


csv_files <-
  list.files(
    path = "/Users/claravetter/Desktop/BabylabanalysenNEW/BaselineObject21DiffQ",
    pattern = ".*SRF.*.csv",
    full.names = T,
    recursive = F
  )

df_all <- read.csv(csv_files[1], header = T)

for (i in 2:length(csv_files)) {
  df <- read.csv(csv_files[i], header = T)
  if (nrow(df)>0){
    df_all <- bind_rows(df_all, df)
  }
}

write.csv(df_all, "/Users/claravetter/Desktop/BabylabanalysenNEW/SRF_O2_Model.csv")

df_all <- read.csv("/Users/claravetter/Desktop/BabylabanalysenNEW/SRF_O2_Model.csv", header = T)

tr <- vector()
for (i in 1:length(unique(df_all$Subject))){
  tr <- append(tr,length(unique(df_all$TrialID[df_all$Subject==unique(df_all$Subject)[i]])))
}
mean(tr) # on average, infants contributed with 6.88 trials
sd(tr) # 4.40
range(tr) # 1, 15


df1 <- subset(df_all, !is.na(df_all$BaseTime))
df1$ModelEmo <- as.factor(df1$ModelEmo)


df1$ModelEmo<-relevel(df1$ModelEmo, "Neutral")

df1$TrialID <- df1$TrialID-1
df2 <- select(df1, TrialID, AverageDiameterPupils, BaseTime, Sex, ModelEmo,
              Subject, mother.anxiety, father.anxiety, mother.infant.temp,
              father.infant.temp, ModelLook,ObjectPos)

n <- length(unique(df_all$Subject)) # 16
r <- length(df2)*40*n*16

df3 <- matrix(c(rep.int(NA,r)),nrow=40*n*16,ncol=length(df2))
df3 <- as.data.frame(df3)
colnames(df3) <- colnames(df2)



#df2$BaseTime <- round_any(df2$BaseTime,50)


trials <- rep(c(0:15),n,each=40)
subjects <- rep(unique(df2$Subject),each=40*16)
times <- rep(c(0:39),n*16)
df3$TrialID <- trials
df3$Subject <- subjects
df3$BaseTime <- times*50

#df3 <- df1

for (i in 1:length(df3$Subject)){
  subj <- df3$Subject[i] 
  tri <- df3$TrialID[i]
  tim <- df3$BaseTime[i]
  
  if (length(df2$AverageDiameterPupils[df2$Subject == subj & df2$TrialID == tri& df2$BaseTime == tim]) > 0){
    df3$AverageDiameterPupils[i] <- df2$AverageDiameterPupils[df2$Subject == subj & df2$TrialID == tri & df2$BaseTime == tim]
    
    df3$Sex[i] <- df2$Sex[df2$Subject == subj & df2$TrialID == tri & df2$BaseTime == tim]
  }
  if ((length(df2$ModelEmo[df2$Subject == subj & df2$TrialID == tri])>0)){
    df3$ModelEmo[i] <- (unique(df2$ModelEmo[df2$Subject == subj & df2$TrialID == tri])[1])
  }
  if ((length(df2$ModelLook[df2$Subject == subj & df2$TrialID == tri])>0)){
    df3$ModelLook[i] <- (unique(df2$ModelLook[df2$Subject == subj & df2$TrialID == tri])[1])
  }
  if ((length(df2$ObjectPos[df2$Subject == subj & df2$TrialID == tri])>0)){
    df3$ObjectPos[i] <- (unique(df2$ObjectPos[df2$Subject == subj & df2$TrialID == tri])[1])
  }
  if ((length(df2$Sex[df2$Subject == subj])>0)){
    df3$Sex[i] <- unique(df2$Sex[df2$Subject == subj])[1]
  }
  if ((length(df2$mother.anxiety[df2$Subject == subj])>0)){
    df3$mother.anxiety[i] <- unique(df2$mother.anxiety[df2$Subject == subj])[1]
  }
  if ((length(df2$father.anxiety[df2$Subject == subj])>0)){
    df3$father.anxiety[i] <- unique(df2$father.anxiety[df2$Subject == subj])[1]
  }
  if ((length(df2$mother.infant.temp[df2$Subject == subj])>0)){
    df3$mother.infant.temp[i] <- unique(df2$mother.infant.temp[df2$Subject == subj])[1]
  }
  if ((length(df2$father.infant.temp[df2$Subject == subj])>0)){
    df3$father.infant.temp[i] <- unique(df2$father.infant.temp[df2$Subject == subj])
  }
}




df3$ModelEmo <- as.factor(df3$ModelEmo)
levels(df3$ModelEmo) <- levels(df2$ModelEmo)


for (i in 1:length(df3$Subject)){
  df3$MeanInfantTemp[i] <- mean(c(df3$mother.infant.temp[i],df3$father.infant.temp[i]),na.rm=T)
  df3$ParentAnxiety[i] <- mean(c(df3$mother.anxiety[i],df3$father.anxiety[i]),na.rm=T)
}


# add gaze variable (towards vs. averted)
for (i in 1:length(df3$ModelLook)){
  if (!is.na(df3$ModelLook[i])){ 
    if (df3$ModelLook[i] == df3$ObjectPos[i]){
      df3$ModelObjRelation[i] <- "Towards"
    } else {
      df3$ModelObjRelation[i] <- "Averted"
    }
  } else {
    df3$ModelObjRelation[i] <- NA
  }
  
}

write.csv(df3,"/Users/claravetter/Desktop/BabylabanalysenNEW/WithNA_SRF_O2_Model.csv")

#################################
#################################
#################################

#################################
#################################
#################################

df3 <- read.csv("/Users/claravetter/Desktop/BabylabanalysenNEW/WithNA_SRF_O2_Model.csv", header=T)
df3$mother.anxiety <- scale(df3$mother.anxiety)
df3$father.anxiety <- scale(df3$father.anxiety)
df3$MeanInfantTemp <- scale(df3$MeanInfantTemp)
df3$AverageDiameterPupils <- scale(df3$AverageDiameterPupils)

df3$ModelEmo <- as.factor(df3$ModelEmo)
df3$ModelEmo <- relevel(df3$ModelEmo, "Neutral")


df3$ModelObjRelation <- as.factor(df3$ModelObjRelation)
levels(df3$ModelObjRelation)
df3$ModelObjRelation <- relevel(df3$ModelObjRelation,"Towards")

# initial model 
fit <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation + BaseTime + TrialID, 
           random = ~ 1 + BaseTime| Subject/TrialID, 
           method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
           data = df3, na.action = na.exclude)
summary(fit)

output <- summary(fit)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit.csv")

CI<-intervals(fit, which = "fixed")
write.csv(data.frame(CI$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit_CI.csv")

fit0 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo*ModelObjRelation + BaseTime + TrialID , 
            random = ~ 1 + BaseTime| Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = df3, na.action = na.exclude)
summary(fit0)

output <- summary(fit0)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit0.csv")

CI0<-intervals(fit0, which = "fixed")
write.csv(data.frame(CI0$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit0_CI.csv")

#############

sum(!is.na(df3$father.anxiety))/length(df3$father.anxiety) # 36.28 not missing
sum(!is.na(df3$mother.anxiety))/length(df3$mother.anxiety) # 90.26% not missing
sum(!is.na(df3$MeanInfantTemp))/length(df3$MeanInfantTemp) # 90.50% not missing


mean(df3$mother.anxiety,na.rm=T) +sd(df3$mother.anxiety,na.rm=T)

mean(df3$AverageDiameterPupils[df3$ModelEmo == "Sad"],na.rm=T) -sd(df3$AverageDiameterPupils[df3$ModelEmo == "Sad"],na.rm=T)
# Main effects Parental anxiety and infant temperament
#df3 <- select(df1, TrialID, AverageDiameterPupils, BaseTime, Sex, ModelEmo, Subject, mother.anxiety, father.anxiety, mother.infant.temp, father.infant.temp)

# Main effects Parental anxiety and infant temperament
fit1 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation+ BaseTime + TrialID +  mother.anxiety + MeanInfantTemp, 
            random = ~ 1 + BaseTime | Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = df3, na.action = na.exclude)
summary(fit1)

output <- summary(fit1)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit1.csv")


CI1<-intervals(fit1, which = "fixed")
write.csv(data.frame(CI1$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit1_CI.csv")

# interactions with emotion 
fit2 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation+ BaseTime + TrialID + ModelEmo*mother.anxiety + MeanInfantTemp*ModelEmo, 
            random = ~ 1 + BaseTime | Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = df3, na.action = na.exclude)
summary(fit2)



output <- summary(fit2)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit2.csv")




CI2<-intervals(fit2, which = "fixed")
write.csv(data.frame(CI2$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit2_CI.csv")

# interactions anx and infant temp
fit3 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation + BaseTime + TrialID + mother.anxiety*MeanInfantTemp, 
            random = ~ 1 + BaseTime | Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = df3, na.action = na.exclude)
summary(fit3)

output <- summary(fit3)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit3.csv")


CI3<-intervals(fit3, which = "fixed")
write.csv(data.frame(CI3$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O2_Model_fit3_CI.csv")



##############
#### exploratory 
df3ex <- df3[df3$ModelEmo!="Neutral",]

df3ex$ModelEmo <- as.factor(df3ex$ModelEmo)
df3ex$ModelEmo <- relevel(df3ex$ModelEmo,"Happy")



fit4 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation + BaseTime + TrialID , 
            random = ~ 1 + BaseTime| Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = df3ex, na.action = na.exclude)
summary(fit4) 

output <- summary(fit4)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit.csv")


CI4<-intervals(fit4, which = "fixed")
write.csv(data.frame(CI4$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit_CI.csv")

fit0ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo*ModelObjRelation + BaseTime + TrialID , 
              random = ~ 1 + BaseTime| Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = df3ex, na.action = na.exclude)
summary(fit0ex)

output <- summary(fit0ex)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit0.csv")

CI0ex<-intervals(fit0ex, which = "fixed")
write.csv(data.frame(CI0ex$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit0_CI.csv")




fit1ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation + BaseTime + TrialID +  mother.anxiety + MeanInfantTemp , 
              random = ~ 1 + BaseTime | Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = df3ex, na.action = na.exclude)
summary(fit1ex)


output <- summary(fit1ex)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit1.csv")


CI1ex<-intervals(fit1ex, which = "fixed")
write.csv(data.frame(CI1ex$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit1_CI.csv")


fit2ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation + BaseTime + TrialID +  mother.anxiety + MeanInfantTemp  + ModelEmo*mother.anxiety + MeanInfantTemp*ModelEmo, 
              random = ~ 1 + BaseTime | Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = df3ex, na.action = na.exclude)
summary(fit2ex)


output <- summary(fit2ex)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit2.csv")


CI2ex<-intervals(fit2ex, which = "fixed")
write.csv(data.frame(CI2ex$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit2_CI.csv")

# interactions anx and infant temp
fit3ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation+ BaseTime  + TrialID  + mother.anxiety*MeanInfantTemp, 
              random = ~ 1 + BaseTime | Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = df3ex, na.action = na.exclude)
summary(fit3ex)

output <- summary(fit3ex)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit3.csv")


CI3ex<-intervals(fit3ex, which = "fixed")
write.csv(data.frame(CI3ex$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O2_Model_fit3_CI.csv")


