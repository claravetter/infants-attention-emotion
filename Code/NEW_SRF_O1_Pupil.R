###############################################
### NEW_SRF_O1_Pupil
###############################################

##### Libraries ######
library(dplyr)
library(nlme)
setwd("/Users/claravetter/Desktop/BabylabanalysenNEW/BaselineSingleModelQ")

csv_files <-
  list.files(
    path = "/Users/claravetter/Desktop/BabylabanalysenNEW/BaselineSingleModelQ",
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

write.csv(df_all, "/Users/claravetter/Desktop/BabylabanalysenNEW/SRF_O1_Model.csv")

df_all <- read.csv("/Users/claravetter/Desktop/BabylabanalysenNEW/SRF_O1_Model.csv", header = T)

df1 <- subset(df_all, !is.na(df_all$SingleModelBaseTime))
df1$ModelEmo <- as.factor(df1$ModelEmo)
df1$ModelEmo<-relevel(df1$ModelEmo, "Neutral")

#df1$TrialID <- df1$TrialID-1
df2 <- select(df1, TrialID, AverageDiameterPupils, BaseTime, Sex, ModelEmo, 
              Subject, mother.anxiety, father.anxiety, mother.infant.temp, 
              father.infant.temp)


n <- length(unique(df_all$Subject))
r <- length(df2)*20*n*16

df3 <- matrix(c(rep.int(NA,r)),nrow=20*n*16,ncol=length(df2)) 
df3 <- as.data.frame(df3)
colnames(df3) <- colnames(df2)



#df2$BaseTime <- round_any(df2$BaseTime,50)


trials <- rep(c(0:15),n,each=20)
subjects <- rep(unique(df2$Subject),each=20*16)
times <- rep(c(0:19),n*16)
df3$TrialID <- trials
df3$Subject <- subjects
df3$BaseTime <- times*50



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

write.csv(df3,"/Users/claravetter/Desktop/BabylabanalysenNEW/WithNA_SRF_O1_Model.csv")

#################################
#################################
#################################

ndf <- read.csv("/Users/claravetter/Desktop/BabylabanalysenNEW/WithNA_SRF_O1_Model.csv", header=T)
ndf$mother.anxiety <- scale(ndf$mother.anxiety)
ndf$father.anxiety <- scale(ndf$father.anxiety)
ndf$MeanInfantTemp <- scale(ndf$MeanInfantTemp)
ndf$AverageDiameterPupils <- scale(ndf$AverageDiameterPupils)

skewness(ndf$MeanInfantTemp,na.rm=T)
kurtosis(ndf$MeanInfantTemp,na.rm=T) 
skewness(ndf$mother.anxiety,na.rm=T)
kurtosis(ndf$mother.anxiety,na.rm=T)

ndf$ModelEmo <- as.factor(ndf$ModelEmo)
ndf$ModelEmo <- relevel(ndf$ModelEmo, "Neutral")


# initial model 
fit <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID, 
           random = ~ 1 + BaseTime| Subject/TrialID, 
           method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
           data = ndf, na.action = na.exclude)

summary(fit)
par(mfrow = c(3,2))
plot(resid(fit))
output <- summary(fit)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit.csv")

CI<-intervals(fit, which = "fixed")
write.csv(data.frame(CI$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit_CI.csv")

fit0 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo*BaseTime + TrialID ,
            random = ~ 1 + BaseTime| Subject/TrialID,
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = ndf, na.action = na.exclude)
summary(fit0)

plot(resid(fit0))



output <- summary(fit0)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit0.csv")

CI0<-intervals(fit, which = "fixed")
write.csv(data.frame(CI0$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit0_CI.csv")

#############

sum(!is.na(ndf$father.anxiety))/length(ndf$father.anxiety) # 41.18% not missing
sum(!is.na(ndf$mother.anxiety))/length(ndf$mother.anxiety) # 76.47% not missing
sum(!is.na(ndf$MeanInfantTemp))/length(ndf$MeanInfantTemp) # 76.47% not missing



# Main effects Parental anxiety and infant temperament
fit1 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID +  mother.anxiety + MeanInfantTemp, 
            random = ~ 1 + BaseTime | Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = ndf, na.action = na.exclude)
summary(fit1)


plot(resid(fit1))

output <- summary(fit1)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit1.csv")


CI1<-intervals(fit1, which = "fixed")
write.csv(data.frame(CI1$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit1_CI.csv")

# interactions with emotion 
fit2 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID +  mother.anxiety + MeanInfantTemp + ModelEmo*mother.anxiety + MeanInfantTemp*ModelEmo,  
            random = ~ 1 + BaseTime | Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = ndf, na.action = na.exclude)
summary(fit2)

plot(resid(fit2))


output <- summary(fit2)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit2.csv")




CI2<-intervals(fit2, which = "fixed")
write.csv(data.frame(CI2$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit2_CI.csv")

# interactions anx and infant temp
fit3 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID  +  mother.anxiety*MeanInfantTemp, 
            random = ~ 1 + BaseTime | Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = ndf, na.action = na.exclude)
summary(fit3)


plot(resid(fit3))
dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/SRF1_Resid_fit_fit3.png')
dev.off()

anova(fit1,fit3)

output <- summary(fit3)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit3.csv")


CI3<-intervals(fit3, which = "fixed")
write.csv(data.frame(CI3$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/SRF_O1_Model_fit3_CI.csv")



##############
#### exploratory 
ndfex <- ndf[ndf$ModelEmo!="Neutral",]

ndfex$ModelEmo <- as.factor(ndfex$ModelEmo)
ndfex$ModelEmo <- relevel(ndfex$ModelEmo,"Happy")

fit4 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID, 
            random = ~ 1 + BaseTime| Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = ndfex, na.action = na.exclude)
summary(fit4) 

par(mfrow = c(3,2))
plot(resid(fit4))

output <- summary(fit4)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit.csv")


CI4<-intervals(fit4, which = "fixed")
write.csv(data.frame(CI4$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit_CI.csv")


fit0ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo*BaseTime + TrialID, 
              random = ~ 1 + BaseTime | Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = ndfex, na.action = na.exclude)
summary(fit0ex)
plot(resid(fit0ex))


output <- summary(fit0ex)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit0.csv")

CI0ex<-intervals(fit1ex, which = "fixed")
write.csv(data.frame(CI0ex$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit0_CI.csv")


fit1ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID +  mother.anxiety  + MeanInfantTemp, 
              random = ~ 1 + BaseTime | Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = ndfex, na.action = na.exclude)
summary(fit1ex)
plot(resid(fit1ex))


output <- summary(fit1ex)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit1.csv")

CI1ex<-intervals(fit1ex, which = "fixed")
write.csv(data.frame(CI1ex$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit1_CI.csv")

fit2ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID + ModelEmo*mother.anxiety  + ModelEmo*MeanInfantTemp, 
              random = ~ 1 + BaseTime | Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = ndfex, na.action = na.exclude)
summary(fit2ex)
plot(resid(fit2ex))

output <- summary(fit2ex)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit2.csv")


CI2ex<-intervals(fit2ex, which = "fixed")
write.csv(data.frame(CI2ex$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit2_CI.csv")

# interactions anx and infant temp
fit3ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID +  mother.anxiety*ModelEmo + mother.anxiety*MeanInfantTemp, 
              random = ~ 1 + BaseTime | Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = ndfex, na.action = na.exclude)
summary(fit3ex)
plot(resid(fit3ex))
dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/SRF1_Resid_exfit_exfit3.png')
dev.off()

output <- summary(fit3ex)
output <- as.data.frame(output$tTable)
write.csv(output,"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit3.csv")


CI3ex<-intervals(fit3ex, which = "fixed")
write.csv(data.frame(CI3ex$fixed),"/Users/claravetter/Desktop/BabylabanalysenNEW/Output/HAPPY_SRF_O1_Model_fit3_CI.csv")




