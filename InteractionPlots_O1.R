### Interaction Plots ####

##### Libraries ######
library(nlme)
library(effects)
library(psych)
library(ggplot2)

# NO: O1: Neutral: Maternal anxiety*Emotion 
ndf <- read.csv("/Users/claravetter/Desktop/BabylabanalysenNEW/WithNA_SRO_O1_Model.csv", header=T)
#ndf$mother.anxiety <- scale(ndf$mother.anxiety)
ndf$father.anxiety <- scale(ndf$father.anxiety)
ndf$MeanInfantTemp <- scale(ndf$MeanInfantTemp)
ndf$ModelEmo <- as.factor(ndf$ModelEmo)
ndf$ModelEmo <- relevel(ndf$ModelEmo, "Neutral")


ndf$AverageDiameterPupils <- scale(ndf$AverageDiameterPupils)

fit2 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID  + ModelEmo*mother.anxiety, #+ MeanInfantTemp*ModelEmo,
            random = ~ 1 + BaseTime | Subject/TrialID,
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = ndf, na.action = na.exclude)
summary(fit2)

descr <- describe(ndf$mother.anxiety)
descr
ef <- effect(term = "ModelEmo*mother.anxiety", xlevels =list(mother.anxiety = c(descr$mean-descr$sd,descr$mean,descr$mean+descr$sd)),mod = fit2 )
efdata1 <- as.data.frame(ef)

efdata1$mother.anxiety <- scale(efdata1$mother.anxiety)

ggplot(efdata1, aes(x=mother.anxiety, y=fit, color=ModelEmo,group=ModelEmo)) +
  geom_point() +
  geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Maternal anxiety", y="Pupil response", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/SRO_O1_Interaction_motherAnxModelEmo.png')
dev.off()

efdata2 <- efdata1[efdata1$ModelEmo == c("Neutral","Fear"),]
ggplot(efdata2, aes(x=mother.anxiety, y=fit, color=ModelEmo,group=ModelEmo)) +
  geom_point() +
  geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Maternal anxiety", y="Pupil response", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/SRO_O1_Interaction_motherAnxModelFEAR.png')
dev.off()

##########################

# NF: O1: Happy: Maternal anxiety*ModelEmo

ndf <- read.csv("/Users/claravetter/Desktop/BabylabanalysenNEW/WithNA_SRF_O1_Model.csv", header=T)
#ndf$mother.anxiety <- scale(ndf$mother.anxiety)
#ndf$father.anxiety <- scale(ndf$father.anxiety)
#ndf$MeanInfantTemp <- scale(ndf$MeanInfantTemp)
ndf$AverageDiameterPupils <- scale(ndf$AverageDiameterPupils)

ndf$ModelEmo <- as.factor(ndf$ModelEmo)
ndf$ModelEmo <- relevel(ndf$ModelEmo, "Neutral")

ndfex <- ndf[ndf$ModelEmo!="Neutral",]

ndfex$ModelEmo <- as.factor(ndfex$ModelEmo)
ndfex$ModelEmo <- relevel(ndfex$ModelEmo,"Happy")

fit2ex <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + BaseTime + TrialID + ModelEmo*mother.anxiety  + ModelEmo*MeanInfantTemp, 
              random = ~ 1 + BaseTime | Subject/TrialID, 
              method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
              data = ndfex, na.action = na.exclude)
summary(fit2ex)

descr <- describe(ndfex$mother.anxiety)
descr
ef <- effect(term = "ModelEmo*mother.anxiety", xlevels =list(mother.anxiety = c(descr$mean-descr$sd,descr$mean,descr$mean+descr$sd)),mod = fit2ex )
efdata1 <- as.data.frame(ef)
efdata1$mother.anxiety <- scale(efdata1$mother.anxiety)
#efdata1$fit <- scale(efdata1$fit)
#efdata1$se <- scale(efdata1$se)
ggplot(efdata1, aes(x=mother.anxiety, y=fit, color=ModelEmo,group=ModelEmo)) +
  geom_point() +
  geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Maternal anxiety", y="Pupil response", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/HAPPY_SRF_O1_Interaction_motherAnxModelEmo.png')
dev.off()

efdata2 <- subset(efdata1,efdata1$ModelEmo != "Fear" )
ggplot(efdata2, aes(x=mother.anxiety, y=fit, color=ModelEmo,group=ModelEmo)) +
  geom_point() +
  geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Maternal anxiety", y="Pupil response", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/HAPPY_SRF_O1_Interaction_motherAnxModelSAD.png')
dev.off()

