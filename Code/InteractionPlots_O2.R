# Interaction Plots O2
# NO: O2: Neutral: Mat. anx.*EMotion 

# Prepare data
df3 <- read.csv("/Users/claravetter/Desktop/BabylabanalysenNEW/WithNA_SRO_O2_Model.csv", header=T)

#df3$mother.anxiety <- scale(df3$mother.anxiety)
#df3$father.anxiety <- scale(df3$father.anxiety)
#df3$MeanInfantTemp <- scale(df3$MeanInfantTemp)
#df3$AverageDiameterPupils <- scale(df3$AverageDiameterPupils)

df3$ModelEmo <- as.factor(df3$ModelEmo)
df3$ModelEmo <- relevel(df3$ModelEmo, "Neutral")

df3$ModelObjRelation <- as.factor(df3$ModelObjRelation)
levels(df3$ModelObjRelation)
df3$ModelObjRelation <- relevel(df3$ModelObjRelation,"Towards")

fit2 <- lme(fixed = AverageDiameterPupils ~ 1+ ModelEmo + ModelObjRelation + BaseTime + TrialID + ModelEmo*mother.anxiety + MeanInfantTemp*ModelEmo, 
            random = ~ 1 + BaseTime | Subject/TrialID, 
            method = "ML", correlation = corAR1(form = ~ BaseTime | Subject/TrialID),
            data = df3, na.action = na.exclude)
summary(fit2)


descr <- describe(df3$mother.anxiety)
descr
ef <- effect(term = "ModelEmo*mother.anxiety", xlevels =list(mother.anxiety = c(descr$mean-descr$sd,descr$mean,descr$mean+descr$sd)),mod = fit2)
efdata1 <- as.data.frame(ef)

efdata1$mother.anxiety <- scale(efdata1$mother.anxiety)
#efdata1$fit <- scale(efdata1$fit)
#efdata1$se <- scale(efdata1$se)
ggplot(efdata1, aes(x=mother.anxiety, y=fit, color=ModelEmo,group=ModelEmo)) +
  geom_point() +
  geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Maternal anxiety", y="Change in pupil response", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/_SRO_O2_Interaction_motherAnxModelEmo.png')
dev.off()

efdata2 <- efdata1[efdata1$ModelEmo == c("Neutral","Fear"),]
ggplot(efdata2, aes(x=mother.anxiety, y=fit, color=ModelEmo,group=ModelEmo)) +
  geom_point() +
  geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Maternal anxiety", y="Change in pupil response", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/_SRO_O2_Interaction_motherAnxModelFEAR.png')
dev.off()
