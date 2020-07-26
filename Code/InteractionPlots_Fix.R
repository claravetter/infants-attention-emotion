# Interaction plots fixation time

# Prepare data
ma_df <- read.csv('Data/Gazedata/fixationtimes.csv', header = T)
ma_df[ma_df == 0.0] <- NA

##### Libraries ######
library(nlme)
library(effects)
library(psych)
library(ggplot2)

# Make Neutral the reference 
ma_df$ModelEmo <- as.factor(ma_df$ModelEmo)
ma_df$ModelEmo <- relevel(ma_df$ModelEmo,'Neutral')

ma_df$ModelObjRelation <- as.factor(ma_df$ModelObjRelation)
ma_df$ModelObjRelation <- relevel(ma_df$ModelObjRelation, 'Towards')

ma_df$Block <- as.factor(ma_df$Block)


###########
ma_df_objects <- subset(ma_df, substr(ma_df$SubjectNo,1,3) != "SRF")


# exclude trials where baby looks < 500ms to Model+Obj
df_o2_SRO_excl <- subset(ma_df_objects,ma_df_objects$FixModelObj >= 500)
nrow(df_o2_SRO_excl)

expl <- df_o2_SRO_excl[df_o2_SRO_excl$ModelEmo!="Neutral",]
expl$ModelEmo <- relevel(expl$ModelEmo, "Happy")

expl_fit0 <- lme(fixed = FixDiffObject ~ 1 + ModelEmo*ModelObjRelation + Trial,
                 random = ~1 | SubjectNo, 
                 method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                 data = expl, na.action = na.exclude)



ef <- effect(term = "ModelEmo*ModelObjRelation", mod = expl_fit0)
ef
efdata1 <- as.data.frame(ef)
efdata1
efdata1$ModelEmo
#efdata1$mother.anxiety <- scale(efdata1$mother.anxiety)
#efdata1$fit <- scale(efdata1$fit)
#efdata1$se <- scale(efdata1$se)
ggplot(efdata1, aes(x=ModelObjRelation, y=fit, color= ModelEmo, group=ModelEmo)) +
  geom_point() +
  geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Gaze direction", y="Change in fixation duration", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/HAPPY_Fix_SRO_O2_InteractionGazeModelEmo.png')
dev.off()

efdata2 <- subset(efdata1,efdata1$ModelEmo != "Fear")

ggplot(efdata2, aes(x=ModelObjRelation, y=fit, color=ModelEmo,group=ModelEmo)) +
  geom_point() +
  geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Gaze direction", y="Change in fixation duration", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))
dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/HAPPY_Fix_SRO_O2_InteractionGazeSAD.png')
dev.off()
