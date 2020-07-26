# Prepare data
ma_df <- read.csv('Data/Gazedata/fixationtimes.csv', header = T)
library(nlme)
ma_df[ma_df == 0.0] <- NA

# make Neutral the reference 
ma_df$ModelEmo <- as.factor(ma_df$ModelEmo)
ma_df$ModelEmo <- relevel(ma_df$ModelEmo,'Neutral')

ma_df$ModelObjRelation <- as.factor(ma_df$ModelObjRelation)
ma_df$ModelObjRelation <- relevel(ma_df$ModelObjRelation, 'Towards')

ma_df$Block <- as.factor(ma_df$Block)


###########
ma_df_objects <- subset(ma_df, substr(ma_df$SubjectNo,1,3) != "SRF")


# Exclude trials where baby looks < 500ms to Model+Obj
fix <- subset(ma_df_objects,ma_df_objects$FixModelObj >= 500)

nrow(fix)

#expl <- fixdf[fixdf$ModelEmo!="Neutral",]
#expl$ModelEmo <- relevel(expl$ModelEmo, "Happy")

############
df3 <- read.csv("/Users/claravetter/Desktop/BabylabanalysenNEW/WithNA_SRO_O2_Model.csv", header=T)

length(unique(df3$Subject))

subjects <- rep(unique(df3$Subject),each=16)
pupil <- data.frame(Subject = subjects, Trial = rep(0:15,length(unique(df3$Subject))))

fix$ModelEmo

for (i in 1:length(unique(df3$Subject))){
  for (j in 1:length(0:15)) {
    pupil$AvPupil[pupil$Subject == unique(df3$Subject)[i] & pupil$Trial == j] <- 
      mean(df3$AverageDiameterPupils[df3$Subject == unique(df3$Subject)[i] & df3$TrialID == j], na.rm=T)
    if (length(fix$FixDiffObject[fix$Subject == unique(df3$Subject)[i] & fix$Trial == j])>0){
      pupil$Fix[pupil$Subject == unique(df3$Subject)[i] & pupil$Trial == j] <- 
        fix$FixDiffObject[fix$Subject == unique(df3$Subject)[i] & fix$Trial == j] 
      pupil$ModelEmo[pupil$Subject == unique(df3$Subject)[i] & pupil$Trial == j] <- 
        fix$ModelEmo[fix$Subject == unique(df3$Subject)[i] & fix$Trial == j]
    }
  }
}

pupil$ModelEmo <- as.factor(pupil$ModelEmo)
levels(pupil$ModelEmo) <- c("Neutral","Fear","Happy","Sad")
pupil$ModelEmo 


cor.test(pupil$AvPupil,pupil$Fix, use = "complete.obs")


NoNApupil <- pupil[!is.na(pupil$ModelEmo),]

pupil$zPupil <- scale(pupil$AvPupil)
pupil$zFix <- scale(pupil$Fix)
NoNApupil <- pupil[!is.na(pupil$ModelEmo),]
ggplot(NoNApupil, aes(x=Fix, y=AvPupil, color= ModelEmo, group=ModelEmo)) +
  geom_point() +
  #geom_line(size=1.2) +
  #geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=ModelEmo),alpha=0.3) +
  labs(x= "Fixation time", y="Pupil response", color="Emotion", fill="Emotion") + theme_classic() + theme(text=element_text(size=20))

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/Pupil_Fix_Correlation.png')
dev.off()

library("ggpubr")
ggscatter(NoNApupil, x = "Fix", y = "AvPupil", 
          add = "reg.line", conf.int = T, color = "blue",
          cor.coef = F, cor.method = "pearson",
          xlab = "Fixation duration", ylab = "Pupil dilation")

dev.copy(png,'/Users/claravetter/Desktop/BabylabanalysenNEW/Plots/Correlation.png')
dev.off()

ggscatter(NoNApupil, x = "Fix", y = "AvPupil", 
          add = "reg.line")

length(unique(pupil$Subject))
