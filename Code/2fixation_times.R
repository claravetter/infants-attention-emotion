#### fixation times ####
##### Libraries ######
library(readr)

## gazepath package ##

#setwd("/Users/claravetter/Documents/Master/Babylab/Data/Gazedata/outliers_removed/0SD_removed/InfantEmoToolsFaces")
#df <- read.csv('SRF202_InfantEmoToolsFaces_0SD_removed.csv',header=TRUE)

# gazepath needs column with distance to eye tracker in mm; infants were
# secured in a car seat at 60cm (600mm) distance; --> add this variable: 
#df$Distance <- 600
# df$StimulusHeight_mm[df$EventNum == 0] <- 0 # blank screen
# df$StimulusHeight_mm[df$EventNum == 1 | df$EventNum == 4] <- 6.78 # object only
# df$StimulusHeight_mm[df$EventNum == 2 | df$EventNum == 3] <- 15 # Single_Model
# df$StimulusHeight_mm[df$EventNum == 3] <- 15 # Model (face + object)? 
# df$StimulusWidth_mm[df$EventNum == 0] <- 0 # blank screen
# df$StimulusWidth_mm[df$EventNum == 1 | df$EventNum == 4] <- 5.23 # object only
# df$StimulusWidth_mm[df$EventNum == 2] <- 9 # Single_Model
# df$StimulusWidth_mm[df$EventNum == 3] <- 14.23 # Model (face + object)?

library(gazepath)



files_faces <- list.files(path="Data/Gazedata/outliers_removed/0SD_removed/InfantEmoToolsFaces", full.names = TRUE)


files_objects <- list.files(path="Data/Gazedata/outliers_removed/0SD_removed/InfantEmoToolsObject", full.names = TRUE)

files <- append(files_faces,files_objects)
ma_df <- data.frame(SubjectNo=0,
                    Trial=0,
                    FixObj1 = 0,
                    FixObj2 = 0,
                    FixDiffObject=0,
                    FixSingleModel=0,
                    FixModelObj=0, 
                    ModelEmo=0,
                    ModelLook=0,
                    ObjPos=0,
                    Block=0)







for (i in 1:length(files)){
  df <- read.csv(files[i],header = TRUE)
  df$Distance <- 600
  
  df_O1 <- subset(df, df$Event == 'Object1')
  df_O2 <- subset(df, df$Event == 'Object2')
  df_SM <- subset(df, df$Event == 'Single_Model')
  df_MO <- subset(df, df$Event == 'Model')
  
  gp_O1 <- gazepath(df_O1, x1 = 'CursorX', y1 = 'CursorY', d1 = 'Distance', 
                    trial = 'TrialID', height_px = 1024, width_px = 1280, 
                    height_mm = 270, width_mm = 330, samplerate = 120, 
                    method= 'gazepath')
  gp_O2 <- gazepath(df_O2, x1 = 'CursorX', y1 = 'CursorY', d1 = 'Distance', 
                    trial = 'TrialID', height_px = 1024, width_px = 1280, 
                    height_mm = 270, width_mm = 330, samplerate = 120, 
                    method= 'gazepath')
  gp_SM <- gazepath(df_SM, x1 = 'CursorX', y1 = 'CursorY', d1 = 'Distance', 
                    trial = 'TrialID', height_px = 1024, width_px = 1280, 
                    height_mm = 270, width_mm = 330, samplerate = 120, 
                    method= 'gazepath')
  gp_MO <- gazepath(df_MO, x1 = 'CursorX', y1 = 'CursorY', d1 = 'Distance', 
                    trial = 'TrialID', height_px = 1024, width_px = 1280, 
                    height_mm = 270, width_mm = 330, samplerate = 120, 
                    method= 'gazepath')
  

  fix_O1 <- summary(gp_O1, fixations_only = T)
  fix_O2 <- summary(gp_O2, fixations_only = T)
  fix_SM <- summary(gp_SM, fixations_only = T)
  fix_MO <- summary(gp_MO, fixations_only = T)
  
  for (j in 1:16){
    if (fix_O1[1]!= "There were no fixations or saccades classified, probably data quality of this particpant is very low"){
      dur_O1 <- sum(fix_O1$Duration[fix_O1$Trial == j],na.rm = T)
    } else {
      dur_O1 <- NA
    }
    if (fix_O2[1] != "There were no fixations or saccades classified, probably data quality of this particpant is very low"){
      dur_O2 <- sum(fix_O2$Duration[fix_O2$Trial == j],na.rm = T)

    } else {
      dur_O2 <- NA
    }
    if (!is.na(dur_O1) & !is.na(dur_O2)){
      dif_O <- dur_O2 - dur_O1
    } else {
      dif_O <- NA
    }
    if (fix_SM[1] != "There were no fixations or saccades classified, probably data quality of this particpant is very low"){
      dur_SM <- sum(fix_SM$Duration[fix_SM$Trial == j],na.rm = T)
    } else {
      dur_SM <- NA
    }
    if (fix_MO[1] != "There were no fixations or saccades classified, probably data quality of this particpant is very low"){
      dur_MO <- sum(fix_MO$Duration[fix_MO$Trial == j],na.rm = T)
    } else {
      dur_MO <- NA
    }
    
    emotion <- df$ModelEmo[df$TrialID == j][1]
    look <- df$ModelLook[df$TrialID == j][1]
    objpos <- df$ObjectPos[df$TrialID == j][1]
    block <- df$Block[df$TrialID == j][1]
    new_row <- data.frame(SubjectNo = as.character(df$Subject[1]),
                          Trial = j,
                          FixObj1 = dur_O1,
                          FixObj2 = dur_O2,
                          FixDiffObject = dif_O,
                          FixSingleModel = dur_SM,
                          FixModelObj = dur_MO,
                          ModelEmo = emotion, 
                          ModelLook = look, 
                          ObjPos = objpos, 
                          Block = block)
    ma_df<- rbind(ma_df,new_row)
  }
}
  
ma_df <- ma_df[2:nrow(ma_df),]


# fix some typos 
ma_df$SubjectNo[305:320] <- "SRF225"
ma_df$SubjectNo[ma_df$SubjectNo == 'SR225'] <- "SRO225"
ma_df$SubjectNo[ma_df$SubjectNo == 'SR201'] <- "SRO201"
ma_df$SubjectNo[ma_df$SubjectNo == 'SR206'] <- "SRO206"

# fix block variable
ma_df$Block[ma_df$Trial > 8 ] <- 2

# add gaze variable (towards vs. averted)
for (i in 1:length(ma_df$ModelLook)){
  if (!is.na(ma_df$ModelLook[i])){
    if (ma_df$ModelLook[i] == ma_df$ObjPos[i]){
      ma_df$ModelObjRelation[i] <- "Towards"
    } else {
      ma_df$ModelObjRelation[i] <- "Averted"
    }
  }

}



# write file with fixation times 
setwd('Data/Gazedata')
write.csv(ma_df,'fixationtimes.csv', row.names = F)


###########################

ma_df <- read.csv('fixationtimes.csv', header = T)
library(nlme)

# make Neutral the reference 
ma_df$ModelEmo <- as.factor(ma_df$ModelEmo)
ma_df$ModelEmo <- relevel(ma_df$ModelEmo,'Neutral')

ma_df$ModelObjRelation <- as.factor(ma_df$ModelObjRelation)
ma_df$ModelObjRelation <- relevel(ma_df$ModelObjRelation, 'Towards')

ma_df$Block <- as.factor(ma_df$Block)


ma_df_faces <- subset(ma_df, substr(ma_df$SubjectNo,1,3) == "SRF")


# exclude trials where baby looks < 500ms to SingleModel
df_o1_SRF_excl <- subset(ma_df_faces,ma_df_faces$FixSingleModel > 500)
nrow(df_o1_SRF_excl)

# 1 <- 1/320 = 0.0031
# no analysis for outcome 1 on this condition




# exclude trials where baby looks < 500ms to Model+Obj
df_o2_SRF_excl <- subset(ma_df_faces,ma_df_faces$FixModelObj >= 500)
nrow(df_o2_SRF_excl)

df_o2_SRF_excl<- subset(df_o2_SRF_excl,!is.na(df_o2_SRF_excl$FixDiffObject))

# 60 <- 18% of trials 
# is ok 


# outcome 2 condition SRF
fit1_o2_SRF <- lme(fixed = FixSingleModel ~ 1 + ModelEmo + ModelObjRelation, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~Trial|SubjectNo),
                   data = df_o2_SRF_excl, na.action = na.exclude)
# Error in lme.formula(fixed = FixSingleModel ~ 1 + ModelEmo, random = ~1 |  : 
# nlminb problem, convergence error code = 1
# message = false convergence (8)
# overparameterized model (?)
# too many missing values 
summary(fit1_o2_SRF)

# plot different emotions boxplot
plot(df_o2_SRF_excl$ModelEmo,df_o2_SRF_excl$FixDiffObject, col = c("red","blue","green","yellow"))

###########
ma_df_objects <- subset(ma_df, substr(ma_df$SubjectNo,1,3) != "SRF")

# exclude trials where baby looks < 500ms to SingleModel
df_o1_SRO_excl <- subset(ma_df_objects,ma_df_objects$FixSingleModel > 500)
nrow(df_o1_SRO_excl)

# 7 <- 2.6%
# no analysis for outcome 1 on this condition

# exclude trials where baby looks < 500ms to Model+Obj
df_o2_SRO_excl <- subset(ma_df_objects,ma_df_objects$FixModelObj >= 500)
nrow(df_o2_SRO_excl)


# 92 <- 33.87% of trials 
# is ok 

# outcome 2 condition SRO
df_o2_SRO_excl$Trial <- df_o2_SRO_excl$Trial-1 # to make intercept better interpretable

fit1_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + ModelObjRelation + Trial, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit1_o2_SRO)

# sex not significant, therefore removed


fit1_o2_SRO_nocorAR <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + ModelObjRelation + Trial, 
                           random = ~1 | SubjectNo, 
                           method = "ML", #correlation = corAR1(form = ~ Trial|SubjectNo),
                           data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit1_o2_SRO_nocorAR)


anova(fit1_o2_SRO,fit1_o2_SRO_nocorAR)
# not significant; model fit does not decrease significantly when cor included
# do I still keep corAR1?

CI<-intervals(fit1_o2_SRO_nocorAR, which = "fixed")



# plot boxplots
plot(df_o2_SRO_excl$ModelEmo,df_o2_SRO_excl$FixDiffObject, col = c("red","blue","green","yellow"))


# interaction effect of ModelEmo and ModelObjRelation
fit2_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo*ModelObjRelation, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit2_o2_SRO)
# no interaction effect



# read questionnaire data for further analyses
q <- read.csv("Data/Questionnaire/questionnaire.csv", header=T)
df_o2_SRO_excl$infantNo <- substr(df_o2_SRO_excl$SubjectNo,4,100)

q$ID <- substr(q$ID,1,3)
qm <- subset(q,q$sex == 2)
qf <- subset(q,q$sex == 1)

for (i in 1:length(qm$ID)){
  if (qm$ID[i] %in% df_o2_SRO_excl$infantNo){
    df_o2_SRO_excl$MatAnxiety[df_o2_SRO_excl$infantNo == qm$ID[i]] <- qm$Anxiety_Score[i]
    df_o2_SRO_excl$MatDepr[df_o2_SRO_excl$infantNo == qm$ID[i]] <- qm$Depression_Score[i]
    df_o2_SRO_excl$MatNegAff[df_o2_SRO_excl$infantNo == qm$ID[i]] <- qm$Neg_affect[i]
    df_o2_SRO_excl$MatNegTemp[df_o2_SRO_excl$infantNo == qm$ID[i]] <- qm$NegTemp[i]
    df_o2_SRO_excl$Sex[df_o2_SRO_excl$infantNo == qm$ID[i]] <- qm$child_sex[i]
  }
}

for (i in 1:length(qf$ID)){
  if (qf$ID[i] %in% df_o2_SRO_excl$infantNo){
    df_o2_SRO_excl$PatAnxiety[df_o2_SRO_excl$infantNo == qf$ID[i]] <- qf$Anxiety_Score[i]
    df_o2_SRO_excl$PatDepr[df_o2_SRO_excl$infantNo == qf$ID[i]] <- qf$Depression_Score[i]
    df_o2_SRO_excl$PatNegAff[df_o2_SRO_excl$infantNo == qf$ID[i]] <- qf$Neg_affect[i]
    df_o2_SRO_excl$PatNegTemp[df_o2_SRO_excl$infantNo == qf$ID[i]] <- qf$NegTemp[i]
  }
}

for (i in 1:length(df_o2_SRO_excl$SubjectNo)){
  df_o2_SRO_excl$MeanNegTemp[i] <- mean(c(df_o2_SRO_excl$MatNegTemp[i],df_o2_SRO_excl$PatNegTemp[i]),na.rm=T)
  df_o2_SRO_excl$ParentAnxiety[i] <- mean(c(df_o2_SRO_excl$MatAnxiety[i],df_o2_SRO_excl$PatAnxiety[i]),na.rm=T)
}

# Main effects
fit3_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + Trial + MatAnxiety + PatAnxiety + MeanNegTemp, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit3_o2_SRO)

# 2-way interactions with Emo
fit4_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + Trial + MatAnxiety*ModelEmo + PatAnxiety*ModelEmo + MeanNegTemp*ModelEmo, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit4_o2_SRO)

# 2-way interactions with Temp
fit5_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + Trial +  ParentAnxiety*MeanNegTemp, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit5_o2_SRO)

# PatAnxiety * ModelEmo 
fit6_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + Trial + PatAnxiety*ModelEmo, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit6_o2_SRO)

# MeanNegTemp
fit7_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + Trial + MeanNegTemp, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit7_o2_SRO)

# MeanNegTemp * ModelEmo 
fit8_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + Trial + MeanNegTemp*ModelEmo, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit8_o2_SRO)



############## 


library(ggplot2)
ggplot(df_o2_SRO_excl, aes(x=ModelEmo, y=FixDiffObject, colour=SubjectNo)) +
  geom_point(size=3) +
  geom_line(aes(y = predict(fit1_o2_SRO),group=SubjectNo))
  







