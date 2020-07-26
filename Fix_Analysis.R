# Prepare data
ma_df <- read.csv('Data/Gazedata/fixationtimes.csv', header = T)
library(nlme)

ma_df[ma_df == 0.0] <- NA

# Make Neutral the reference 
ma_df$ModelEmo <- as.factor(ma_df$ModelEmo)
ma_df$ModelEmo <- relevel(ma_df$ModelEmo,'Neutral')

ma_df$ModelObjRelation <- as.factor(ma_df$ModelObjRelation)
ma_df$ModelObjRelation <- relevel(ma_df$ModelObjRelation, 'Towards')

ma_df$Block <- as.factor(ma_df$Block)


###########
ma_df_objects <- subset(ma_df, substr(ma_df$SubjectNo,1,3) != "SRF")

# Exclude trials where baby looks < 500ms to SingleModel
df_o1_SRO_excl <- subset(ma_df_objects,ma_df_objects$FixSingleModel > 500)
nrow(df_o1_SRO_excl)

# 7 <- 2.6%
# No analysis for outcome 1 on this condition

# Exclude trials where baby looks < 500ms to Model+Obj
df_o2_SRO_excl <- subset(ma_df_objects,ma_df_objects$FixModelObj >= 500)
nrow(df_o2_SRO_excl)


# 92 <- 33.87% of trials 
# Is ok 

df_o2_SRO_excl$FixModelObj <- scale(df_o2_SRO_excl$FixModelObj)
# Outcome 2 condition SRO
df_o2_SRO_excl$Trial <- df_o2_SRO_excl$Trial-1 # to make intercept better interpretable

fit1_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + ModelObjRelation + Trial, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit1_o2_SRO)

output <- summary(fit1_o2_SRO)
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit.csv")

CI<-intervals(fit1_o2_SRO, which = "fixed")
write.csv(data.frame(CI$fixed),"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit_CI.csv")


# Interaction effect of ModelEmo and ModelObjRelation
fit2_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo*ModelObjRelation + Trial, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit2_o2_SRO)
# No interaction effect
output <- summary(fit2_o2_SRO)
output$tTable
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit0.csv")

CI2<-intervals(fit2_o2_SRO, which = "fixed")
write.csv(data.frame(CI2$fixed),"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit0_CI.csv")

anova(fit1_o2_SRO,fit2_o2_SRO)
# Read questionnaire data for further analyses
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

df_o2_SRO_excl$MatAnxiety <- scale(df_o2_SRO_excl$MatAnxiety)
df_o2_SRO_excl$PatAnxiety <- scale(df_o2_SRO_excl$PatAnxiety)
df_o2_SRO_excl$MeanNegTemp <- scale(df_o2_SRO_excl$MeanNegTemp)
fit3_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + ModelObjRelation + Trial + MatAnxiety  + MeanNegTemp, 
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit3_o2_SRO)


output <- summary(fit3_o2_SRO)
output$tTable
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit1.csv")

CI3<-intervals(fit3_o2_SRO, which = "fixed")
write.csv(data.frame(CI3$fixed),"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit1_CI.csv")


# 2-way interactions with Emo
fit4_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + + ModelObjRelation + Trial + ModelEmo*MatAnxiety + MeanNegTemp*ModelEmo,
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit4_o2_SRO)

output <- summary(fit4_o2_SRO)
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit2.csv")

CI4<-intervals(fit4_o2_SRO, which = "fixed")
write.csv(data.frame(CI4$fixed),"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit2_CI.csv")


fit5_o2_SRO <- lme(fixed = FixDiffObject ~ 1 + ModelEmo + + ModelObjRelation + Trial + MatAnxiety*MeanNegTemp,
                   random = ~1 | SubjectNo, 
                   method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                   data = df_o2_SRO_excl, na.action = na.exclude)
summary(fit5_o2_SRO)

output <- summary(fit5_o2_SRO)
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit3.csv")

CI5<-intervals(fit5_o2_SRO, which = "fixed")
write.csv(data.frame(CI5$fixed),"Hauptanalysen/Model/Output/FIX_SRO_O2_Model_fit3_CI.csv")


anova(fit3_o2_SRO,fit4_o2_SRO,fit5_o2_SRO)

# Exploratory 
expl <- df_o2_SRO_excl[df_o2_SRO_excl$ModelEmo!="Neutral",]
expl$ModelEmo <- relevel(expl$ModelEmo, "Happy")

expl_fit <- lme(fixed = FixDiffObject ~ 1 + ModelEmo +  ModelObjRelation + Trial,
                random = ~1 | SubjectNo, 
                method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                data = expl, na.action = na.exclude)

output <- summary(expl_fit)
output$tTable
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit.csv")

CIe<-intervals(expl_fit, which = "fixed")
write.csv(data.frame(CIe$fixed),"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit_CI.csv")

expl_fit0 <- lme(fixed = FixDiffObject ~ 1 + ModelEmo*ModelObjRelation + Trial,
                 random = ~1 | SubjectNo, 
                 method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                 data = expl, na.action = na.exclude)

output <- summary(expl_fit0)

output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit0.csv")

CIe0<-intervals(expl_fit0, which = "fixed")
write.csv(data.frame(CIe0$fixed),"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit0_CI.csv")

anova(expl_fit,expl_fit0)

expl_fit1 <- lme(fixed = FixDiffObject ~ 1 + ModelEmo*ModelObjRelation + Trial + MatAnxiety + MeanNegTemp,
                 random = ~1 | SubjectNo, 
                 method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                 data = expl, na.action = na.exclude)

output <- summary(expl_fit1)
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit1.csv")

CIe1<-intervals(expl_fit1, which = "fixed")
write.csv(data.frame(CIe1$fixed),"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit1_CI.csv")

expl_fit2 <- lme(fixed = FixDiffObject ~ 1 + ModelEmo*ModelObjRelation + Trial + MatAnxiety*ModelEmo + MeanNegTemp*ModelEmo,
                 random = ~1 | SubjectNo, 
                 method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                 data = expl, na.action = na.exclude)

output <- summary(expl_fit2)
output$tTable
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit2.csv")

CIe2<-intervals(expl_fit2, which = "fixed")
write.csv(data.frame(CIe2$fixed),"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit2_CI.csv")


expl_fit3 <- lme(fixed = FixDiffObject ~ 1 + ModelEmo*ModelObjRelation + Trial + MatAnxiety*MeanNegTemp,
                 random = ~1 | SubjectNo, 
                 method = "ML", correlation = corAR1(form = ~ Trial|SubjectNo),
                 data = expl, na.action = na.exclude)

output <- summary(expl_fit3)
output$tTable
output <- as.data.frame(output$tTable)
write.csv(output,"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit3.csv")

CIe3<-intervals(expl_fit3, which = "fixed")
write.csv(data.frame(CIe3$fixed),"Hauptanalysen/Model/Output/FIX_HAPPY_SRO_O2_Model_fit3_CI.csv")

anova(expl_fit1,expl_fit2,expl_fit3)
