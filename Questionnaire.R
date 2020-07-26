### Preparing questionnaire data ###
# There is one .csv file for all questionnaire data of all infants (both conditions mixed)
# This script calculates anxiety, depression, negative affect, infant temperament scores 

dataset <- read.csv("Data/Questionnaire/Questionnaire_Data_all.csv",header = TRUE)

# Unimportant
drops <- c("RecipientLastName","RecipientFirstName","RecipientEmail","ExternalReference","LocationLatitude",
           "LocationLongitude","DistributionChannel","UserLanguage","ResponseId","RecordedDate",
           "StartDate","EndDate","Status","IPAddress","Progress","Duration__in_seconds_","RecordedDate")
questions <- dataset[ , !(names(dataset) %in% drops)]


#Q1: General information
  #Q1.1: Date
  #Q1.2: initials
  #Q1.2: Partner's initials
  #Q1.4: sex (male=1; female=2)
  #Q1.5: age
  #Q1.6: child's birthdate
  #Q1.7: child's sex
  #Q1.8: country of birth (of parent)
  #Q1.9: country of birth of father
  #Q1.10: country of birth of mother
  #Q1.11: duration of relationship
  #Q1.12: living situation
  #Q1.13: children of other relationship
  #Q1.14: level of occupation
  #Q1.15: education
  #Q1.16: current occupation
  #Q1.17: working hours per week
  #Q1.18: monthly income

# rename questions to make questionnare easier to understand
names(questions)[names(questions)=='Q1.1'] <- 'date'
names(questions)[names(questions)=='Q1.2'] <- 'initials'
names(questions)[names(questions)=='Q1.3'] <- 'partner_initials'
names(questions)[names(questions)=='Q1.4'] <- 'sex'
names(questions)[names(questions)=='Q1.5'] <- 'age'
names(questions)[names(questions)=='Q1.6'] <- 'child_birthdate'
names(questions)[names(questions)=='Q1.7'] <- 'child_sex'
names(questions)[names(questions)=='Q1.8'] <- 'origin'
names(questions)[names(questions)=='Q1.9'] <- 'origin_father'
names(questions)[names(questions)=='Q1.10'] <- 'origin_mother'
names(questions)[names(questions)=='Q1.11'] <- 'duration_relationship'
names(questions)[names(questions)=='Q1.12'] <- 'living_situation'
names(questions)[names(questions)=='Q1.12_4_TEXT'] <- 'living_situation_4_text'
names(questions)[names(questions)=='Q1.13'] <- 'children_other_relationship'
names(questions)[names(questions)=='Q1.13_1_TEXT'] <- 'children_other_relationship_1_text'
names(questions)[names(questions)=='Q1.13_2_TEXT'] <- 'children_other_relationship_2_text'
names(questions)[names(questions)=='Q1.13_3_TEXT'] <- 'children_other_relationship_3_text'
names(questions)[names(questions)=='Q1.14'] <- 'professional_level'
names(questions)[names(questions)=='Q1.15'] <- 'educational_level'
names(questions)[names(questions)=='education_9-text'] <- 'education_9_text'
names(questions)[names(questions)=='Q1.16'] <- 'professional_status'
names(questions)[names(questions)=='Q1.16_5_TEXT'] <- 'professional_status_5_text'
names(questions)[names(questions)=='Q1.16_8_TEXT'] <- 'professional_status_8_text'
names(questions)[names(questions)=='Q1.17'] <- 'working_hours_week'
names(questions)[names(questions)=='Q1.18'] <- 'income_month'

write.csv(questions,"questionnaire.csv", row.names =FALSE)
### Overview of variables: 

#Q2.1: The Depression Anxiety Stress Scales (DASS-NL)
#Q2.2: child's development
#Q2.5: medical history/ medication

#Q3: Emotions PANAS
  # Negative Affect: items: 2,4,6,7,8,11,13,15,18,20

#Q4: Infant temperament

#Q5: Depression BDI-II

#Q6: Anxiety
  # subscales: social phobia?? 

#Q7: Emotions expressed when child present

#Q8: Who takes care of child


# Remove unfinished data and test 
#finished <- questions[questions$Finished == 1,]
#final <- finished[finished$ID != 'test',]
#final <- final[final$ID != '',]
final <- questions[questions$ID != 'test',]
final <- final[final$ID != ' ',]
final <- final[final$ID != '',]
final <- final[final$ID != 'evin',]
final <- final[final$ID != 'TEST',]
final <- final[final$ID != 'testmae',]

# Deal with duplicates
final$ID[duplicated(final$ID)]

# 204f
f204 <- final[final$ID == '204f',]
all.equal(f204[1,],f204[2,])
# keep later one (has less NA)
# remove the one with 'janson diederik' as a name

# 218m
m218 <- final[final$ID == '218m',]
# remove 'Rm Barends-Dekker'

# 225F
f225 <- final[final$ID == '225F',]
# one is mother --> rename
final$ID[final$initials == 'Schuil Maaike'] <- '225M'

# 221M
m221 <- final[final$ID == '221M',]
# remove name ''

# 220F
f220 <- final[final$ID == '220F',]
# remove Zinger Tom

# 223M
m223 <- final[final$ID == '223M',]

# Remove the one with partner Jorma de Ronde and the one with no name

remove_name <- c('janson diederik','Rm Barends-Dekker','','Zinger Tom')
remove_partner <- c('Jorma de Ronde')
final <- subset(final,!(final$initials %in% remove_name))
final <- final[!(final$partner_initials %in% remove_partner),]
final <- final[!(final$ID == '221M' & final$Finished == 0),]
final <- final[!(final$ID == '223M' & final$Finished == 0),]

# Test 
sort(final$ID)

# 218f and 218F
f218 <- final[final$ID == '218f' | final$ID == '218F',]


# Test if no duplicates
final$ID[duplicated(final$ID)]
final <- final[!(final$ID == '218F'),]

final$ID[final$ID == '213f'] <- '213M'
final$sex[final$ID == '214M'] <- 2
write.csv(final,"Data/Questionnaire/questionnaire.csv", row.names =FALSE)

final <- read.csv('Data/Questionnaire/questionnaire.csv',header=T)
# anxiety score 
library(dplyr)
library(tidyselect)
library(psy)

anxiety <- tidyselect::vars_select(names(final), starts_with('Q6'))


final$Anxiety_Score <- apply(final[,anxiety], 1, sum, na.rm=F)


# Mean anxiety score mothers 
mean(final$Anxiety_Score[final$sex==2], na.rm=TRUE) # 87.79
sd(final$Anxiety_Score[final$sex==2], na.rm=TRUE) # 11.29
range(final$Anxiety_Score[final$sex==2], na.rm=TRUE) # 74 - 114

# Mean anxiety score fathers
mean(final$Anxiety_Score[final$sex==1], na.rm=TRUE) # 84.47
sd(final$Anxiety_Score[final$sex==1], na.rm=TRUE) # 16.25
range(final$Anxiety_Score[final$sex==1], na.rm=TRUE) # 72 - 137

plot(final$Anxiety_Score)

library(ggplot2)
qplot(final$Anxiety_Score)

# Depression score
depression <- tidyselect::vars_select(names(final), starts_with('Q5'))
final$Depression_Score <- apply(final[,depression], 1, sum, na.rm=TRUE)

# Mean depression score mothers
mean(final$Depression_Score[final$sex==2], na.rm=TRUE) # 5.57
sd(final$Depression_Score[final$sex==2], na.rm=TRUE) # 4.44
range(final$Depression_Score[final$sex==2], na.rm=TRUE) # 0 - 17

# Mean depression score fathers
mean(final$Depression_Score[final$sex==1], na.rm=TRUE) # 4.65
sd(final$Depression_Score[final$sex==1], na.rm=TRUE) # 4.92
range(final$Depression_Score[final$sex==1], na.rm=TRUE) # 0 - 16

# Negative affect
neg_affect <- tidyselect::vars_select(names(final), starts_with('Q3'))
final$Neg_affect <- apply(final[,neg_affect],1,sum, na.rm = TRUE)

# Nean negative affect score mothers
mean(final$Neg_affect[final$sex==2], na.rm=TRUE) # 50.86
sd(final$Neg_affect[final$sex==2], na.rm=TRUE) # 6.17
range(final$Neg_affect[final$sex==2], na.rm=TRUE) # 39 - 61

# Mean negativ affect score fathers
mean(final$Neg_affect[final$sex==1], na.rm=TRUE) # 49.29
sd(final$Neg_affect[final$sex==1], na.rm=TRUE) # 6.69
range(final$Neg_affect[final$sex==1], na.rm=TRUE) # 38 - 61

#### Infant temperament 
temperament <- tidyselect::vars_select(names(final), starts_with('Q4'))
final$NegTemp <-apply(final[,temperament], 1, sum, na.rm=TRUE)


write.csv(final,"Data/Questionnaire/questionnaire.csv", row.names =FALSE)

#####
# descriptives
sum(final$sex == 1) # 17 men
sum(final$sex == 2) # 21 women

# Age 
mean(final$age[final$sex == 1])
sd(final$age[final$sex == 1])
range(final$age[final$sex == 1])
# Man: mean = 36, sd = 4.68, range = 30-44
mean(final$age[final$sex == 2])
sd(final$age[final$sex == 2])
range(final$age[final$sex == 2])
# Woman: mean = 33.42, sd = 3.88, range = 28-41

### Origin 
final$origin <- factor(final$origin)
summary(final$origin)
summary(final$origin[final$sex ==1])
summary(final$origin[final$sex ==2])
# 33 Netherlands, 1 Romania, 1 Slowakia, 1 Finland, 1 Belgium, 1 England
  # man: NL = 17
  # female: NL = 16, Romania = 1, Slowakia = 1 , Belgium = 1, England = 1, Finland = 1

16/21*100 # percentage women
  # percentage NL
    # men = 100%
    # women = 76.19%


# Wducational level 
  # men
sum(final$educational_level[final$sex ==1]==4) # 1 HAVO 
sum(final$educational_level[final$sex ==1]==5) # 2 VWO  
sum(final$educational_level[final$sex ==1]==6) # 1 MBO  
sum(final$educational_level[final$sex ==1]==7) # 2 HBO  
sum(final$educational_level[final$sex ==1]==8) # 10 University  
sum(final$educational_level[final$sex ==1]==9) # 0 PhD  

# Women 
sum(final$educational_level[final$sex ==2]==4) # 0 HAVO
sum(final$educational_level[final$sex ==2]==5) # 1 VWO  
sum(final$educational_level[final$sex ==2]==6) # 0 MBO
sum(final$educational_level[final$sex ==2]==7) # 3 HBO  
sum(final$educational_level[final$sex ==2]==8) # 17 University  
sum(final$educational_level[final$sex ==2]==9) # 0 

# Professional level 
# Men
sum(final$professional_level[final$sex ==1]==1)
sum(final$professional_level[final$sex ==1]==2)
sum(final$professional_level[final$sex ==1]==3)
sum(final$professional_level[final$sex ==1]==4)
sum(final$professional_level[final$sex ==1]==5)
sum(final$professional_level[final$sex ==1]==6)   # 2 Zelfstandig ondernemer met maximaal 4 werknemers
sum(final$professional_level[final$sex ==1]==7)
sum(final$professional_level[final$sex ==1]==8)
sum(final$professional_level[final$sex ==1]==9)   # 5 in loondienst op HBO niveau en niet leigingsgevend
sum(final$professional_level[final$sex ==1]==10)  # 4 in loondienst op HBO niveau en leigingsgevend
sum(final$professional_level[final$sex ==1]==11)  # 6 in loondienst waarbij een wetenschappelijke opleiding vereist is

# Women 
sum(final$professional_level[final$sex ==2]==1)
sum(final$professional_level[final$sex ==2]==2)
sum(final$professional_level[final$sex ==2]==3)
sum(final$professional_level[final$sex ==2]==4)   # 2 Overwegend hoofdarbeid waarbij een beroepsopleiding is vereist 
sum(final$professional_level[final$sex ==2]==5)
sum(final$professional_level[final$sex ==2]==6)   # 4 Zelfstandig ondernemer met maximaal 4 werknemers
sum(final$professional_level[final$sex ==2]==7)
sum(final$professional_level[final$sex ==2]==8)
sum(final$professional_level[final$sex ==2]==9)   # 1 in loondienst op HBO niveau en niet leigingsgevend
sum(final$professional_level[final$sex ==2]==10)  # 1 in loondienst op HBO niveau en leigingsgevend
sum(final$professional_level[final$sex ==2]==11)  # 13 in loondienst waarbij een wetenschappelijke opleiding vereist is


# Professional status
sum(final$professional_status[final$sex ==1]==1)
sum(final$professional_status[final$sex ==1]==2)
sum(final$professional_status[final$sex ==1]==3)  # 10 fulltime working
sum(final$professional_status[final$sex ==1]==4)  # 6 parttime working
sum(final$professional_status[final$sex ==1]==5)
sum(final$professional_status[final$sex ==1]==6)  # 
sum(final$professional_status[final$sex ==1]==7)
sum(final$professional_status[final$sex ==1]==8)  # 1 working and studying
# final$professional_status_8_text[final$sex ==1][final$professional_status == 8]

# Women N = 21
sum(final$professional_status[final$sex ==2]==1)  # 1 housewife 
sum(final$professional_status[final$sex ==2]==2)
sum(final$professional_status[final$sex ==2]==3)  # 5 fulltime working
sum(final$professional_status[final$sex ==2]==4)  # 15 parttime working
sum(final$professional_status[final$sex ==2]==5)
sum(final$professional_status[final$sex ==2]==6)   
sum(final$professional_status[final$sex ==2]==7)
sum(final$professional_status[final$sex ==2]==8)

final$ID[is.na(final$professional_status)]


# Monthly income N = 38
# 7-point Likert scale from 1 (<500) to 7 (>5000) per month
mean(final$income_month[final$sex ==1]) # men: 4.88 # about 3000-4000 euros
sd(final$income_month[final$sex ==1]) # 1.5
mean(final$income_month[final$sex ==2]) # women: 4.81
sd(final$income_month[final$sex ==2]) # 1.81

length(final$income_month[final$sex ==1]) # men
# Category 2 = 1 time, 3 = 2 times, 4 = 4 times, 5 = 3 times, 6 = 2 times, 7 = 4 times 
final$income_month[final$sex ==2] # women
# Category 3 = 2 times, 4 = 7 times, 5 = 2 times, 6 = 1 time, 7 = 4 times, 8 = 1 time (?)

# Working hours 
final$working_hours_week <- factor(final$working_hours_week)
summary(final$working_hours_week[final$sex ==1])
(15+30*2+32*3+34+35+36*4+38+40*4)/17 # = 34.24 mean men
sd(c(40,40,40,40,38,36,36,36,36,35,34,32,32,32,30,30,15)) # 6.03

summary(final$working_hours_week[final$sex ==2])
(40*2 + 38*2 + 36*5 + 32*5 + 28*2 + 26*1 + 24*1 + 20*1 +16*1 + 0*1)/21 # 30.1 mean women 
sd(c(40,40,38,38,36,36,36,36,36,32,32,32,32,32,28,28,26,24,20,16,0)) # 9.44

############################################################

