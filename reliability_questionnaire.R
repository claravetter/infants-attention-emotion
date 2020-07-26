# Prepare data
q <- read.csv("Data/Questionnaire/questionnaire.csv", header=T)

##### Libraries ######
library(dplyr)
library(tidyselect)
library(psy)

# anxiety score
anxiety <- tidyselect::vars_select(names(q), starts_with('Q6'))

q$ID[is.na(q$Anxiety_Score)] # "206m" "204f" "218f" "222M"

missing_anx <- q[q$ID == '206m' |q$ID == '204f'| q$ID == '218f'| q$ID == '222M',]
names <- c("ID",anxiety)
missing_anx <- missing_anx[,names] # they all did not fill it in at all 


# reliability of items
# mothers
mothers <- q[q$sex == 2,]

cronbach(mothers[,anxiety]) # number of items: 71, mothers: 18, c-alpha = 0.90

# fathers
fathers <- q[q$sex == 1,]
cronbach(fathers[,anxiety]) # number of items: 71, mothers: 16, c-alpha = 0.96

# neg temp infant
temperament <- tidyselect::vars_select(names(q), starts_with('Q4'))

q$ID[is.na(q$NegTemp)] # no missing 


cronbach(mothers[,temperament])
cronbach(fathers[,temperament])


# correlation anxiety

dat <- read.csv("Data/Questionnaire/Questionnaire_Excel.csv", 
                header = T,dec=",",sep = ";")

m_anx <- as.matrix(dat[,8:9])


library(Hmisc)
rcorr(m_anx, type="pearson") 

# correlation temp parentla ratings

m_temp <- as.matrix(dat[,10:11])
rcorr(m_temp, type="pearson") 

# correlation maternal and paternal anxiety and temp(averaged)

dat$MTemp <- apply(dat[c('mother.infant.temp','father.infant.temp')],
                   1,mean, na.rm=TRUE) 

m <- as.matrix(dat[,c(8:9,12:16)])
rcorr(m, type="pearson")
                            