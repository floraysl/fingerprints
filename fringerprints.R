setwd("~/Desktop/fingerprints")
library(readxl)
LPCdata<- read_xlsx("2018LPC-CaseData.xlsx")
LPCdata<- LPCdata[,-c(1,3,7,9,11,12)]
LPCdata<- LPCdata[,1:8]

for(i in 1:5) {
  for(j in 1:24558) {
    if(is.na(LPCdata[j,i])) {
      LPCdata[j,i] <- LPCdata[j-1,i]
    }
  }
for(j in 1:24558) {
  if((LPCdata$Sufficiency[j]=="AQ")&(is.na(LPCdata$`Anatomical source`[j])))
    LPCdata$`Anatomical source`[j] <- LPCdata$`Anatomical source`[j-1]
}


#regroup
replace <- function(input, toCheck, toBe) {
  #toCheck should be using regular expression
  if(!grepl(toCheck, input, ignore.case = TRUE)){
    return(input)
  }else{
    return(toBe)
  }
}
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "Attemp", "Attemp")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "Other|failure|burg|special|Unclassified|FSGI|officer|Elderly|Request", "Other")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "assault|harassment|stalking", "Assault")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "Kidnapping", "Kidnapping")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "robbery|theft|burglary|stolen", "Robbery")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "sex|rape|educator|exposure", "Sex")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "arson", "Arson")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "bomb", "bomb")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "hom|murder|dead|deadly", "Homicide")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "mischief|vice|disorderly|unauthorized", "Mischief")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "trespasing", "Trespasing")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "drugs|narcotics|Methamphetamin|Opium|Marijuana", "Drugs")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "traffic|vehicular|Vehicle", "Traffic")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "organized", "Organized_Crime")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "evade", "Evade")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "terroristics", "Terroristics")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "forgery", "Forgery")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "fraud", "Fraud")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "suicide", "Suicide")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "missing", "Missing")
LPCdata$Offense <- sapply(LPCdata$Offense, replace, "unlawful", "Weapon")

#change to factor
data = LPCdata
data$Offense = as.factor(data$Offense)
data$Analyst = as.factor(LPCdata$Analyst)
data$Sufficiency = as.factor(LPCdata$Sufficiency)
data$`Anatomical source` = as.factor(LPCdata$`Anatomical source`)
data$`AFIS system` = as.factor(LPCdata$`AFIS system`)
data$`AFIS Result` = as.factor(LPCdata$`AFIS Result`)


#examiner and total case
count1<-table(data$Analyst)
plot_totalcase<-barplot(count1, main = "Total Number of Evidence Processed by Each Examiner", xlab = "Examiner", ylab="Total Number of Evidence")

#examiner and sufficiency
#examiner dataset split 
Adam<- data[which(data$Analyst =="Adam"), ]
Becky <- data[which(data$Analyst =="Becky"), ]
Brenda<- data[which(data$Analyst =="Brenda"), ]
Cassaundra <- data[which(data$Analyst =="Cassaundra"), ]
David<-data[which(data$Analyst =="David"), ]
DJ <-data[which(data$Analyst =="DJ"), ]   
Gail<- data[which(data$Analyst =="Gail"), ] 
Jeniffer<- data[which(data$Analyst =="Jeniffer"), ]
Julian<- data[which(data$Analyst =="Julian"), ]
Maranda<- data[which(data$Analyst =="Maranda"), ]
Rodney<- data[which(data$Analyst =="Rodney"), ]
Samantha<- data[which(data$Analyst =="Samantha"), ]
Sandy<- data[which(data$Analyst =="Sandy"), ]
Stacie<- data[which(data$Analyst =="Stacie"), ]
Starla<- data[which(data$Analyst =="Starla"), ]
Todd<- data[which(data$Analyst =="Todd"), ]
Tracy<- data[which(data$Analyst =="Tracy"), ]
Vickie<- data[which(data$Analyst =="Vickie"), ] 

library('plyr')
count(Adam, 'Sufficiency')
count(Becky, 'Sufficiency')
count(Cassaundra, 'Sufficiency')
count(Brenda, 'Sufficiency')
count(David, 'Sufficiency')
count(DJ, 'Sufficiency')
count(Gail, 'Sufficiency')
count(Jeniffer, 'Sufficiency')
count(Maranda, 'Sufficiency')
count(Rodney,'Sufficiency')
count(Samantha,'Sufficiency')
count(Sandy,'Sufficiency')
count(Stacie,'Sufficiency')
count(Starla,'Sufficiency')
count(Tracy,'Sufficiency')
count(Vickie,'Sufficiency')
#mid polish
suff <- read.csv("~/Desktop/fingerprints/suff.csv")
suff_remove<-as.data.frame.matrix(suff[,-c(1,2)])
suff.med <- medpolish(scale(suff_remove))
suff.med



#off and exa
count(Adam, 'Offense')
count(Becky, 'Offense')
count(Cassaundra, 'Offense')
count(Brenda, 'Offense')
count(David, 'Offense')
count(DJ, 'Offense')
count(Gail, 'Offense')
count(Jeniffer, 'Offense')
count(Maranda, 'Offense')
count(Rodney,'Offense')
count(Sandy,'Offense')
count(Stacie,'Offense')
count(Starla,'Offense')
count(Tracy,'Offense')
count(Vickie,'Offense')
#med polish 
off_exa <- read.csv("~/Desktop/fingerprints/off_exa.csv")
off_exa_remove<-as.data.frame.matrix(off_exa[,-c(1,7,8)])
off_exa.med <- medpolish(scale(off_exa_remove))
off_exa.med

#exa and ana 
count(Adam, '`Anatomical source`')
count(Becky, '`Anatomical source`')
count(Cassaundra, '`Anatomical source`')
count(Brenda, '`Anatomical source`')
count(David, '`Anatomical source`')
count(DJ, '`Anatomical source`')
count(Gail, '`Anatomical source`')
count(Jeniffer, '`Anatomical source`')
count(Maranda, '`Anatomical source`')
count(Rodney,'`Anatomical source`')
count(Sandy,'`Anatomical source`')
count(Stacie,'`Anatomical source`')
count(Starla,'`Anatomical source`')
count(Tracy,'`Anatomical source`')
count(Vickie,'`Anatomical source`')
#medpolish
ana <- read.csv("~/Desktop/fingerprints/ana.csv")
ana_remove<-as.data.frame.matrix(ana[,-1])
ana.med <- medpolish(scale(ana_remove))
ana.med


#off and result 
count(Adam, '`AFIS Result`')
count(Becky, '`AFIS Result`')
count(Cassaundra, '`AFIS Result`')
count(Brenda, '`AFIS Result`')
count(David, '`AFIS Result`')
count(DJ, '`AFIS Result`')
count(Gail, '`AFIS Result`')
count(Jeniffer, '`AFIS Result`')
count(Maranda, '`AFIS Result`')
count(Rodney,'`AFIS Result`')
count(Sandy,'`AFIS Result`')
count(Stacie,'`AFIS Result`')
count(Starla,'`AFIS Result`')
count(Tracy,'`AFIS Result`')
count(Vickie,'`AFIS Result`')
#medpolish
off_result <- read.csv("~/Desktop/fingerprints/off_result.csv")
off_result_remove<-as.data.frame.matrix(off_result[,-1])
off_result.med <- medpolish(scale(off_result_remove))
off_result.med


#examiner and total case
Result<-table(data$`AFIS Result`)
plot_Result<-barplot(Result, main = "Prevalence of HFSC examiner conclusion", xlab = "Conclusions")
