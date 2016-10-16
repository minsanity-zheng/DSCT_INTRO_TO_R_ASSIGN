library(dplyr)
library(ggplot2)
library(scales)
# Load Data
NORC<-read.csv(file = "./DSCT_INTRO_TO_R_ASSIGN/35478-0001-Data.csv",
               header = TRUE,sep = ",")
NORCsubset<-NORC[c("MARITAL","AGE","GENDER1","HOMPOP","HAPPY",
                   "HEALTH","SATJOB","REALINC","RACE")]

# Q2 & Q3
NORCsubset$MARITAL[NORCsubset$MARITAL==9]<-NA
NORCsubset$MARITAL[NORCsubset$MARITAL==1]<-"Married"
NORCsubset$MARITAL[NORCsubset$MARITAL==2]<-"Widowed"
NORCsubset$MARITAL[NORCsubset$MARITAL==3]<-"Divorced"
NORCsubset$MARITAL[NORCsubset$MARITAL==4]<-"Separated"
NORCsubset$MARITAL[NORCsubset$MARITAL==5]<-"Never married"

NORCsubset$AGE[NORCsubset$AGE>97]<-NA

NORCsubset$GENDER1[NORCsubset$GENDER1==0 | NORCsubset$GENDER1>7]<-NA
NORCsubset$GENDER1[NORCsubset$GENDER1==1]<-"Male"
NORCsubset$GENDER1[NORCsubset$GENDER1==2]<-"Female"

NORCsubset$HOMPOP[NORCsubset$HOMPOP>97]<-NA

NORCsubset$HAPPY[NORCsubset$HAPPY==0 | NORCsubset$HAPPY>7]<-NA
NORCsubset$HAPPY[NORCsubset$HAPPY==1]<-"Very happy"
NORCsubset$HAPPY[NORCsubset$HAPPY==2]<-"Pretty happy"
NORCsubset$HAPPY[NORCsubset$HAPPY==3]<-"Not too happy"

NORCsubset$HEALTH[NORCsubset$HEALTH==0 | NORCsubset$HEALTH>7]<-NA
NORCsubset$HEALTH[NORCsubset$HEALTH==1]<-"Excellent"
NORCsubset$HEALTH[NORCsubset$HEALTH==2]<-"Good"
NORCsubset$HEALTH[NORCsubset$HEALTH==3]<-"Fair"
NORCsubset$HEALTH[NORCsubset$HEALTH==4]<-"Poor"

NORCsubset$SATJOB[NORCsubset$SATJOB==0 | NORCsubset$SATJOB>7]<-NA
NORCsubset$SATJOB[NORCsubset$SATJOB==1]<-"Very satisfied"
NORCsubset$SATJOB[NORCsubset$SATJOB==2]<-"Moderately satisfied"
NORCsubset$SATJOB[NORCsubset$SATJOB==3]<-"3 A little dissatisfied"
NORCsubset$SATJOB[NORCsubset$SATJOB==4]<-"Very dissatisfied"

NORCsubset$REALINC[NORCsubset$REALINC==0|NORCsubset$REALINC>=999998.00]<-NA

NORCsubset$RACE[NORCsubset$RACE==0]<-NA
NORCsubset$RACE[NORCsubset$RACE==1]<-"White"
NORCsubset$RACE[NORCsubset$RACE==2]<-"Black"
NORCsubset$RACE[NORCsubset$RACE==3]<-"Other"

# Q1
IncompleteData<-NORCsubset[rowSums(is.na(NORCsubset))>0,]

# Q4
NORCsubset<-mutate(NORCsubset,percapinc = REALINC / HOMPOP)
NORCsubset$percapinc[NORCsubset$percapinc==0]<-NA
sum(is.na(NORCsubset$percapinc))

# Q5
summary(NORCsubset$percapinc)
boxplot(NORCsubset$percapinc,outline = FALSE)

# Q6
GH<-na.omit(NORCsubset[,c("GENDER1","HAPPY")])
Count_G<-table(GH)
Count_G2<-prop.table(Count_G,1)
mydf <- data.frame(Count_G2)
ggplot(mydf ,aes(GENDER1, Freq, fill = HAPPY)) + 
    geom_bar(position = "dodge", stat = "identity") +
    scale_y_continuous(labels=scales::percent) 

chisq.test(Count_G)

# Q7
NORCsubset$AGE[NORCsubset$AGE>17 & NORCsubset$AGE<=35]<-"Young adult"
NORCsubset$AGE[NORCsubset$AGE>35 & NORCsubset$AGE<=55]<-"Middle-aged adult"
NORCsubset$AGE[NORCsubset$AGE>55 & NORCsubset$AGE<=97]<-"Elderly"

AH<-na.omit(NORCsubset[,c("AGE","HAPPY")])
Count_A<-table(AH)
Count_A2<-prop.table(Count_A,1)
mydf <- data.frame(Count_A2,2)
ggplot(mydf ,aes(AGE, Freq, fill = HAPPY)) + 
    geom_bar(position = "dodge", stat = "identity") +
    scale_y_continuous(labels=scales::percent)

chisq.test(Count_A)

# Q8
WH<-na.omit(NORCsubset[,c("percapinc","HAPPY")])
summary(WH$percapinc[WH$HAPPY=="Very happy"])
summary(WH$percapinc[WH$HAPPY=="Pretty happy"])
summary(WH$percapinc[WH$HAPPY=="Not too happy"])
ggplot(WH ,aes(HAPPY,percapinc)) + 
    geom_boxplot(aes(colour = factor(HAPPY)),outlier.shape = NA) +
    coord_cartesian(ylim = (boxplot.stats(WH$percapinc)$stats[c(1, 5)])*1.05)

# Q9
WHL<-na.omit(NORCsubset[,c("percapinc","HEALTH")])
summary(WHL$percapinc[WHL$HEALTH=="Excellent"])
summary(WHL$percapinc[WHL$HEALTH=="Good"])
summary(WHL$percapinc[WHL$HEALTH=="Fair"])
summary(WHL$percapinc[WHL$HEALTH=="Poor"])
ggplot(WHL ,aes(HEALTH,percapinc)) + 
    geom_boxplot(aes(colour = factor(HEALTH)),outlier.shape = NA) +
    coord_cartesian(ylim = (boxplot.stats(WH$percapinc)$stats[c(1, 5)])*1.05)

# Q10
MH<-na.omit(NORCsubset[,c("MARITAL","HAPPY")])
Count_MH<-table(MH)
Count_MH2<-prop.table(Count_MH,1)
mydf <- data.frame(Count_MH2,2)
ggplot(mydf ,aes(MARITAL, Freq, fill = HAPPY)) + 
    geom_bar(position = "dodge", stat = "identity") +
    scale_y_continuous(labels=scales::percent)

chisq.test(Count_MH)

# Q11
WR<-na.omit(NORCsubset[,c("REALINC","RACE")])
W<-summary(WR$REALINC[WR$RACE=="White"])
B<-summary(WR$REALINC[WR$RACE=="Black"])
O<-summary(WR$REALINC[WR$RACE=="Other"])
RR<-cbind(W,B,O)
chisq.test(RR)