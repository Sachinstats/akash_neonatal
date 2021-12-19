library(haven)
library(ggplot2)
library(tidyverse)
library(ggsci);library(hrbrthemes)
library(haven)
ak2 <- read_sav("C:/Users/Sachin/Desktop/all data/akash/ak2.sav")
patientid=seq(1:31)
a=data.frame(patientid,ak2)
r=rep(patientid,ak2$Lengthofstay[])
ng1=matrix(c(seq(1:ak2$Lengthofstay[1])),nrow=ak2$Lengthofstay[1],byrow=TRUE)
ng2=matrix(c(seq(1:ak2$Lengthofstay[2])),nrow=ak2$Lengthofstay[2],byrow=TRUE)
ng3=matrix(c(seq(1:ak2$Lengthofstay[3])),nrow=ak2$Lengthofstay[3],byrow=TRUE)
ng4=matrix(c(seq(1:ak2$Lengthofstay[4])),nrow=ak2$Lengthofstay[4],byrow=TRUE)
ng5=matrix(c(seq(1:ak2$Lengthofstay[5])),nrow=ak2$Lengthofstay[5],byrow=TRUE)
ng6=matrix(c(seq(1:ak2$Lengthofstay[6])),nrow=ak2$Lengthofstay[6],byrow=TRUE)
ng7=matrix(c(seq(1:ak2$Lengthofstay[7])),nrow=ak2$Lengthofstay[7],byrow=TRUE)
ng8=matrix(c(seq(1:ak2$Lengthofstay[8])),nrow=ak2$Lengthofstay[8],byrow=TRUE)
ng9=matrix(c(seq(1:ak2$Lengthofstay[9])),nrow=ak2$Lengthofstay[9],byrow=TRUE)
ng10=matrix(c(seq(1:ak2$Lengthofstay[10])),nrow=ak2$Lengthofstay[10],byrow=TRUE)
ng11=matrix(c(seq(1:ak2$Lengthofstay[11])),nrow=ak2$Lengthofstay[11],byrow=TRUE)
ng12=matrix(c(seq(1:ak2$Lengthofstay[12])),nrow=ak2$Lengthofstay[12],byrow=TRUE)
ng13=matrix(c(seq(1:ak2$Lengthofstay[13])),nrow=ak2$Lengthofstay[13],byrow=TRUE)
ng14=matrix(c(seq(1:ak2$Lengthofstay[14])),nrow=ak2$Lengthofstay[14],byrow=TRUE)
ng15=matrix(c(seq(1:ak2$Lengthofstay[15])),nrow=ak2$Lengthofstay[15],byrow=TRUE)
ng16=matrix(c(seq(1:ak2$Lengthofstay[16])),nrow=ak2$Lengthofstay[16],byrow=TRUE)
ng17=matrix(c(seq(1:ak2$Lengthofstay[17])),nrow=ak2$Lengthofstay[17],byrow=TRUE)
ng18=matrix(c(seq(1:ak2$Lengthofstay[18])),nrow=ak2$Lengthofstay[18],byrow=TRUE)
ng19=matrix(c(seq(1:ak2$Lengthofstay[19])),nrow=ak2$Lengthofstay[19],byrow=TRUE)
ng20=matrix(c(seq(1:ak2$Lengthofstay[20])),nrow=ak2$Lengthofstay[20],byrow=TRUE)
ng21=matrix(c(seq(1:ak2$Lengthofstay[21])),nrow=ak2$Lengthofstay[21],byrow=TRUE)
ng22=matrix(c(seq(1:ak2$Lengthofstay[22])),nrow=ak2$Lengthofstay[22],byrow=TRUE)
ng23=matrix(c(seq(1:ak2$Lengthofstay[23])),nrow=ak2$Lengthofstay[23],byrow=TRUE)
ng24=matrix(c(seq(1:ak2$Lengthofstay[24])),nrow=ak2$Lengthofstay[24],byrow=TRUE)
ng25=matrix(c(seq(1:ak2$Lengthofstay[25])),nrow=ak2$Lengthofstay[25],byrow=TRUE)
ng26=matrix(c(seq(1:ak2$Lengthofstay[26])),nrow=ak2$Lengthofstay[26],byrow=TRUE)
ng27=matrix(c(seq(1:ak2$Lengthofstay[27])),nrow=ak2$Lengthofstay[27],byrow=TRUE)
ng28=matrix(c(seq(1:ak2$Lengthofstay[28])),nrow=ak2$Lengthofstay[28],byrow=TRUE)
ng29=matrix(c(seq(1:ak2$Lengthofstay[29])),nrow=ak2$Lengthofstay[29],byrow=TRUE)
ng30=matrix(c(seq(1:ak2$Lengthofstay[30])),nrow=ak2$Lengthofstay[30],byrow=TRUE)
ng31=matrix(c(seq(1:ak2$Lengthofstay[31])),nrow=ak2$Lengthofstay[31],byrow=TRUE)

ngg=rbind(ng1,ng2,ng3,ng4,ng5,ng6,ng7,ng8,ng9,ng10,ng11,ng12,ng13,ng14,ng15,ng16,ng17,
          ng18,ng19,ng20,ng21,ng22,ng23,ng24,ng25,ng26,ng27,ng28,ng29,ng30,ng31)
v1=cbind(r,ngg)
library(readxl)
v11= write.csv2(v1,"v1.csv")
head(v)
v1 <- read_excel("v1.xlsx")

v1$time.days.=as.numeric(v1$time.days.)
v1$Patient_ID=factor(v1$Patient_ID)
library(tidyverse)
v1%>%mutate(status1=factor(Status,level=c("Hepaticence","SBP","systemicinfection","ascites")))
p=ggplot(v1,aes(x=time, y=patientid, xlab(Patient_id),ylab(time),group=patientid,shape=Status))+
  geom_line(aes(group=patientid),color="BLACK")+
  geom_point()+
  p+theme()
p
names(v1)
v1%>%mutate(status1=factor(Status,level=c("PD")))
p=ggplot(v1,aes(y=Patient_ID,x=`time(days)`,color=Status,shape=Status,group=Event))+
  geom_line(aes(group=Patient_ID),color="BLACK",size=0.5)+
  geom_point(size=1)
p
theme_ipsum(base_size=9)
theme(legend.position="top",
      panel.grid.)
#######
library(readxl)

 v1 <- read_excel("v1.xlsx", sheet = "Sheet1")

 v1$time.days.=as.numeric(v1$`time(days)`)
 v1$Patient_ID=factor(v1$Patient_ID)
 p=ggplot(v1,aes(y=Patient_ID,x=time.days.,color=Status,shape=Status,group=Event))+
   geom_line(aes(group=Patient_ID),color="BLACK",size=0.5)+
   geom_point(size=3)
 p + labs(colour="Sepsis")
########
 library(readxl)
 
 v1 <- read_excel("v1.xlsx", sheet = "Sheet1")
 
 v1$time.days.=as.numeric(v1$`time(days)`)
 v1$Patient_ID=factor(v1$Patient_ID)
 p=ggplot(v1,aes(y=Patient_ID,x=time.days.,color=Status,shape=Status,group=Event))+
   geom_line(aes(group=Patient_ID),color="BLACK",size=0.5)+
   geom_point(size=3)
 p + labs(colour="Sepsis")
 ######
 library(readxl)
 
 v1 <- read_excel("v1.xlsx", sheet = "Sheet2")
 
 v1$time.days.=as.numeric(v1$`time(days)`)
 v1$Patient_ID=factor(v1$Patient_ID)
 p=ggplot(v1,aes(y=Patient_ID,x=time.days.,color=Status,shape=Status,group=Event))+
   geom_line(aes(group=Patient_ID),color="BLACK",size=0.5)+
   geom_point(size=3)
p+labs(colour="Nephrotoxic_Drug")
 ########
 library(readxl)
 
 v1 <- read_excel("v1.xlsx", sheet = "Sheet3")
 
 v1$time.days.=as.numeric(v1$`time(days)`)
 v1$Patient_ID=factor(v1$Patient_ID)
 p=ggplot(v1,aes(y=Patient_ID,x=time.days.,color=Status,shape=Status,group=Event))+
   geom_line(aes(group=Patient_ID),color="BLACK",size=0.5)+
   geom_point(size=3)
 p + labs(colour="CHD ")
 #########
 library(readxl)
 
 v1 <- read_excel("v1.xlsx", sheet = "Sheet4")
 
 v1$time.days.=as.numeric(v1$`time(days)`)
 v1$Patient_ID=factor(v1$Patient_ID)
 p=ggplot(v1,aes(y=Patient_ID,x=time.days.,color=Status,shape=Status,group=Event))+
   geom_line(aes(group=Patient_ID),color="BLACK",size=0.5)+
   geom_point(size=3)
 p + labs(colour="Abnormal_USG")
 ####
 library(readxl)
 
 v1 <- read_excel("v1.xlsx", sheet = "Sheet5")
 
 v1$time.days.=as.numeric(v1$`time(days)`)
 v1$Patient_ID=factor(v1$Patient_ID)
 p=ggplot(v1,aes(y=Patient_ID,x=time.days.,color=Status,shape=Status,group=Event))+
   geom_line(aes(group=Patient_ID),color="BLACK",size=0.5)+
   geom_point(size=3)
 p + labs(colour="Medical_Management")

 #######
 library(readxl)
 
 v1 <- read_excel("v1.xlsx", sheet = "Sheet6")
 
 v1$time.days.=as.numeric(v1$`time(days)`)
 v1$Patient_ID=factor(v1$Patient_ID)
 p=ggplot(v1,aes(y=Patient_ID,x=time.days.,color=Status,shape=Status,group=Outcome))+
   geom_line(aes(group=Patient_ID),color="BLACK",size=0.5)+
   geom_point(size=3)
 p
 