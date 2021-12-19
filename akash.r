library(haven)
setwd("C:/Users/Sachin/Desktop/all data/akash/akash data/akash_neonatal")# set directory
akash<- read_sav("akash.sav")
names(akash)
glimpse(akash)
library(reshape2)
library(ggplot2)
#data11 = melt(akash$BaselinesCr)
library(tidyverse)

akash %>% 
ggplot(aes( x = "BaselineCr")) + 
  geom_boxplot()  + theme_bw()

p1=p1+xlab("")+ylab("Baseline Creatnine(log)")+theme(
  axis.title.x = element_text(color="black", size=10, face="bold"),
  axis.title.y = element_text(color="black", size=10, face="bold")
) 
p1 + ylim(0,2)
h=c(3,10,18)
M <- c("I","II","III")
p2=barplot(h,names.arg=M,xlab="Grade of AKI",ylab="frequency",col="blue",
        main="",border="red")
df <- data.frame(grade_of_aki=c("I","II","III"),
                 frequency=c(3,10,18))
library(ggplot2)
# Basic barplot
p2<-ggplot(data=df, aes(x=grade_of_aki, y=frequency)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("red", "green", "blue") ) +
  theme(legend.position="none")
p2+xlab("Staes ofAKI (KDIGO)")+ylab("Frequency")+theme(
  axis.title.x = element_text(color="black", size=10, face="bold"),
  axis.title.y = element_text(color="black", size=10, face="bold")
) 
head(df)
######
df1<-data.frame(Oliguric_Non_oliguric=c("Oliguric","Non_Oliguric"), frequency=c(22,9))
p3=ggplot(data=df1,aes(x=Oliguric_Non_oliguric, y=frequency))+
  geom_bar(stat="identity",color="blue")
p3+xlab("Urea")+ylab("Frequency")+theme(
  axis.title.x = element_text(color="black", size=10, face="bold"),
  axis.title.y = element_text(color="black", size=10, face="bold")
) 

###
library(reshape2)
library(ggplot2)
attach(akash)
data12=akash[,c(25,9)]
head(data12)
data12 = melt(log(akash$BaselinesCr),id.vars = c("olignonoligAKI"))
p4= ggplot(data12,aes(x=olignonoligAKI, y = log(BaselinesCr), fill = as.factor(olignonoligAKI))) + geom_boxplot()  + theme_bw() + scale_fill_discrete(name ="olignonoligAKI", labels = c("1=Oliguric","2=Normal wine Oliguric"))
p4+xlab("Oliguric")+ylab("Baseline Creatnine")+theme(
  axis.title.x = element_text(color="black", size=10, face="bold"),
  axis.title.y = element_text(color="black", size=10, face="bold")
)  

data13=akash[,c(25,18)]
p5= ggplot(data13,aes(x=GradeofAKI, y = log(BaselinesCr), fill = as.factor(GradeofAKI))) + geom_boxplot()  + theme_bw() + scale_fill_discrete(name ="GradeofAKI", labels = c("I", "II","III"))
p5+xlab("Grade of AKI")+ylab("Baseline Creatnine")+theme(
  axis.title.x = element_text(color="black", size=10, face="bold"),
  axis.title.y = element_text(color="black", size=10, face="bold")
)  
library(patchwork)
(p1+p2+p3)/(p4+p5)
####

library(haven);library(finalfit);library(dplyr)
library(ggplot2)
names(akash)
akash$Lengthofstay
glimpse(akash)
akash$NEPHROTOXICDRUG_A=factor(akash$NEPHROTOXICDRUG_A)
akash$CHD=factor(akash$CHD)
akash$SEPSIS=factor(akash$SEPSIS)
akash$ABNORMALUSG=factor(akash$ABNORMALUSG)
akash$MEDICALMANAGEMENT=factor(akash$MEDICALMANAGEMENT)

akash$outcome=factor(akash$outcome)
glimpse(akash)
attach(akash)
explanatory = c("CHD", "SEPSIS", "ABNORMALUSG")
dependent = "outcome" 
akash %>% 
  finalfit(dependent, explanatory, 
           dependent_label_prefix = "") -> table1
table1
akash%>%
  summary_factorlist(dependent, explanatory, 
                     p=TRUE, na_include=TRUE)

akash %>%
  or_plot_code(dependent, explanatory)

#########
library(survival)
km_fit <- survfit(Surv(Lengthofstay, outcome) ~CHD, data=akash)
summary(km_fit)
plot(km_fit, xlab="length of stay", main = 'Kaplan Meyer Plot')
library(ggplot2)
library("survminer")
fit1 <- survfit(Surv(Lengthofstay, outcome) ~ NEPHROTOXICDRUG_A, data = akash)
summary(fit1)
ggsurvplot(fit1, data = akash, pval = TRUE)
#######
cox <- coxph(Surv(Lengthofstay, outcome) ~ CHD, data = akash)
summary(cox)
akash$outcome


##########
v$time(months)=as.numeric(v$time.months.)
v$patientid=factor(v$patientid)
v%>%
  mutate(status1=factor(status,level=c("Hepaticence","SBP","systemicinfection","ascites")))
p=ggplot(v,aes(x=time, y=patientid, xlab(Patient_id),ylab(time),group=patientid,shape=status))+
  geom_line(aes(group=patientid),color="BLACK")+
  geom_point()+
  p+theme()
p
ggplot(v,aes(y=patientid,x=time.months.,color=status,shape=status,group=event))+
  geom_line(aes(group=patientid),color="BLACK",size=1)+
  geom_point(size=3)+
  scale_shape_manual(values = c(15:24))
theme_ipsum(base_size=9)
theme(legend.position="top",
      panel.grid.)

##############
######for paired test#######
akash<- read_sav("C:/Users/Sachin/Desktop/all data/akash/akash.sav")

#akash=na.omit(akash)
akash$prePD
library(tidyverse)
names(akash)
test <- akash %>%
  pivot_longer(c(prePD, postPD), names_to = "BB", values_to = "cases") %>% mutate(paired = rep(1:(n()/2),each=2),                                                                                                               BB=factor(BB)) %>%
  select(BABYOF, BB, cases, paired) %>%
  rename(PD = cases)
library(ggstatsplot)
test=na.omit(test)
set.seed(123)
# Mann-Whitney U test (nonparametric test)
ggstatsplot::ggwithinstats(
  data = dplyr::filter(
    test,
    BB%in%c("prePD","postPD")),
  x = BB,
  y = PD,
  type = "np",#non-parametric test
  conf.level = 0.99,
  title = "PD",
  xlab = "PD",
  ylab = "PD",
  package = "ggsci",
  palette = "uniform_startrek",
  ggtheme = ggthemes::theme_map(),
  outlier.tagging = TRUE
  )
##################
names(akash)
test1 <- akash %>%
  pivot_longer(c(BaselinesCr, Lastcr), names_to = "BB", values_to = "cases") %>% mutate(paired = rep(1:(n()/2),each=2),                                                                                                               BB=factor(BB)) %>%
  select(BABYOF, BB, cases, paired) %>%
  rename(Creatnine = cases)
library(ggstatsplot)
test1=na.omit(test1)
set.seed(123)
# Mann-Whitney U test (nonparametric test)
ggstatsplot::ggwithinstats(
  data = dplyr::filter(
    test1,
    BB%in%c("BaselinesCr","Lastcr")),
  x = BB,
  y = Creatnine,
  type = "np",#non-parametric test
  conf.level = 0.99,
  title = "Creatnine",
  xlab = "",
  ylab = "Creatnine(mg/day)",
  package = "ggsci",
  palette = "uniform_startrek",
  ggtheme = ggthemes::theme_map(),
  outlier.tagging = TRUE
)
#####
# common setup
set.seed(123)
glimpse(akash)
test$BB=as.character(test$BB)
# plot
grouped_ggwithinstats(
  data = dplyr::filter(
    test,
        BB %in% c("prePD", "postPD")
  ),
  x = BB,
  y = PD,
  type = "np", # non-parametric statistics
  xlab = "BB",
  ylab = "PD",
  outlier.tagging = TRUE,
  outlier.label = education
)

set.seed(123)
library(WRS2) # for data
library(afex) # to run anova

# plot
ggwithinstats(
  data = WineTasting,
  x = Wine,
  y = Taste,
  title = "Wine tasting",
  caption = "Data source: `WRS2` R package",
  ggtheme = ggthemes::theme_fivethirtyeight()
)
