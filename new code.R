library(readxl)
library(finalfit)
library(dplyr)
library(ggplot2)
library(likert)
library(patchwork)
library(knitr)
library(kableExtra)
library(tidyverse)
library(tidytext)
data= read_excel("EXCEL  AKI 27 nov F whole - FINAL.xlsx")
data1=data[,c(1,24,28,31,41,47,49,51,54,57,59,65,66,67,68,70,71)]#subset
data1$`oligo=1, poly=2, NR=3`=as.factor(data1$`oligo=1, poly=2, NR=3`)
data1$`Grade of AKI...54`=as.factor(data1$`Grade of AKI...54`)
#box plot baseline_cr
p <- ggplot(data1, aes(y=`baseline cr`))+
  geom_boxplot()+
labs( y = "Baseline_creatnine")
p+ scale_fill_grey() + theme_classic()
#boxplot baseline_cr with Oligouric

p1<- ggplot(data1, aes(x=`oligo=1, poly=2, NR=3`,y=`baseline cr`,fill=`oligo=1, poly=2, NR=3`
                       ,color=`oligo=1, poly=2, NR=3`))+
  geom_boxplot()+
  labs( y = "Baseline_creatnine",x="Olioguric")
p1+ scale_fill_grey() + theme_classic()
#box plot baseline_cr with grade of AKI
p2<- ggplot(data1, aes(x=`Grade of AKI...54`,y=`baseline cr`,fill=`Grade of AKI...54`,
                       ,color=`Grade of AKI...54`))+
  geom_boxplot()+
  labs( y = "Baseline_creatnine",x="Stage of AkI")
p2+ scale_fill_grey() + theme_classic()
########barplot Olioguric
library(dplyr)
df <- data1 %>%
  group_by(`oligo=1, poly=2, NR=3`) %>%
  summarise(counts = n())
df=na.omit(df)#find out the frequency
ggplot(df, aes(x = `oligo=1, poly=2, NR=3`, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) +labs( x="Olioguric")
###########bar plot aki
df1=data1 %>%
  group_by(`Grade of AKI...54`) %>%
  summarise(counts = n())
df1=na.omit(df1)#find out the frequency
ggplot(df1, aes(x = `Grade of AKI...54`, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) 

#############logistic regression
names(data1)
glimpse(data1)
data1$SEPSIS =as.factor(data1$SEPSIS )
data1$CHD =as.factor(data1$`CHD, yes=1, no=2` )
data1$`outcome-discharge=1, death=2`=as.factor(data1$`outcome-discharge=1, death=2`)
data1$NEPHROTOXIC_DRUG=as.factor(data1$`NEPHROTOXIC DRUG`)
data1$CAKUT=as.factor(data1$CAKUT)
data1$ABNORMAL=as.factor(data1$`ABNORMAL PN  USG`)
data1$UTI=as.factor(data1$UTI)
data1$outcome=data1$`outcome-discharge=1, death=2`
explanatory = c("SEPSIS", "CHD ", "NEPHROTOXIC_DRUG","CAKUT","ABNORMAL"
                ,"UTI")
dependent = "outcome" 
data1 %>% 
  finalfit(dependent, explanatory, 
           dependent_label_prefix = "") -> table1
table1
data1%>%
  summary_factorlist(dependent, explanatory, 
                     p=TRUE, na_include=TRUE)

data1 %>%
  or_plot_code(dependent, explanatory)
#########Survival plot########there is some error
library(survival)
names(data1)
data1$`Length of stay`
km_fit <- survfit(Surv(`Length of stay`, outcome) ~CHD, data=data1)
summary(km_fit)
plot(km_fit, xlab="length of stay", main = 'Kaplan Meyer Plot')
library(ggplot2)
library("survminer")
fit1 <- survfit(Surv(Lengthofstay, outcome) ~ NEPHROTOXICDRUG_A, data = akash)
summary(fit1)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

km_trt_fit <- survfit(Surv(`Length of stay`, outcome) ~ CHD, data=data1)
plot(km_trt_fit)
library(Rcmdr)
######for paired test#######

#akash=na.omit(akash)
data1$prePD
library(tidyverse)
names(data1)
test <- data1 %>%
  pivot_longer(c(prePD, postPD), names_to = "BB", values_to = "cases") %>% mutate(paired = rep(1:(n()/2),each=2),                                                                                                               BB=factor(BB)) %>%
  select(BABY.OF...1, BB, cases, paired) %>%
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
names(data1)
test1 <- data1 %>%
  pivot_longer(c(baseline.cr, Last.creat), names_to = "BB", values_to = "cases") %>% mutate(paired = rep(1:(n()/2),each=2),                                                                                                               BB=factor(BB)) %>%
  select(BABY.OF...1, BB, cases, paired) %>%
  rename(Creatnine = cases)
library(ggstatsplot)
test1=na.omit(test1)
set.seed(123)
# Mann-Whitney U test (nonparametric test)
ggstatsplot::ggwithinstats(
  data = dplyr::filter(
    test1,
    BB%in%c("baseline.cr","Last.creat")),
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
glimpse(data1)
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
