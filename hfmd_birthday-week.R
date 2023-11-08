#Remove objects
rm(list = ls())
#Install and load packages
{
install.packages("lubridate");install.packages("tidyverse");install.packages("reshape2");install.packages("data.table");install.packages("readxl")
install.packages("ggplot2");install.packages("customLayout");install.packages("cowplot");install.packages("CATT");install.packages("ggpp")
install.packages("devtools")
install.packages("remotes")#ggpattern未托管在CRAN上，使用remotes::install_github安装
remotes::install_github("coolbutuseless/ggpattern")
devtools::install_github("thomasp85/patchwork")
}
library(readxl);library(ggpp);library(lubridate);library(tidyverse);library(reshape2);library(data.table);
library(ggplot2);library(cowplot);library(patchwork);library(ggpattern);library(CATT)

#read data
getwd()
setwd("C:/Users/lenovo/Nutstore/1/我的坚果云/2. 科研/生日周现象-手足口/data")
hfmd<-read_excel("hfmd.xlsx")

#display and summarize data
str(hfmd)
summary(hfmd)

#select variables
hfmd<-hfmd%>%
  select("性别","出生日期","发病日期")

#rename selected variables
colnames<-c("sex","birthdate","hfmddate")
for(i in 1:3){
  colnames(hfmd)[i]<-colnames[i]
}
#set variable type:as.factor()and as.Date()
{
  hfmd$sex<-as.factor(hfmd$sex)
  hfmd$birthdate<-as.Date(hfmd$birthdate)
  hfmd$hfmddate<-as.Date(hfmd$hfmddate)
}

#########################################################################
############get afterbirthday and agebydays##############################
{
  #①#####Get the year, month, and day of birth and onset by BIRTHDATE and HFMDDATE
         {
    hfmd$birthyear<-year(hfmd$birthdate);
    hfmd$birthmonth<-month(hfmd$birthdate);
    hfmd$birthday<-day(hfmd$birthdate);
    hfmd$onsetyear<-year(hfmd$hfmddate);
    hfmd$onsetmonth<-month(hfmd$hfmddate);
    hfmd$onsetday<-day(hfmd$hfmddate);
  }
  #②#####get "afterbirthday_01":the days difference between onset date and last birthday 
        #get "age":how many years of last birthday
        {
    hfmd$diseasebirthdate<-as.Date(ISOdate(hfmd$onsetyear,hfmd$birthmonth,hfmd$birthday))
    hfmd$diseasebirthdate_1<-as.Date(ISOdate(hfmd$onsetyear-1,hfmd$birthmonth,hfmd$birthday))
    hfmd$afterbirthday_01<-ifelse(as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate)>=0,
                                  as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate),
                                  as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate_1))
    hfmd$age<-ifelse(as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate)>=0,
                     as.numeric(hfmd$onsetyear-hfmd$birthyear),
                     as.numeric(hfmd$onsetyear-hfmd$birthyear-1))
  }
        #Special circumstances 1: 456 cases were born on February 29th of the leap year, but the onset of non -leap year
           hfmd[which(is.na(hfmd$afterbirthday_01)),]
          #Modify February 29th to February 28th, and then repeat the first step① 
           {a<-subset(hfmd,is.na(hfmd$afterbirthday_01))
            a$birthday<-gsub("29","28",a$birthday)
            b<-subset(hfmd,!(is.na(hfmd$afterbirthday_01)))
            hfmd<-rbind(a,b)
  }
           {
    hfmd$diseasebirthdate<-as.Date(ISOdate(hfmd$onsetyear,hfmd$birthmonth,hfmd$birthday))
    hfmd$diseasebirthdate_1<-as.Date(ISOdate(hfmd$onsetyear-1,hfmd$birthmonth,hfmd$birthday))
    hfmd$afterbirthday_01<-ifelse(as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate)>=0,
                                  as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate),
                                  as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate_1))
    hfmd$age<-ifelse(as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate)>=0,
                     as.numeric(hfmd$onsetyear-hfmd$birthyear),
                     as.numeric(hfmd$onsetyear-hfmd$birthyear-1))
  }
        #Special circumstances 2: 8265 cases' afterbirthDay_01 were 365,modifying them to 364 (365 indicated a non -leap year of birth, but a leap year of the onset, so 365 means that it was actually the one day before birthday)
         hfmd[which(hfmd$afterbirthday_01==365),]#8265,0.85%
         hfmd$afterbirthday<-ifelse(hfmd$afterbirthday_01==365,364,hfmd$afterbirthday_01)
  #③#####get ”agebydays“
  hfmd$aagebydays<-hfmd$afterbirthday+hfmd$age*365
}
############get age_01 and age_cate
{
hfmd$age_01<-ifelse(as.numeric(hfmd$hfmddate-hfmd$diseasebirthdate)>0,
                    as.numeric(hfmd$onsetyear-hfmd$birthyear),
                    as.numeric(hfmd$onsetyear-hfmd$birthyear-1))
hfmd$age_cate<-ifelse((hfmd$age_01==0|hfmd$age_01==-1),0,hfmd$age_01)
hfmd$age_cate<-ifelse((hfmd$age_01==1|hfmd$age_01==2),1,hfmd$age_cate)
hfmd$age_cate<-ifelse((hfmd$age_01==3|hfmd$age_01==4|hfmd$age_01==5|hfmd$age_01==6),2,hfmd$age_cate)
hfmd$age_cate<-ifelse((hfmd$age_01>=7),3,hfmd$age_cate)
}
####Table 1 Cumulative cases and BWPs across different age groups and sex 
{
#All cases(No.)
hfmd%>%
  group_by(age_cate)%>%
  summarise(num=n())
hfmd%>%
    group_by(age_cate,sex)%>%
    summarise(num=n())
#Cases during the birthday week(No.)
  hfmd359_0<-subset(hfmd,(afterbirthday==0|afterbirthday==364|afterbirthday==363|afterbirthday==362|afterbirthday==361|afterbirthday==360|afterbirthday==359))
  hfmd359_0%>%
    group_by(age_cate)%>%
    summarise(num=n())
  hfmd359_0%>%
    group_by(age_cate,sex)%>%
    summarise(num=n())
} 
###Supplementary Table S1 Cumulative cases proportion and times relative to the average daily proportion of HFMD in everyday of 365 days
{
  #2008~2022Cumulative cases
  S1<-hfmd%>%
    group_by(afterbirthday)%>%
    summarise(num=n())
  #every year
  ddd<-hfmd%>%
    group_by(afterbirthday,onsetyear)%>%
    summarise(num=n())
  ddd.cast2<-dcast(ddd,afterbirthday~onsetyear)
}
###Supplementary Table S2 Cumulative cases and BWPs of different age groups stratified by months of birth
{
  #All cases(No.)
  a<-hfmd%>%
    group_by(age_cate,birthmonth)%>%
    summarise(num=n())
  a.cast2<-dcast(a,age_cate~birthmonth)
  #Cases during the birthday week(No.)
  b<-hfmd359_0%>%
    group_by(age_cate,birthmonth)%>%
    summarise(num=n())
  b.cast2<-dcast(b,age_cate~birthmonth)
}
###Supplementary Table S3 Case numbers and BWPs of different age groups stratified by reporting years
{
#All cases(No.)
a<-hfmd%>%
  group_by(age_cate,onsetyear)%>%
  summarise(num=n())
a.cast2<-dcast(a,age_cate~onsetyear)
#Cases during the birthday week(No.)
b<-hfmd359_0%>%
    group_by(age_cate,onsetyear)%>%
    summarise(num=n())
b.cast2<-dcast(b,age_cate~onsetyear)
}