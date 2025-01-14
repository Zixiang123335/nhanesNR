R.Version4RUN<-343;
R.LibLocation <- "C:/Users/Administrator/AppData/Roaming/EmpowerRCH/R343/library"
#***************** Regarding ALL Following R Functions ********************
#***** COPYRIGHT (c) 2010 X&Y Solutions, ALL RIGHT RESERVED ***************
#******************* www.EmpowerStats.com *********************************
#**************************************************************************
Sys.setlocale("LC_TIME", "C")
library(doBy,lib.loc=R.LibLocation)
library(plotrix,lib.loc=R.LibLocation)
library(stringi,lib.loc=R.LibLocation)
library(stringr,lib.loc=R.LibLocation)
library(survival,lib.loc=R.LibLocation)
library(rms,lib.loc=R.LibLocation)
library(nnet,lib.loc=R.LibLocation)
library(car,lib.loc=R.LibLocation)
library(mgcv,lib.loc=R.LibLocation)
pdfwd<-6; pdfht<-6
setwd("D:/MyEmpowerstats/datastep/datastep")
load("D:/MyEmpowerstats/datastep/allde.Rdata")
if (length(which(ls()=="EmpowerStatsR"))==0) EmpowerStatsR<-get(ls()[1])
names(EmpowerStatsR)<-toupper(names(EmpowerStatsR))
#--#

slt.vname<-c()
