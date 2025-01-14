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
setwd("D:/MyEmpowerstats/datastep")
load("D:/MyEmpowerstats/albumin.Rdata")
if (length(which(ls()=="EmpowerStatsR"))==0) EmpowerStatsR<-get(ls()[1])
names(EmpowerStatsR)<-toupper(names(EmpowerStatsR))
#--#

slt.vname<-c()

library(foreign,lib.loc=R.LibLocation)
library(Hmisc,lib.loc=R.LibLocation)
 
ofname<-"allde"; 
othdata<-c('D:/PROGRAM FILES/ANALYSIS/DATAPROJECT/NPAR/RAWDATA/0304DE.XPT', 'D:/PROGRAM FILES/ANALYSIS/DATAPROJECT/NPAR/RAWDATA/0506DE.XPT', 'D:/PROGRAM FILES/ANALYSIS/DATAPROJECT/NPAR/RAWDATA/0708DE.XPT', 'D:/PROGRAM FILES/ANALYSIS/DATAPROJECT/NPAR/RAWDATA/0910DE.XPT', 'D:/PROGRAM FILES/ANALYSIS/DATAPROJECT/NPAR/RAWDATA/1112DE.XPT', 'D:/PROGRAM FILES/ANALYSIS/DATAPROJECT/NPAR/RAWDATA/1314DE.XPT', 'D:/PROGRAM FILES/ANALYSIS/DATAPROJECT/NPAR/RAWDATA/1516DE.XPT', 'D:/PROGRAM FILES/ANALYSIS/DATAPROJECT/NPAR/RAWDATA/1718DE.XPT'); 
idvar<-''; 
set<-'F'; 
idcompress<-0; 
ofname<-'allde'; 
ssdfmt<-c('XPT', 'XPT', 'XPT', 'XPT', 'XPT', 'XPT', 'XPT', 'XPT'); 
##R package## foreign Hmisc ##R package##;
printTxt<-function(txt,cname,rname) {
  tmp<-as.matrix(txt,ncol=1)
  rname1<-rname
  if (length(rname1)==1) rname1=rep(rname,times=nrow(tmp))
  colnames(tmp)=cname
  rownames(tmp)=rname1
 print(tmp,quote=F)
}

df.append<-function(dflist, matchname) { 
  ndf = length(dflist) 
  df.cname.1 <- names(dflist[[1]]) 
  df.cname.t <- df.cname.1
  for (i in (2:ndf)) { 
    df.cname.2 <- names(dflist[[i]]) 
    df.cname.1 <- df.cname.2[match(df.cname.1,df.cname.2)] 
    df.cname.1 <- df.cname.1[!is.na(df.cname.1)] 
    df.cname.t <- c(df.cname.t,df.cname.2)
  }    
  df.cname.t <- unique(df.cname.t)
  ncommonvar = length(df.cname.1) 
 if (matchname=="T") {
   if (ncommonvar>0) { 
     tmp.rr <- dflist[[1]][,df.cname.1] 
     if (ncommonvar==1) { 
       tmp.rr<-as.data.frame(tmp.rr) 
       names(tmp.rr)<-df.cname.1 
     } 
     for (i in (2:ndf)) { 
      tmp.rr2 <- dflist[[i]][,df.cname.1] 
      if (ncommonvar==1) { 
         tmp.rr2<-as.data.frame(tmp.rr2) 
         names(tmp.rr2)<-df.cname.1 
      } 
      tmp.rr <- rbind(tmp.rr,tmp.rr2) 
     }  
     return (tmp.rr) 
   } else { 
     return (NA) 
   } 
 } else {
   for (i in (1:ndf)) {
     miss.name1 <- df.cname.t[is.na(match(df.cname.t,names(dflist[[i]])))]
     miss.df1 <- matrix(NA,ncol=length(miss.name1),nrow=nrow(dflist[[i]]))
     colnames(miss.df1) <- miss.name1
     tmp.df1 <- data.frame(dflist[[i]],miss.df1)
     if (i==1) {
       df.all <- tmp.df1
     } else {
       df.all <- rbind(df.all, tmp.df1)
     }
     rm(miss.name1, miss.df1, tmp.df1)
   }
   return (df.all)
 }
} 

readEPD<-function(inFile, fmt, skips=0) {
  tmp <- unlist(strsplit(fmt," "))
  if (length(tmp)==1) tmp <- unlist(strsplit(fmt,"\\."))
  fmt <- tmp[1]
  if (length(tmp)>1) skips = tmp[2]
  if (fmt=="EPD") {
    nvar<-read.table(inFile, nrows=1); skiplines<-as.numeric(nvar+1);
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep="\t", skip= skiplines, nrows=-1, blank.lines.skip=TRUE, fill=TRUE)
  }
  if (fmt=="CSV") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep=",", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="")
  }
  if (fmt=="CSV1") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep=",", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="\'")
  }
  if (fmt=="CSV2") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep=",", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="\"")
  }
  if (fmt=="TXT") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep=" ", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="")
  }
  if (fmt=="TXT1") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep=" ", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="\'")
  }
  if (fmt=="TXT2") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep=" ", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="\"")
  }
  if (fmt=="TAB") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep="\t", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="")
  }
  if (fmt=="TAB1") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep="\t", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="\'")
  }
  if (fmt=="TAB2") {
    d<-read.table(inFile, header=TRUE, as.is=TRUE, na.string=c("NA",".",""," "), comment.char="",
    sep="\t", nrows=-1, skip=skips, blank.lines.skip=TRUE, fill=TRUE, quote="\"")
  }
  if (fmt=="RDATA") {load(inFile); d<-get(ls());}
  if (fmt=="SAS7BDAT") {
     source("http://www.empowerstats.com/empowerStats/R.functions/_read.sas7bdat.R")
     d<-read.sas7bdat(inFile)
  }
  if (fmt=="XPT") {d<-sasxport.get(inFile);}
  if (fmt=="SAV") {
     d<-read.spss(inFile,to.data.frame = TRUE,use.value.labels = FALSE)
  }     
  d<-d[(apply(is.na(d),1,sum)<ncol(d)),]
  d<-d[,(apply(is.na(d),2,sum)<nrow(d))]
  colnames(d)<-toupper(colnames(d))       
  return(d)
}

ndfile<-length(othdata);
if (idcompress==1 && idvar>"") EmpowerStatsR[,idvar]<-gsub(" *$","",EmpowerStatsR[,idvar])
dflist<-list(EmpowerStatsR);
ssdfmtx<-ssdfmt[1]
if (length(ssdfmt)>1) {
 for (i in 2:length(ssdfmt)) {
  if (!is.na(as.numeric(ssdfmt[i]))) {
    ntmp<-length(ssdfmtx)
    ssdfmtx[ntmp]<-paste(ssdfmtx[ntmp],ssdfmt[i],sep=".")
  } else {ssdfmtx<-c(ssdfmtx,ssdfmt[i]);}  
 }
}
mx.desp<-c("Nobs","Variables")
dtname.oth<-c("")
for (i in 1:ndfile) {
  tmpd<-readEPD(othdata[i], ssdfmtx[i])
  if (idcompress==1 && idvar>"") tmpd[,idvar]<-gsub(" *$","",tmpd[,idvar])
  dflist[[i+1]]<-tmpd
  mx.desp<-rbind(mx.desp,c(nrow(tmpd),paste(colnames(tmpd),collapse=" ")));
  dtname.oth<-c(dtname.oth,tail(unlist(strsplit(othdata[i],"\\/")),n=1))
}
colnames(mx.desp)<-mx.desp[1,]
mx.desp<-mx.desp[-1,]
if (!is.matrix(mx.desp)) mx.desp <- matrix(mx.desp,nrow=1)
rownames(mx.desp)<-dtname.oth[-1]
WD1<-df.append(dflist,set)
sink(paste(ofname,".txt",sep=""))
write.table(WD1,file=paste(ofname,".xls",sep=""),append=FALSE,quote=FALSE,
 sep='\t',row.names=FALSE,col.names=TRUE)

printTxt(paste("Append data files (top to bottom) and save to: ",ofname,".xls success",sep=""),"","")
printTxt("Number of records in original data:","","")
nrow(EmpowerStatsR)
printTxt("Variables in original data:","","")
names(EmpowerStatsR)
printTxt("Data files appended:","","")
print(othdata)
print(mx.desp)
printTxt("Number of records in new data:","","")
nrow(WD1)
printTxt("Variables in new data:","","")
names(WD1)
sink()


