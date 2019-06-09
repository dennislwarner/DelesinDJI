#Start Me Up--------------------------------------------
dirOneDrive         <-  "D:/OneDrive";
dirProject          <-  paste(dirOneDrive,"/DelesinDJI/DDJI",sep="");
source("00_SPDRsStartup.R",echo=FALSE);
REBUILD<-FALSE;
if(REBUILD){
    source("00_BuildHoldingsFiles.R",echo=FALSE);
}

#----
#Begin analysis
----
#Load Libraries----

#work on the portfolio trackinig

library(fPortfolio);
suppressMessages(library(PerformanceAnalytics));
suppressMessages(library(GA));
suppressMessages(library(PortfolioAnalytics));
suppressMessages(library(DEoptim));

*suppressMessages(library(ROI));
suppressMessages(require(ROI.plugin.glpk));
suppressMessages(require(ROI.plugin.quadprog));
suppressMessages(library(psoptim));
suppressMessages(library(GenSA));
suppressMessages(library(foreach));
suppressMessages(library(iterators));
suppressMessages(library(tidyquant));
registerDoSEQ()
source("0_fPortfolioFunctions.R",echo=FALSE);
 #----
#------------------------------------------------------------------------------|
#   v.holdings   names of files of holdings  e.g. "DIA_H.csv"
#   v.ETFs       Eod stock symbols for the coresponding ETF e.g. "DIA"
#   df.ETFdict   data frame with (Ticker, Quandl_Code Name Exchange LTDate etc)
#   df.dictentry df row for a particular ETF
#   symb         ticker symbol for a particular etf  , an element of v.ETFs
#   df.x.xts     data frame of prices of components with price of ETF in last column
#   l.RES        list of results returned from the estimation routine
#
#-----------------------------------------------------------------------------|


v.holdings    <- list.files(dirHoldings,include.dirs=FALSE);
v.ETFs        <- str_sub(v.holdings,1,-7)
# 0
# <- df.EODTickers%>%dplyr::filter(Ticker %in% v.ETFs);
ietf<-4;
ietf<-ietf+1;
df.TargetDirectoryETFs$ETF<-as.character(df.TargetDirectoryETFs$ETF)
df.dictentry<-df.TargetDirectoryETFs[ietf,]
symb<-str_sub(df.dictentry$ETF,1,-3)
fnin<-paste(dirPackages,"/",symb,"_P.xts",sep="")
ss_df.x.xts<-load(fnin)
names(df.x.xts)<-str_sub(names(df.x.xts),5,); #correct this in the 00_BuildHoldingsFiles.R file
#lets look at every target ETF data series
ietf<-0;
v.symbs<-df.TargetDirectoryETFs$ETF;
while(ietf<38){
    ietf<-ietf+1;
    symb<-str_sub(v.symbs[ietf],1,-3)
    fnin<-paste(dirPackages,"/",symb,"_P.xts",sep="")
    ss_df.x.xts<-load(fnin)
    
    
    #if(ietf==21){df.x.xts<-df.x.xts[,-10]}
    #ensure coloumns are in required order, where the target etf is the last column
    #  Should move to the the 00_BuildHoldingsFiles.R above----
    v.target<-df.x.xts[,symb]
    firsttarget<-min(which(is.finite(v.target)))
    firsttargetdate<-index(df.x.xts)[firsttarget]
    targetdesc<-df.TargetDirectoryETFs$Description[ietf]
   
   # df.x.xts<-df.xx%>%f.df2xts()
    M<-ncol(df.x.xts)
    #----
    cat(ietf,df.TargetDirectoryETFs$ETF[ietf],df.TargetDirectoryETFs$Description[ietf],"\n");
    targetname<-paste(ietf," ",symb," - ",targetdesc,sep="")
    if(ietf>=36){
       df.x.xts["/2015-09-15",M]<-NA
    }
    plot.xts(df.x.xts[,M],main=targetname)
}








#l.Res<-f.ProcessEstimates(df.x.xts,df.dictentry)


#l.Res<-f.fPortfolioEstimates(df.x.xts,df.dictentry);
trainyears<-6;
testmonths<-12;
l.Res<-f.fPortFullSim([ietf], df.x.xts,df.dictentry,trainyears,testmonths);
plot.xts(l.Res$df.fore.xts[l.Res$v.goodobs,],legend.loc='topleft',main=l.Res$chartmain)



