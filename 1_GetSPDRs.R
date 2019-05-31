#Start Me Up--------------------------------------------
dirOneDrive         <-  "C:/Users/Denni/OneDrive";
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

suppressMessages(library(fPortfolio));
suppressMessages(library(PerformanceAnalytics));
suppressMessages(library(GA));
suppressMessages(library(PortfolioAnalytics));
suppressMessages(library(DEoptim));
suppressMessages(library(ROI));
suppressMessages(require(ROI.plugin.glpk));
suppressMessages(require(ROI.plugin.quadprog));
suppressMessages(library(psoptim));
suppressMessages(library(GenSA));
suppressMessages(library(foreach));
suppressMessages(library(iterators));
suppressMessages(library(tidyquant));
registerDoSEQ()
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
0
<- df.EODTickers%>%dplyr::filter(Ticker %in% v.ETFs);
ietf<-21;
df.TargetDirectoryETFs$ETF<-as.character(df.TargetDirectoryETFs$ETF)
df.dictentry<-df.TargetDirectoryETFs[ietf,]
symb<-str_sub(df.dictentry$ETF,1,-3)
fnin<-paste(dirPackages,"/",symb,"_P.xts",sep="")
ss_df.x.xts<-load(fnin)
names(df.x.xts)<-str_sub(names(df.x.xts),5,); #correct this in the 00_BuildHoldingsFiles.R file
if(ietf==21){df.x.xts<-df.x.xts[,-10]}
#ensure coloumns are in required order, where the target etf is the last column
#  Should move to the the 00_BuildHoldingsFiles.R above----
df.x<-f.xts2df(df.x.xts)
v.target<-df.x[,symb]
df.xx<-df.x%>%dplyr::select(-symb)
df.xx<-data.frame(df.xx,v.target);names(df.xx)[ncol(df.xx)]<-symb
df.x.xts<-df.xx%>%f.df2xts()
M<-ncol(df.x.xts)
#----

#l.Res<-f.ProcessEstimates(df.x.xts,df.dictentry)

source("0_fPortfolioFunctions.R",echo=FALSE);
l.Res<-f.fPortfolioEstimates(df.x.xts,df.dictentry);





