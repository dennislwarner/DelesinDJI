dirOneDrive         <-  "C:/Users/Denni/OneDrive";
dirProject          <-  paste(dirOneDrive,"/DelesinDJI/DDJI",sep="");
dirDDrive<-"D:/WarkleighD"       #Piasa Version
dirData<-"D:/WarkleighD/WarkleighData";
dirHoldings<-paste(dirDDrive,"/ETFHoldings",sep="");
dirPackages<-paste(dirDDrive,"/ETFPackages",sep="");
dirsupport<-paste(dirDDrive,"/ETFSupport",sep="");

source(paste(dirProject, "/0_AllFunctions.R", sep = ""));
source(paste(dirProject, "/0_EtfFunctions.R", sep = ""));
source(paste(dirProject, "/0_BuildDJI.R", sep = ""));
source(paste(dirProject, "/0_LoadLibs.R", sep = ""));
dirDocs             <-   paste(dirProject, "/Docs", sep = "");
source(paste(dirProject,"/0_Prep.R",sep=""));

#retrieve the State Street SPDR ES and their components
v.SectorETFS<-c("XLC","XLY","XLP","XLE","XLF","XLV","XLI","XLB","XLRE","XLSR","XLK","XLU")
v.BigETFs<-c("SPY","MDY","SLY","DIA")
v.IndustryETFs<-c("XITK","XNTK","XAR","KBE","XBI",
                "KCE","XHE","XHS","XHB","KIE",
                "XWEB","XME","XES","XOP","XPH",
                "KRE","XRT","XSD","XSW","XTH",
                "XTL","XTN");
df.shorts<-read.csv("Docs/Shorts.csv",stringsAsFactors = FALSE)
v.shorts<-names(df.shorts)
df.EODTickers<-read.csv("Docs/EODTicker.csv",stringsAsFactors = FALSE)

df.SectorETFs<-df.EODTickers%>%dplyr::filter(Ticker %in% v.SectorETFS)
df.BigETFs<-df.EODTickers%>%dplyr::filter(Ticker %in% v.BigETFs)
df.IndustryETFs<-df.EODTickers%>%dplyr::filter(Ticker %in% v.IndustryETFs)
df.ShortsETFs<-df.EODTickers%>%dplyr::filter(Ticker %in% v.shorts)
fnin<-paste(dirDocs,"/TargetDirectory.xlsx",sep="");
df.TargetDirectoryETFs<-read.xlsx(fnin,sheetIndex = 1)

#Begin analysis
targetETF<-"XHS";
v.etf<-df.SectorETFs%>%dplyr::filter(Ticker=="targetETF")
#df.ETFdict<-df.EODTickers%>%dplyr::filter(Ticker %in% v.)
#get holdings
fnin<-paste(dirHoldings,"/",targetETF,"_H.csv",sep="");
file.exists(fnin)
df.x<-read.csv(fnin)
names(df.x)
v.etfTickers<-c(targetETF,as.character(df.x$Identifier));
df.x<-f.getETFpackage(v.etfTickers )

fnout<-paste(dirPackages,"/",targetETF,"_P.csv",sep="");
write.csv(df.x,file=fnout);
v.holdings<-list.files(dirHoldings,include.dirs=FALSE)
tic();
ihold<-0;
while(ihold<length(v.holdings)){
    ihold<-ihold+1;
    hold<-v.holdings[ihold]
    if(hold=="Original"){next}
    hold<-str_sub(hold,start=1,end=-5);
    etf<-str_sub(hold,1,-3)
    fnin<-paste(dirHoldings,"/",hold,".csv",sep="");
    df.x<-read.csv(fnin);
    v.etfTickers<-c(etf,as.character(df.x$Identifier));
    etfCO<-df.EODTickers%>%dplyr::filter(Ticker==etf)
    df.CO<-df.EODTickers%>%dplyr::filter(Ticker %in% v.etfTickers)
    s<-setdiff(v.etfTickers,df.CO$Ticker)
    df.good<-df.CO%>%dplyr::filter(Ticker %in% v.etfTickers) ;
    v.goodSymbol<-df.good$Quandl_Code#[v.etfTickers %in% df.CO$Ticker]
    df.x.xts<-f.getETFpackage(v.goodSymbol)
    fnout<-paste(dirPackages,"/",etf,"_P.xts",sep="");
    save(df.x.xts,file=fnout)
    cat(ihold,etf,"---",etfCO$Name,"#Companies = ",nrow(df.CO)-1,"\n");
   
}

v.ETFs<-str_sub(v.holdings,1,-7)
df.ETFdict<-df.EODTickers%>%dplyr::filter(Ticker %in% v.ETFs);

#work on the retail

library(fPortfolio)
library(PerformanceAnalytics)
library(GA)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
library(psoptim)
library(GenSA)
library(foreach)
registerDoSEQ()

ietf<-12;
df.TargetDirectoryETFs$ETF<-as.character(df.TargetDirectoryETFs$ETF)
df.dictentry<-df.TargetDirectoryETFs[ietf,]
symb<-str_sub(df.dictentry$ETF,1,-3)
fnin<-paste(dirPackages,"/",symb,"_P.xts",sep="")
ss_df.x.xts<-load(fnin)

#ensure coloumns are in required order, where the target etf is the last column
df.x<-f.xts2df(df.x.xts)
v.target<-df.x[,symb]
df.xx<-df.x%>%dplyr::select(-symb)
df.xx<-data.frame(df.xx,v.target);names(df.xx)[ncol(df.xx)]<-symb
df.x.xts<-df.xx%>%f.df2xts()
M<-ncol(df.x.xts)


l.Res<-f.ProcessEstimates(df.x.xts,df.dictentry)


