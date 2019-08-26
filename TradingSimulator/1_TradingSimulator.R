
setwd("D:/Projects/DDJI/TradingSimulator");
dirProject<-getwd();

f.openDir<-function(first,second){
    fn<-paste(first,second,sep="")
    if(!dir.exists(fn)){
        erm<-paste("Directory ", fn," does not exists",sep="");
        stop(erm, call.=TRUE)
    }
    return(fn);
}
dirD<-f.openDir("D:/Projects","/WarkleighD");
dirData<-f.openDir(dirD,"/WarkleighData");
dirResults<-f.openDir(dirD,"/TradingRes");
dirSupport<-f.openDir(dirD,"/TradingSupport");
dirAdjOpens<-f.openDir(dirData,"/adjOpens");
dirDDJI<-f.openDir("D:/Projects","/DDJI");
dirDocs<-f.openDir(dirDDJI,"/Docs");



#----Script R files used
l.ScriptFilesUsed<-list();
l.LibsNeeded<-list();
#-----declare files to be used
l.ScriptFilesUsed[[1]]<-paste(dirDDJI, "/0_AllFunctions.R", sep = "");
l.ScriptFilesUsed[[2]]<-paste(dirDDJI, "/0_BuildDJI.R", sep = "")
#l.ScriptFilesUsed[[3]]<-paste(dirProject, "/0_f.TradeSim.R",sep="")
l.ScriptFilesUsed[[3]]<-paste(dirProject, "/0_tradeFunctions.R",sep="")
l.ScriptFilesUsed[[4]]<-paste(dirDDJI,"/0_Prep.R",sep="");
#l.ScriptFilesUsed[[6]]<-paste(dirProject,"/0_GroupTrading.R",sep="")
#-----find the packages required for the above source files
Nfiles<-length(l.ScriptFilesUsed)
isource<-0;
library(NCmisc)
l.libs<-list();
while(isource<Nfiles){
    isource<-isource+1;
    l.LibsNeeded[[isource]]<-list.functions.in.file(l.ScriptFilesUsed[[isource]])
    #source(l.ScriptFilesUsed[[isource]]);
    l.libs<-c(l.libs,l.LibsNeeded[[isource]]);
}
#----load libraries
library(stats);
library(readr);
library(magrittr);
library(qdapTools);
suppressMessages(library(xts));
suppressMessages(library(PerformanceAnalytics));
library(stringr);
library(Quandl);
suppressMessages(library(tidyquant));
#-----Run source files
isource<-0;
while(isource<Nfiles){
    isource<-isource+1;
    #l.LibsNeeded[[isource]]<-list.functions.in.file(l.ScriptFilesUsed[[isource]])
    cat(l.ScriptFilesUsed[[isource]],"\n");
    source(l.ScriptFilesUsed[[isource]]);
    #l.libs<-c(l.libs,l.LibsNeeded[[isource]]);
}

#library(DMwR2)
# lOAD THE PRICES FOR THE DIA etf---
NEWRETRIEVE<-FALSE;
NEWGENERATE<-TRUE;

#-----------------------------------------------------------------------
#   use thd DJ components as the sample data set
if(NEWRETRIEVE){
    l.Prices<-f.getPricesA(v.tickers);
    save(l.Prices,file="Prices.RDATA")
    l.etf.xts<-f.getPricesA("DIA");
    df.etf.xts<-l.etf.xts[[1]]
    save(df.etf.xts,file="DIAETF.RDATA");
}else{
    s_l.Prices<-load("Prices.RDATA");
    s_df.etf.xts<-load("DIAETF.RDATA")
}
#----Initialize counters and parameters----
totaltrades<-0;
totalprofits<-0;
iticker<-0

ub<-24
lb<-0;
horizon<-20;
#----Set intensity table
v.q<-stats::quantile(l.Prices[[1]], probs = seq(0, 1, 0.1))

#----
#  Prepare the group Ror tables
#specify a matrix of ub,lb pairs----
# specify the window lengths to be scanned
v.windows <- c(5,10,20,65,130,261,522);
#v.windows<-c(65)
#f.GroupTrade<-function(l.Prices,horizon,ub,lb)
fnout        <-"positions.RDATA";
if(NEWGENERATE){
    m.pos    <- f.DistinctTrade(l.Prices,df.etf.xts,v.windows);
    #plot.xts(df.TotalProf.xts)
    #save the algorithmically generated daily trading weights
    
    save(m.pos,file=fnout)
}
ss_m.pos <- load(fnout)
v.tickers    <- df.DJDict$Symbol;
iticker      <- 0;
l.Trades<-list();
while(iticker<length(l.Prices)){
    iticker     <- iticker+1;
    symb        <- v.tickers[[iticker]];
    #description-v.descriptions[[iticker]]
    thisdict<-df.DJDict%>%dplyr::filter(Symbol==symb)
    cname       <- as.character(thisdict$Description)
    cat(iticker,symb,cname,"\n");
   
    df.xts        <-l.Prices[[symb]]
    if(is.null(df.xts)){next}
    plot.xts(df.xts[,1:4],main=cname)
    #v.p.xts<-df.p.xts[,symb]
    #retrieve the price data for this symbol
    CASH          <- 100000;
    v.signals     <- (m.pos[,iticker]);
    nt<-sum(abs(diff(v.signals)))
if(length(v.signals)!=nrow(df.xts)){next}
    df.trades     <-   f.TradeSim(df.xts,v.signals,symb,cname,CASH);
    l.Trades[[length(l.Trades)+1]]<-df.trades
}
df.AllTrades<-list_df2df(l.Trades)    
fnout<-"D:/Projects/DDJIOutput/AllSimTrade.csv";
write.csv(df.AllTrades,file=fnout);

df.Positions<-f.derivePositions(df.AllTrades,l.Prices);
fnout<-"D:/Projects/DDJIOutPut/AllPositions.csv";
write.csv(df.Positions,file=fnout);

