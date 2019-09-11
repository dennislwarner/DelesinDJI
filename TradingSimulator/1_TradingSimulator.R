
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
NEWRETRIEVE<-TRUE;
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
    df.D     <-   f.TradeSim(df.xts,v.signals,symb,cname,CASH);
    l.Trades[[length(l.Trades)+1]]<-df.D
}
df.AllD<-list_df2df(l.Trades)    
fnout<-"D:/Projects/DDJIOutput/AllD.csv";
write.csv(df.AllD,file=fnout);
df.dg<-df.AllD%>%dplyr::group_by(Date)
df.dgsummar<-df.dg%>%summarise(MV=sum(abs(CurValue)),RZD=sum(TotalRealized),SPandL=sum(Openprof))%>%dplyr::mutate(TotalVal=MV+RZD);
df.dgsummar.xts<-f.df2xts(df.dgsummar)
plot.xts(df.dgsummar.xts$TotalVal)
#--------------------------------------------------------------------------------------------
#-------from the detail data  frame df.D....create the trade records mimicing those from Tradestation
v.tickers<-sort(unique(df.AllD$Symbol))
iticker<-0;
l.Trades<-list();
while(iticker<length(v.tickers)){
    iticker<-iticker+1;
    symb<-v.tickers[iticker];
    thisdict<-df.DJDict%>%dplyr::filter(Symbol==symb);
    cname<-thisdict$Description;
    df.dd<-df.AllD%>%dplyr::filter(Symbol==symb)
    #df.Trades<-f.traderecstart(symb,cname)
    df.Trades<-f.initTrades()
    #find all the records in df.dd which translate to trades
    df.ddtrades<-df.dd%>%dplyr::filter(TQ!=0)
    
    it<-0;
    while(it<nrow(df.ddtrades)){
        it<-it+1;
        
        df.ddt<-df.ddtrades[it,];
        
        
        df.Trades[nrow(df.Trades)+1,]<-f.traderecstart(symb,cname);
        
        df.Trades$TradeInd[it]<-"T";
        df.Trades$ADP[it]<-"";
        df.Trades$Symbol[it]<-symb;
        df.Trades$Quantity[it]<-abs(df.ddt$TQ)
        df.Trades$ActivityDate[it]  <- df.ddt$Date;
        df.Trades$Type[it]<-ifelse(((df.ddt$StartQ<0)||(df.ddt$EndQ<0)),"Short","Margin");
        df.Trades$Transaction[it]<-ifelse((df.ddt$TQ>0),"Buy","Sell");
        df.Trades$price[it]         <- df.ddt$Open;
        df.Trades$Amount[it]        <- abs(df.ddt$TQ)*df.ddt$Open;
        df.Trades$Commission[it]    <- 5;
        id<-df.ddt$it
        rn<-row.names(df.ddt)
        id<-as.numeric(paste(rn,as.character(df.ddt$t),sep=""));
        df.Trades$Order_ID[it]      <- id
    }
    l.Trades[[symb]]<-df.Trades
   # return(df.trades );   #return to f.tradesim
}
df.AllTrades<-list_df2df(l.Trades)
fnout<-"D:/Projects/DDJIOutput/AllTrades.csv";
write.csv(df.AllTrades,file=fnout);
#--------------------------------------------------------------------------------------------
#-------from the detail data  frame df.D....create the position records mimicing those from Tradestation
df.Positions    <- f.derivePositions(df.AllTrades,l.Prices);
fnout           <- "D:/Projects/DDJIOutPut/AllPositions.csv";
write.csv(df.Positions,file=fnout);





fnout<-"D:/Projects/DDJIOutPut/AllPositions.csv";
df.Positions<-read.csv(fnout,stringsAsFactors = FALSE);
df.Pos<-df.Positions%>%dplyr::arrange(Date,symbol,X)
df.G<-df.Pos%>%group_by(Date)
df.GSummar<-df.G%>%summarise(TC=sum(TotalCost),MV=sum(MarketValue),SPandL=sum(OpenPandL));
#  build a large PDF output with tables and charts, by company
iticker<-0;
v.tickers<-sort(unique(df.Pos$symbol))
while (iticker<length(l.Tfades)){
    iticker<-iticker+1;
    symb<-v.tickers[[iticker]];
    df.G<-df.Pos%>%dplyr::filter(symbol==symb)
    mv.xts<-as.xts(df.G$MarketValue,order.by=as.Date(df.G$Date));
    plot.xts(mv.xts,main=symb)

}  