dirProject<-getwd();
f.openDir<-function(first,second){
    fn<-paste(first,second,sep="")
    if(!dir.exists(fn)){
        erm<-paste("Directory ", fn," does not exists",sep="");
        stop(erm, call.=TRUE)
    }
    return(fn);
}

dirDocs<-f.openDir(dirProject,"/Docs");


dirD<-f.openDir("D:/Projects","/WarkleighD");
dirData<-f.openDir(dirD,"/WarkleighData");

dirResults<-f.openDir(dirD,"/TradingRes");
dirSupport<-f.openDir(dirD,"/TradingSupport");
dirAdjOpens<-f.openDir(dirData,"/adjOpens");

source(paste(dirProject, "/0_AllFunctions.R", sep = ""));
source(paste(dirProject, "/0_BuildDJI.R", sep = ""));
source(paste(dirProject, "/0_LoadLibs.R", sep = ""));
source(paste(dirProject, "/0_f.TradeSim.R",sep=""))
source(paste(dirProject, "/0_SimTradingFunctions.R",sep=""))
dirDocs             <-   paste(dirProject, "/Docs", sep = "");
source(paste(dirProject,"/0_Prep.R",sep=""));
#library(DMwR2)
NEWRETRIEVE<-FALSE;
#-----------------------------------------------------------------------
#   use thd DJ components as the sample data set
if(NEWRETRIEVE){
    l.Prices<-f.getPricesA(v.tickers);
    save(l.Prices,file="Prices.RDATA")
}else{
    s_l.Prices<-load("Prices.RDATA");
}

iticker<-0
while(iticker<length(l.Prices)){
    iticker<-iticker+1;
    symb<-v.tickers[[iticker]];
    #description-v.descriptions[[iticker]]
    cname<-as.character(df.DJDict[iticker,"Description"])
    df.xts<-l.Prices[[symb]]
    plot.xts(df.xts[,1:4],main=cname)
    #v.p.xts<-df.p.xts[,symb]
    #retrieve the price data for this symbol
    CASH<-100000;
    dft<-f.searchTrades(df.xts,symb,cname,CASH)
    cat(iticker,symb,"\n");
}





#












# # establish th eprice calendar with SPY retrieval
# df.SPY.xts<-f.getSPY()%>%f.df2xts()
# BUILD<-FALSE;
# if(BUILD){
#     l.X       <- f.buildDJIdata()
#     fnout<-"l.X.RDATA"
#     save(l.X,file=fnout)
# }else{
#     fnin<-"l.X.RDATA";
#     ss_l.X<-load(fnin);   #silently load dt.X.xts modelling data
# }

#Begin with the first changedatam, using the 3 prior years of data, form an optimal portfolio
#then update in 1 quarter increments,  and track moving portfolio
#note , use adjusted prices for the portfolio optimization
v.changedates<-l.X$v.changedates;
thisdate<-v.changedates[1];

v.DIA.xts<-dt.X.xts$DIA;   #extract actual DIA etf

#load the original weights--------------------
fnin<-"Docs/DJIWeights.csv";
df.weights<-read.csv(fnin)
#select the price series actually used by the optimization, the Spider for the DJIA, and the divisor
dt.X<-dt.X.xts%>%f.xts2df()%>%dplyr::select(AAPL, BA, CAT, HD, INTC,JNJ,KO,MCD,MMM,MSFT,PFE,PG,UNH,WMT,DIA,INDU,Divisor)
v.dates<-index(dt.X.xts)
m.X<-as.matrix(dt.X[,1:14]);
m.weights<-as.matrix(df.weights[,-1])/100.0;
m.money<-1000000*m.weights;   #money invested in each of 14 stocks, in each of 3 allocation sets
v.pinit<-as.numeric(dt.X[1,1:14]);
istock<-0;
m.shares<-m.money*0;
while(istock<14){
    istock<-istock+1;
    imethod<-0;
    while(imethod<3){
        imethod<-imethod+1;
        m.shares[istock,imethod]<-m.money[istock,imethod]/v.pinit[istock]
    }
 }
m.wealth<-m.X%*%m.shares
df.indices<-data.frame(m.wealth*initDIA/1000000,DIA=dt.X$DIA);
df.indices2<-df.indices%>%dplyr::mutate(MVspread=MV-DIA,SVspread=SV-DIA,VARspread=VAR-DIA)
dfindices3<-data.frame(Date=v.dates,df.indices2)
df.Indicies.xts<-f.df2xts(dfindicies3)
for(icol in c(2:ncol(df.Indicies.xts))){
    df.Indicies.xts[,icol]<-round(df.Indicies.xts[,icol],digits=2)
}

write.xlsx2(df.Indicies.xts,"df.Indices.xlsx")
somePDFPath = "DIAConstructions.pdf"
pdf(file=somePDFPath)  
plot.xts(df.Indicies.xts,main="DIA constructions",legend.loc = 'left');
dev.off()


for (i in 2:nrow(df)) 
{
    if (as.vector(df$Date[i])  ==  as.vector(df$Date[i-1])) 
    {EqDatedf <- rbind(EqDatedf, df[i,])}
    
    else {
        EqDatedf <- rbind(EqDatedf, EmptyLine)
        EqDatedf <- rbind(EqDatedf, df[i,]) 
    }
}

grid.table(EqDatedf, show.rownames = FALSE)
dev.off()

for (i in seq(5,10))   
    
{   
    par(mfrow = c(2,1))
    VAR1=rnorm(i)  
    VAR2=rnorm(i)  
    plot(VAR1,VAR2)   
} 
dev.off() 





nunits<-1000000/dt.X$DIA[1]
v.diawealth<-dt.X$DIA*nunits;

plot.ts(v.diawealth)

df.wealth<-data.frame(Date=v.dates,m.wealth,v.diawealth);
names(df.wealth)<-c("Date","MV","SV","VAR","DIA");
dt.wealth.xts<-f.df2xts(df.wealth)
plot.xts(dt.wealth.xts);
fnout<-"dt.Wealth.csv";
initDIA<-dt.X$DIA[1]
dt.wealth<-f.xts2df(dt.wealth.xts)#%>%dplyr::mutate(MVgain=MV-DIA,SVgain=SV-DIA,VARgain=VAR-DIA,
                                                   pMVgain=initDIA*MVgain/1000000,pSVgain=initDIA*SVgain/1000000,
                                                   pVARgain=initDIA*VARgain/1000000);
dt.Indices<-dt.wealth%>%
    dplyr::mutate(diaMV=initDIA*MV/1000000,diaSV=initDIA*SV/1000000,diaVAR=initDIA*VAR/1000000)





v.months<-endpoints(dt.wealth.xts,on="months")
dt.wealthMon.xts<-dt.wealth.xts[v.months,]
dt.wealth.xts<-f.df2xts(dt.wealth)
plot.xts(dt.wealth.xts$pMVgain)# 
write_csv(dt.wealth,path=fnout)
# m.shares[,1]<-m.money[,1]/v.pinit
# m.shares[,2]<-t(m.money[,2]/v.pinit)
# v.shares*v.pinit
# p.prices<-
# m.shares<-
# m.R<-m.X* m.weights
# 
# 
# 
# mvweights<-df.weights[,1];
# YMV<-m.X%*%df.weights$MV
# #simulate owning the DJIA or owning the stocks with weights.
# #to avoid confusion  start with $1000000, and fractional investment
# m.shares<-matrix(0,30,2)
# #start Date
# # option 1 invest in
# dim(dt.X)   #the data set has 2515 days
# v.DIA<-dt.X$DIA
# 
# 
# v.proportion<-0.01*df.weights$MV;
# v.money<-v.proportion*1000000;
# v.shares<-0*v.money;
# for(i in c(1:14)){
#     cat(i,"\n");
#     v.shares[i]<-f.safeDiv(v.money[i],m.X[1,i])
# }
# m.X[1,]%*%v.shares
# v.delwealth<-m.X%*%v.shares
# 
# nDIA<-1000000/v.DIA[1]
# v.diawealth<-nDIA*v.DIA
# v.shares
# plot.ts(dt.X$DIA)
# 
# v.shares[is.na(v.shares)]<-0;
# 
# df.investments<-data.frame(v.diawealth,v.delwealth)%>%dplyr::mutate(dif=v.delwealth-v.diawealth,rDIA=f.roc(v.diawealth),rDEL=f.roc(v.delwealth),rdif=rDEL-rDIA)
# 
# df.DJIAComposition<-read.xlsx(paste(dirDocs,"/DJIAComposition.xlsx",sep=""),sheetName="DJIAComposition");
# v.changedates<- df.DJIAComposition$ChangeDates;
# nchangedates <-length(v.changedates);
# firstgood<-v.changedates[nchangedates-1];
# lastgood <-v.changedates[nchangedates]-1;
# goodspan<-paste(firstgood,"/",lastgood,sep="");
# #build f the array of prices
# thisspan=
#     
#     numspans<-nrow(df.DJIAComposition)-1;
#  ispan<-0;
#  while(ispan<numspans){
#      ispan<-ispan+1;
#      firstgood<-v.changedates[ispan];
#      lastgood<-v.changedats[ispan+1];
#      goodspan<-paste(firstgood,"/",lastgood,sep="");
#      v.stocks<-df.DJIAComposition[ispan,-1]
#      
#  }
#     
#     
# portfolio_returns = function(x) {
#     port.returns = 0
#     
#     # Multiplication of the i-th asset by the i-th weight in "x"
#     for (i in 1:length(x)) {
#         port.returns = port.returns + asset_returns[,i] * x[i]
#     }
#     
#     return (port.returns)
# }
# 
# 
# library("GA")
# ga_res = ga(
#     # Tell the genetic algorithm that the 
#     # weights are real variables
#     type="real-valued", 
#     
#     # "ga" function performs maximization, so we must
#     # multiply the objective function by -1
#     function(x){-obj(x)}, 
#     
#     # x_i >= 0
#     lower = rep(0,ncol(asset_returns)), 
#     
#     # x_i <= 1
#     upper = rep(1,ncol(asset_returns)), 
#     
#     # Maximum number of iterations 
#     maxiter = 50000, 
#     
#     # If the maximum fitness remains the same for 50
#     # consecutive transactions, stop the algorithm
#     run=50, 
#     
#     # Exploit multi-core properties of your CPU
#     parallel=TRUE,
#     
#     # We want to see the partial results of the process
#     # while it performs
#     monitor=TRUE,
#     
#     # Seed useful for replicating the results
#     seed=1
# )
# # 
# # 
# # v.wealth<-m.X%*%v.shares
# # 
# # v.minvest<-df.weights$MV*
# # 
# # #
# # #----All data is held in dt.X.xts
# # #step through each data span in the  titak data set
# # f.ror<-function(x){
# #     r<-x/lag(x)
# # }
# # 
# # iticker<-0;
# # #Assemble the price data from ShaRadar----
# # while(iticker<nrow(dt.CO)){
# #     iticker<-iticker+1;
# #     ticker<-dt.CO$ticker[iticker]
# #     sectic<-paste(dt.CO$sector[iticker],"/",ticker,".RDATA",sep="");
# #     fnin<-paste(dirPriceData,"/",sectic,sep="");
# #     
# #     ss_dt.p<-load(fnin); #silently load the dt.p structure for this ticker
# #     dt.pp<-dt.p%>%dplyr::select(date,closeunadj)
# #     dt.pp.xts<-f.df2xtsD(dt.pp,"date")[span]
# #     v.x<-coredata(dt.pp.xts$closeunadj)
# #    
# #     if(iticker==1){
# #         m.djp<-matrix(0,length(v.x),30)
# #         m.djp[,1]<-v.x
# #     }else{
# #         m.djp[,iticker]<-v.x
# #     }
# # }
# # colnames(m.djp)<-v.tickers
# # df.djp.xts<-xts(m.djp,order.by=index(dt.pp.xts))
# # df.djp<-df.djp.xts%>%f.xts2df()
# # #----
# # #compute
# # #----get the supplementary data from original data set----
# # df.partial.xts    <- df.data.xts[,c("DIA","$INDU","Computed","Divisor")]
# # #add it to the ShaRadar component data
# # df.X.xts          <- merge.xts(df.djp.xts,df.partial.xts, join='left');   #merge, based on date
# # df.X              <- f.xts2df(df.X.xts)     #convert to a regular data.frame
# # #Begin computation
# # df.X$Sum30<-rowSums(df.X[,2:31])
# # df.X<-df.X%>%dplyr::mutate(DJIAhat=Sum30/Divisor,Error=X.INDU-DJIAhat)
# # summary(df.X)
