dirOneDrive         <-  "C:/Users/Denni/OneDrive";
dirProject          <-  paste(dirOneDrive,"/DelesinDJI/DDJI",sep="");
source(paste(dirProject, "/0_AllFunctions.R", sep = ""));
source(paste(dirProject, "/0_LoadLibs.R", sep = ""));
dirDocs             <-   paste(dirProject, "/Docs", sep = "");

source(paste(dirProject,"/0_Prep.R",sep=""));
dirData        <- "D:/WarkleighD/WarkleighData";
dirPriceData   <- paste(dirData,"/AdjOpens/Sectors",sep="");

#original data from MH

fnin         <- "DJ_OrigData.csv";
df.data      <- read_csv(fnin);
df.data.xts  <- f.dfall2xts(df.data);  # Get mike's original data in xts form
firstdate    <- index(first(df.data.xts));
lastdate     <- index(last(df.data.xts));
origspan     <- paste(firstdate,"/",lastdate,sep="");
df.odata.xts<-df.data.xts%>%f.xts2df()%>%dplyr::select(Date,DIA,X.INDU,Divisor)%>%f.df2xts();
#load the original weights
fnin<-"Docs/DJIWeights.csv";
df.weights<-read.csv(fnin)
v.changedates<- df.DJIAComposition$ChangeDates;
nchangedates <-length(v.changedates);
firstgood<-v.changedates[nchangedates-1];
lastgood <-v.changedates[nchangedates]-1;
goodspan<-paste(firstgood,"/",lastgood,sep="");

#Load all the component data
v.tickers<-names(df.DJIAComposition)[-1]

v.v<-v.tickers[v.tickers%in% c("DOW","AAPL"]
dt.CO<-dt.Common%>%dplyr::filter(ticker %in% v.tickers)%>%filter(ticker!="DOW")%>%filter(ticker!="AAPL"); #select the dictionary entries for the 30 DJI stocks
#load aapl to get date range

fnin<-paste(dirPriceData,"/Technology/AAPL.RDATA",sep="");
ss_dt.p<-load(fnin)
dt.p<-dt.p%>%dplyr::select(date,closeunadj)
dt.Prices.xts<-f.df2xtsD(dt.p,"date")
names(dt.Prices.xts)[1]<-"AAPL";
v.tickers<-dt.CO$ticker;
iticker<-0;
while(iticker<length(v.tickers)){
    iticker<-iticker+1;
    symb<-v.tickers[iticker];
    #if(!is.na(match(symb,dt.CO$ticker))){
        dt.coo<-dt.CO%>%filter(ticker==symbol)
        #cat(iticker,symb,dt.coo$ticker,"\n");
        fnin<-paste(dirPriceData,"/",dt.coo$sector,"/",dt.coo$ticker,".RDATA",sep="");
        ss_dt.p<-load(fnin)
        dt.p<-dt.p%>%dplyr::select(date,closeunadj)
        dt.p.xts<-f.df2xtsD(dt.p,"date")
        names(dt.p.xts)[1]<-symb;
        dt.Prices.xts<-merge.xts(dt.Prices.xts,dt.p.xts,join='left');
    #}
    #cat(iticker,symbol,match(ticker,dt.CO$ticker),"\n");
}
dt.X.xts<-merge.xts(dt.Prices.xts,df.odata.xts,join='left')[origspan];
fnout<-"dt.X.xts.RDATA";
save(dt.X.xts,file=fnout);
dt.X<-dt.X.xts%>%f.xts2df()%>%dplyr::select(AAPL, BA, CAT, HD, INTC,JNJ,KO,MCD,MMM,MSFT,PFE,PG,UNH,WMT,DIA,X.INDU,Divisor)
m.X<-as.matrix(dt.X[,1:14]);
m.R<-
mvweights<-df.weights[,1]
YMV<-m.X%*%df.weights$MV
#
#----All data is held in dt.X.xts
#step through each data span in the  titak data set


iticker<-0;
#Assemble the price data from ShaRadar----
while(iticker<nrow(dt.CO)){
    iticker<-iticker+1;
    ticker<-dt.CO$ticker[iticker]
    sectic<-paste(dt.CO$sector[iticker],"/",ticker,".RDATA",sep="");
    fnin<-paste(dirPriceData,"/",sectic,sep="");
    
    ss_dt.p<-load(fnin); #silently load the dt.p structure for this ticker
    dt.pp<-dt.p%>%dplyr::select(date,closeunadj)
    dt.pp.xts<-f.df2xtsD(dt.pp,"date")[span]
    v.x<-coredata(dt.pp.xts$closeunadj)
   
    if(iticker==1){
        m.djp<-matrix(0,length(v.x),30)
        m.djp[,1]<-v.x
    }else{
        m.djp[,iticker]<-v.x
    }
}
colnames(m.djp)<-v.tickers
df.djp.xts<-xts(m.djp,order.by=index(dt.pp.xts))
df.djp<-df.djp.xts%>%f.xts2df()
#----
#compute
#----get the supplementary data from original data set----
df.partial.xts    <- df.data.xts[,c("DIA","$INDU","Computed","Divisor")]
#add it to the ShaRadar component data
df.X.xts          <- merge.xts(df.djp.xts,df.partial.xts, join='left');   #merge, based on date
df.X              <- f.xts2df(df.X.xts)     #convert to a regular data.frame
#Begin computation
df.X$Sum30<-rowSums(df.X[,2:31])
df.X<-df.X%>%dplyr::mutate(DJIAhat=Sum30/Divisor,Error=X.INDU-DJIAhat)
summary(df.X)
