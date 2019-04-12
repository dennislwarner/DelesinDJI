dirOneDrive         <-  "C:/Users/Denni/OneDrive";
dirProject          <-  paste(dirOneDrive,"/DelesinDJI/DDJI",sep="");
source(paste(dirProject, "/0_AllFunctions.R", sep = ""));
source(paste(dirProject, "/0_LoadLibs.R", sep = ""));
dirDocs             <-   paste(dirProject, "/Docs", sep = "");
source(paste(dirProject,"/0_Prep.R",sep=""));
dirData        <- "D:/WarkleighD/WarkleighData";

BUILD<-FALSE;
if(BUILD){
    df.X.xts       <- f.buildDJIdata()
}else{
    fnin<-"dt.X.xts.RDATA";
    ss_dt.X.xts<-load(fnin);   #silently load dt.X.xts modelling data
}


#load the original weights--------------------
fnin<-"Docs/DJIWeights.csv";
df.weights<-read.csv(fnin)
#----must convert prices to log-relative ror 
dt.X<-dt.X.xts%>%f.xts2df()%>%dplyr::select(AAPL, BA, CAT, HD, INTC,JNJ,KO,MCD,MMM,MSFT,PFE,PG,UNH,WMT,DIA,INDU,Divisor)
m.X<-as.matrix(dt.X[,1:14]);

m.R<-
mvweights<-df.weights[,1]
YMV<-m.X%*%df.weights$MV
#simulate owning the DJIA or owning the stocks with weights.
#to avoid confusion  start with $1000000, and fractional investment
m.shares<-matrix(0,30,2)
#start Date
# option 1 invest in
dim(dt.X)   #the data set has 2515 days
v.DIA<-dt.X$DIA


v.proportion<-0.01*df.weights$MV;
v.money<-v.proportion*1000000;
v.shares<-0*v.money;
for(i in c(1:14)){
    cat(i,"\n");
    v.shares[i]<-f.safeDiv(v.money[i],m.X[1,i])
}
m.X[1,]%*%v.shares
v.delwealth<-m.X%*%v.shares

nDIA<-1000000/v.DIA[1]
v.diawealth<-nDIA*v.DIA
v.shares
plot.ts(dt.X$DIA)

v.shares[is.na(v.shares)]<-0;

df.investments<-data.frame(v.diawealth,v.delwealth)%>%dplyr::mutate(dif=v.delwealth-v.diawealth,rDIA=f.roc(v.diawealth),rDEL=f.roc(v.delwealth),rdif=rDEL-rDIA)

# 
# 
# v.wealth<-m.X%*%v.shares
# 
# v.minvest<-df.weights$MV*
# 
# #
# #----All data is held in dt.X.xts
# #step through each data span in the  titak data set
# f.ror<-function(x){
#     r<-x/lag(x)
# }
# 
# iticker<-0;
# #Assemble the price data from ShaRadar----
# while(iticker<nrow(dt.CO)){
#     iticker<-iticker+1;
#     ticker<-dt.CO$ticker[iticker]
#     sectic<-paste(dt.CO$sector[iticker],"/",ticker,".RDATA",sep="");
#     fnin<-paste(dirPriceData,"/",sectic,sep="");
#     
#     ss_dt.p<-load(fnin); #silently load the dt.p structure for this ticker
#     dt.pp<-dt.p%>%dplyr::select(date,closeunadj)
#     dt.pp.xts<-f.df2xtsD(dt.pp,"date")[span]
#     v.x<-coredata(dt.pp.xts$closeunadj)
#    
#     if(iticker==1){
#         m.djp<-matrix(0,length(v.x),30)
#         m.djp[,1]<-v.x
#     }else{
#         m.djp[,iticker]<-v.x
#     }
# }
# colnames(m.djp)<-v.tickers
# df.djp.xts<-xts(m.djp,order.by=index(dt.pp.xts))
# df.djp<-df.djp.xts%>%f.xts2df()
# #----
# #compute
# #----get the supplementary data from original data set----
# df.partial.xts    <- df.data.xts[,c("DIA","$INDU","Computed","Divisor")]
# #add it to the ShaRadar component data
# df.X.xts          <- merge.xts(df.djp.xts,df.partial.xts, join='left');   #merge, based on date
# df.X              <- f.xts2df(df.X.xts)     #convert to a regular data.frame
# #Begin computation
# df.X$Sum30<-rowSums(df.X[,2:31])
# df.X<-df.X%>%dplyr::mutate(DJIAhat=Sum30/Divisor,Error=X.INDU-DJIAhat)
# summary(df.X)
