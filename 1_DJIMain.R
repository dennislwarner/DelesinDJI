dirOneDrive         <-  "C:/Users/Denni/OneDrive";
dirProject          <-  paste(dirOneDrive,"/DelesinDJI/DDJI",sep="");
source(paste(dirProject, "/0_AllFunctions.R", sep = ""));
source(paste(dirProject, "/0_LoadLibs.R", sep = ""));
dirDocs             <-   paste(dirProject, "/Docs", sep = "");

source(paste(dirProject,"/0_Prep.R",sep=""));
dirData<-"D:/WarkleighD/WarkleighData";
dirPriceData<-paste(dirData,"/AdjOpens/Sectors",sep="");

#original data from MH

fnin<-"DJ_OrigData.csv";
df.data<-read_csv(fnin);
df.data.xts<-f.dfall2xts(df.data);   Get mike's original data in xts form
firstdate <-index(first(df.data.xts));
lastdate  <-index(last(df.data.xts));
span<-paste(firstdate,"/",lastdate,sep="");

dt.CO<-dt.Common%>%dplyr::filter(ticker %in% v.tickers); #select the dictionary entries for the 30 DJI stocks
iticker<-0;
while(iticker<nrow(dt.CO)){
    iticker<-iticker+1;
    ticker<-dt.CO$ticker[iticker]
    sectic<-paste(dt.CO$sector[iticker],"/",ticker,".RDATA",sep="");
    fnin<-paste(dirPriceData,"/",sectic,sep="");
    ss_dt.p<-load(fnin); #silently load the dt.p structure for this ticker

    dt.p.xts<-f.df2xtsD(dt.p,"date")
}

dirlist<-list.dirs(dirPriceData)
v.pricefiles<-list.files(dirPriceData,recursive = TRUE)
v.djifiles<-paste(v.tickers,".RDATA",sep="")
v.w<-str_which(v.pricefiles,v.djifiles)
v.pricefnames<-str_subset(v.pricefiles,v.djifiles)
