f.lagfill<-function(x){
    xlag<-lag(x);
    xlag[1]<-x[1];
    return(xlag)
}
f.myrank<-function(x){
    y<-t(rank(x,na.last=NA))
}
f.GroupTrade<-function(l.Prices,horizon,ub,lb){
    
    #  Prepare the group cumulative ROR table----
    df.xts<-l.Prices[[1]];
    v.nop               <-c(coredata(df.xts$Adj_Open)[-1],last(df.xts$Adj_Open));
    df.xts$nop<-v.nop;
    df<-f.xts2df(df.xts$nop);
    dfr<-df%>%dplyr::mutate(noplag=f.lagfill(nop),ror=log(nop/noplag),cumror=cumsum(ror),nror=lead(ror,default=0));
    #dfr.xts will hold alll the cumulative rates of return for all the companies
    dfr.xts<-f.df2xts(dfr);
    df.Cumror.xts<-dfr.xts$cumror;
    df.nror.xts<-dfr.xts$nror;
    names(df.Cumror.xts)[1]<-names(l.Prices)[1];
    names(df.nror.xts)[1]<-names(l.Prices[1]);
    ico<-1;
    goodico<-1;
    while(ico<length(l.Prices)){
        ico<-ico+1;
        symb<-names(l.Prices)[[ico]];
        
        df.xts<-l.Prices[[ico]];#get the prices for this company
        v.nop               <-c(coredata(df.xts$Adj_Open)[-1],last(df.xts$Adj_Open));
        v.noplag<-f.lagfill(v.nop);
        v.ror<-log(v.nop/v.noplag);
        v.cumror<-cumsum(v.ror);
        v.nror<-lead(v.ror,default=0.0);
        df.xts$cumror<-v.cumror;
        df.xts$nror<-v.nror;
        df.Cumror.xts<-merge.xts(df.Cumror.xts,df.xts$cumror,join='left')
        names(df.Cumror.xts)[ncol(df.Cumror.xts)]<-symb;
        df.nror.xts<-merge.xts(df.nror.xts,df.xts$nror,join='left')
        names(df.nror.xts)[ncol(df.nror.xts)]<-symb;
  
    }
    #----
    #----Rank the Nday performance----
    plot.xts(df.Cumror.xts[,1:5])
   dfR.xts<-diff.xts(df.Cumror.xts,lag=horizon,na.pad=TRUE);
   dfR.xts[is.na(dfR.xts)]<-0;
   
   m.rank<-t(apply(dfR.xts,1,rank));
   m.rank[is.na(dfR.xts)]<-0
  
   m.pos<-0*m.rank;
   m.pos[m.rank>ub]<-1;
   m.pos[m.rank<lb]<-(-1);
   m.nror<-coredata(df.nror.xts)
   m.prof<-m.pos*m.nror;
   m.prof[is.na(m.prof)]<-0;
   m.cumprof<-apply(m.prof,2,cumsum);
   v.totalprof<-rowSums(m.cumprof)
   m.cumprof<-cbind(m.cumprof,v.totalprof);
   colnames(m.cumprof)[ncol(m.cumprof)]<-"Total";
   df.cumprof.xts<-as.xts(m.cumprof,order.by = index(dfR.xts))
   plot.xts(df.cumprof.xts$Total)
   #---
  
   #----
}
ub<-24
