# Process the downloaded basic filings data , adding derived columns
# Create the entire hiearchy of aggregations
dirOneDrive   <-  "C:/Users/Denni/OneDrive";
dirProject    <-  paste(dirOneDrive,"/Warkleigh",sep="");
source(paste(dirProject,"/Code/0_CreateDirectories.R",sep=""));
source(paste(dirFunctions, "/0_LoadLibs.R", sep = ""));
source(paste(dirFunctions, "/0_DevonFunctions.R", sep = ""));
source(paste(dirRetrieval, "/0c_ProcessFunctions.R", sep = ""), echo = FALSE);
source(paste(dirProject,"/Code/0_Prep.R",sep=""));
dt.Cc<-dt.Common%>%dplyr::filter(!is.na(marketcap));
v.t<-dt.Cc$ticker;
v.bad<-c("TOPS","DRYS","MBVX","HMNY");
v.good<-setdiff(v.t,v.bad)
dt.CD<-dt.Cc%>%dplyr::filter(ticker %in% v.good)
start_time <- Sys.time();

#----------------------------------
#  l. list  v. vector  df data.frame   Agg  Aggregated  Stk  Staxked.  Cmp Ind Sec Eco
#----------------------------------
f.CreateProcessDirectories()      #make sure the destination directories exist
#--------
isec<-0;
l.eco<-list();
secname           <- v.sectors[[1]]
lSectorStack      <- list();
#Traverse sectors
#  Traverse component industries
#    traverse groups
#      compute sums and store
thissector     <- v.sectors[[1]];
l.ESG<-list();
#----Loop Through all Sectors----
l.RSG<-list();  #list of lists  sector x capgroups
tic();

#----------------------------------------------------------------
# df.SS          Raw filing data for a sector ,    stacked company #Quarters x #basic vars data.frames
# df.Cs   Raw    filing data for an industry,  "  "
# df.Csp         Raw filing data plus pricesand Rors      stacked company #weeks x #basic vars + ror data.frame  
#
#l.IG  [[group]]
#l.IsG [[industry]][[group]]
#l.IGx [[group]] expanded
#l.SG [[group]] a sector, basic plus prices
#l.SGx[[group]] expanded sectors
#l.SsG [[sector]][[group]] the economy
#l.EG  [[group]]   the aggregated economy basic
#l.EGx [[group]]   the expanded aggregated economy

#  aggregation levels   C=companies  I=industries S=sectors E=Economy
#  Derivation levels   R=Raw  P=wih RORs      X=Expanded  (exclusive)
#  Frequency  levels   Q=quarterly  W=weekly
#  Structure  df. = data frame  l.=list
#----------------------------------------------------------------
l.SsG<-list();
f.markTime(start_time,"Begin Sectors")
for(thissector in v.sectors){
    tic();
    #cat("Begin Sector",thissector,"\n");
    #----Load Raw Sector Data----
    df.SS    <-  f.loadSector(thissector);
    lsec<-list();
    #----
    v.industries<-sort(unique(df.SS$industry));
    thisindustry<-v.industries[[1]]
    #lIndustryGroupAggs<-list();
    l.IsG<-list();
    for(thisindustry in v.industries){
        #cat("----------Begin Industry-",thisindustry,"\n");
        df.Cs             <- df.SS %>% dplyr::filter(industry==thisindustry);
        df.Csp         <- f.buildCompanies(df.Cs,thissector);
        l.IG    <- f.getRawIndustryCapGroups(thisindustry,df.Csp);
        l.IsG[[thisindustry]]<-l.IG;#l.RIG is a list of lists [[industry]][[capgroup]]
        
        #-----Expand the aggregated groups for this industry, and save
        
        l.IGx<-f.expandIndustryCapGroups(l.IG); #l.IG is a list [[capgroup]] 
        fnout<-paste(dirData,"/ProcessedSF/",thissector,"/",thisindustry,"/l.C.RDATA",sep="");
        df.Cs<-df.Csp%>%
            dplyr::select(-mkror,-mkr1w,-mkr1m,-mkr1q,-mkr2q,-mkr1y,-mkr2y)
        df.Csx<-  f.AddMeasures(df.Cs,PPY=12)
        #break into individual company .xts frames and save as extended list
        v.t<-sort(unique(df.Csx$ticker));
        tic<-v.t[1]
        l.C<-list();
        for(tic in v.t){
            df.c<-df.Csx%>%filter(ticker==tic);
            df.c.xts<-f.dfall2xts(df.c)
            l.C[[tic]]<-df.c.xts
        }
        #Clean the list of companies in each industry
        
        l.CProfitsRaw  <- f.computeCompanyProfits(l.C,dt.CD);
        v.bad2<-setdiff(names(l.CProfitsRaw),dt.CD$ticker);
        v.bad3<-c(v.bad,v.bad2)
        l.C<-list.remove(l.C,v.bad3)
        
        
        
        save(l.C,file=fnout);
        fnout<-paste(dirData,"/ProcessedSF/",thissector,"/",thisindustry,"/l.IGx.RDATA",sep="");
        save(l.IGx,file=fnout);
       cat( f.markTime(start_time,paste(" End Industry ",thisindustry,sep="")),"\n");
    }
    l.SG <- f.getRawSectorCapGroups(thissector,l.IsG);# [[capgroup]]
    l.SsG[[thissector]]  <- l.SG;#l.RSG [[sector]][[capgroup]]
    #-----Expand the aggregated groups for this sector, and save
    l.SGx<-f.expandSectorCapgroups(l.SG);
    fnout<-paste(dirData,"/ProcessedSF/",thissector,"/l.SGx.RDATA",sep="");
    save(l.SGx,file=fnout);
    fnout<-paste(dirData,"/ProcessedSF/",thissector,"/df.SGx.RDATA",sep="");
    df.SGx<-l.SGx[["All"]];
    save(df.SGx,file=fnout);
    msg<-paste(" End Sector ",thissector,sep="")
    cat(f.markTime(start_time,msg),"\n");
}    
toc();
l.EG<-f.getRawEconCapGroups(l.SsG);
l.EGx<-f.expandEconCapGroups(l.EG);
fnout<-paste(dirData,"/ProcessedSF/l.EGx.RDATA",sep="");
save(l.EGx,file=fnout);
df.EGx<-l.EGx[["All"]]
fnout<-paste(dirData,"/ProcessedSF/df.EGx.RDATA",sep="");
save(df.EGx,file=fnout)
f.markTime(start_time," End Process Filings ");
toc();
