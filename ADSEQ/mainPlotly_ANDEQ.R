#ANDEQ main routine
#notation
#Quantities
#  J  factor Information Kaqpital,   L  Labor,   A  good 1,   B good 2,  R  value of toal procution
# Parameters
#  p   price of A,   q  price of B
#  omega   Share of J used in production of A
#  theta   share of L used in  priduction of B
#  alpha   exponent in A production function
#  beta    exponent in B production function
#
#  The form of the production functions are given, and are embeded in two production functions
#       f.A for the production of A and f.B for B

# first load the library for 3d scatterplots;
library(scatterplot3d);
#try the plotly library
suppressMessages(library(plotly));
suppressMessages(library(qdapTools))

#define production functions

f.A<-function(omega,J,theta,L,alpha){
    A<-   ((omega*J*theta*L)/(omega*J+6))^alpha;
    return(A)
}
f.B<-function(omega,J,theta,L,beta){
    B<-(((1-omega)*J*(1-theta)*L)/((1-omega)*J+6))^beta;
    return(B);
}

#=============================================================================
#set the constants and the initial parameter varlues
J<-47.5829;
L<-100;
p<-1;
q<-2;
omega<-1;
theta<-1;   # all production toward Good A
alpha<- .7;
beta <- .6;
#=============================================================================

#   a Brute Force approach
#   double loop over factor allocations,  build a 3d grid of revennue answers,  keep track of maximum
minparm        <- 0.0      # avoid the 
maxparm        <- 1.0

stepsize       <- 0.01;   # how much to vary the allocation shares
v.omegaWeights <- seq(from=minparm ,to =maxparm, by = stepsize);
v.thetaWeights <- v.omegaWeights;   #same grid for K share

#set up iteration over Info Kap exponents
v.phi<-seq(from=.5,to=1.0,length.out=51); #50 values with equal gap between them 
#add the .729 specific value
v.phi<-sort(c(v.phi,0.729));

df.sizeRes<-data.frame(matrix(0,length(v.phi),4));
#=======


names(df.sizeRes)<-c("phi","omegaMax","thetaMax","RMax")
# define a matrix to hold the resulting coordinates for the two input parameters (omega and theta) and the resulting revenue R
# there will be one row for each solution tried
# the number of rows will be the product of the number of each input parameter
#also define an equivalent NxN matrix to enable a simple heat-map type numeric display in excel

Nomega<-length(v.omegaWeights);
Ntheta<-length(v.thetaWeights);
Nphi    <-length(v.phi);
Nrows<- Nomega*Ntheta*Nphi;
m.Z <- matrix(0,Nrows,3)
colnames(m.Z)<-c("Info Cap","Labor","R")


#=======
phi<-v.phi[2];
omega<-v.omegaWeights[2];
theta<-v.thetaWeights[2];



l.Bests<-list();
iphi<-0;  
 count<-0;

while(iphi<Nphi){

    iphi<-iphi+1;
    phi<-v.phi[iphi];
    J<-200^phi
   
    cat("Beginning phi=",phi,"\n");
    # define a matrix with rows=Nrows and 3 columns , distinct for each JSize
    m.X <-matrix(0,Nomega+1,Ntheta+1); 
    maxR<-0;    #initialize the solution value;
    iomega<-0;
    while(iomega<Nomega){
        iomega<-iomega+1;
        omega<-v.omegaWeights[iomega];
        m.X[iomega+1,1]<-omega*J;
        itheta<-0;
        while(itheta<Ntheta){
            itheta<-itheta+1;
            theta<-v.thetaWeights[itheta];
            #fill margins of m.X table for surface table
            m.X[1,itheta+1]<-theta;
            #----for this value of J, omega, and theta compute the production quantities and the Revenue
            A<-f.A(omega,J,theta,L,alpha);
            B<-f.B(omega,J,theta,L,beta);
            R<-p*A + q*B;        #revenue
            #----Save the results in the tables
            m.X[iomega+1,itheta+1]<-R;
            count<-count+1;
            m.Z[count,]<-c(omega*J,theta*L,R);
            #---Capture new maximums
            if(R>maxR){
                v.max<-c(phi=phi,
                         Jamt=round(J,digits=3),
                         omegaOpt=omega,
                         JMax=round(omega*J,digits=3),
                         thetaOpt=theta,
                         Lopt=round(theta*L,digits=3),
                         AOpt=round(A,digits=2),
                         BOpt=round(B,digits=2),
                         ROpt=round(R,digits=3));
                maxR<-R;
                l.Bests[[iphi]]<-data.frame(t(v.max));
            }
        }
    }
    # maintitle<-paste("Revenue where InfoCap exponent = ",phi,sep="");
    # 
    # scatterplot3d(m.Z,highlight.3d = FALSE,col.axis="blue", col.grid="lightblue",
    #               main=maintitle, pch=1,xlab="InfoCap used for A",ylab="Labor used for A",
    #               grid=TRUE,color="red");
    
}
df.Bests<-list_df2df(l.Bests)[,-1];
fnout<-"OptTable_VaryPhi.csv";
write.csv(df.Bests,file=fnout);
