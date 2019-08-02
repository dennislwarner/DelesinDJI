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
minparm        <- 0.2      # avoid the 
maxparm        <- 0.8

stepsize       <- 0.01;   # how much to vary the allocation shares
v.omegaWeights <- seq(from=minparm ,to =maxparm, by = stepsize);
v.thetaWeights <- v.omegaWeights;   #same grid for K share

#set up iteration over Info Kap Amounts
v.JSize<-seq(from=sqrt(200),to=200,length.out=20); #20 values with equal gap between them 
df.sizeRes<-data.frame(mattrix(0,length(0,v.JSize),4));
names(df.sizeRes)<-c("Jamt","omegaMax","thetaMax","RMax")
# define a matrix to hold the resulting coordinates for the two input parameters (omega and theta) and the resulting revenue R
# there will be one row for each solution tried
# the number of rows will be the product of the number of each input parameter
#also define an equivalent NxN matrix to enable a simple heat-map type numeric display in excel
NN<-length(v.omegaWeights);
# define a matrix with rows=Nrows and 3 columns 
m.X <-matrix(0,NN+1,NN+1); 

Nrows<- length(v.omegaWeights)*length(v.thetaWeights);
m.Z <- matrix(0,Nrows,3)
colnames(m.Z)<-c("Info Cap","Labor","R")

omega<-v.omegaWeights[2];
theta<-v.thetaWeights[2];
maxR<-0;
count<-0;
iJ<-0;
iL<-0;
iSIZE<-0;  
for(J in v.JSize){   #egin loop over a
    iSIZE<-iSIZE+1;

    for(omega in v.omegaWeights){
        iJ<-iJ+1;
        m.X[iJ+1,1]<-omega*J;
        iL<-0;
        for(theta in v.thetaWeights){
            iL<-iL+1;
            m.X[1,iL+1]<-theta*L;
            A<-f.A(omega,J,theta,L,alpha);
            B<-f.B(omega,J,theta,L,beta);
            R<-p*A+q*B;
            m.X[iJ+1,iL+1]<-R;
            #round answers for clean output
            A<-round(A,digits=3);
            B=round(B,digits=3);
            R=round(R,digits=3);
            count<-count+1;
            m.Z[count,]<-c(omega*J,theta*L,R);
            if(R>maxR){
                maxR<-R;
                omegaMax<-omega;
                thetaMax<-theta;
                Amax<-A;
                Bmax<-B;
                cat(omega,theta,"A=",A,"B=",B,"R=",R,"Max so Far=",maxR,"omegaMax=",
                    omegaMax,"thetaMax=",thetaMax,"Amax=",Amax,"Bmax=",Bmax,"\n");
            }
          }
    }
}

#----Calculations are done,  store the output in graphic and table form

scatterplot3d(m.Z,highlight.3d = TRUE,col.axis="blue", col.grid="lightblue",
              main="Revenue Surface  Base Case ", pch=1);

df.maxtable<-data.frame(m.X);
fn<-"Maxtable_BaseCase.csv";
write.csv(df.maxtable,file=fn);
