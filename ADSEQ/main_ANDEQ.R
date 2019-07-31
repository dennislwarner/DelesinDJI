#ANDEQ main routine
#notation
#Quantities
#  L  factor 1,   K  factor 2,   A  good 1,   B good 2,  R  value of toal procution
# Parameters
#  p   price of A,   q  price of B
#  omega   Share of L used in production of A
#  theta   share of K used in  priduction of B
#  alpha   exponent in A production function
#  beta    exponent in B production function
#
#  The form of the production functions are given, and are embeded in two production functions
#       f.A for the production of A and f.B for B

# first load the library for 3d scatterplots;
library(scatterplot3d)
f.A<-function(omega,L,theta,K,alpha){
    A<-   ((omega*L*theta*K)/(omega*L+6))^alpha;
    return(A)
}
f.B<-function(omega,L,theta,K,beta){
    B<-(((1-omega)*L*(1-theta)*K)/((1-omega)*L+6))^beta;
    return(B);
}


#set the constants and the initial parameter varlues
L<-47.5829;
K<-100;
p<-1;
q<-2;
omega<-1;
theta<-1;   # all production toward Good A
alpha<- .7;
beta <- .6;
#   a Brute Force approach
#   double loop over factor allocations,  build a 3d grid of revennue answers,  keep track of maximum
stepsize<-.01;   # how much to vary the allocation shares
v.omegaWeights<-seq(from=3*stepsize ,to =1, by = stepsize);
v.thetaWeights<-v.omegaWeights;   #same grid for K share

# define a matrix to hold the resulting coordinates for the two input parameters (omega and theta) and the resulting revenue R
# there will be one row for each solution tried
# the number of rows will be the product of the number of each input parameter
Nrows<- length(v.omegaWeights)*length(v.thetaWeights);
# define a matrix with rows=Nrows and 3 columns 
m.X <-matrix(0,Nrows,3); 
colnames(m.X)<-c("omega","theta","R")

omega<-v.omegaWeights[2];
theta<-v.thetaWeights[2];
maxR<-0;
count<-0;
for(omega in v.omegaWeights){
    for(theta in v.thetaWeights){
        
        A<-f.A(omega,L,theta,K,alpha);
        B<-f.B(omega,L,theta,K,beta);
        R<-p*A+q*B;
        #round answers for clean output
        A<-round(A,digits=3);B=round(B,digits=3);R=round(R,digits=3);
        count<-count+1;
        m.X[count,]<-c(omega,theta,R)
        if(R>maxR){
            maxR<-R;
            omegaMax<-omega;
            thetaMax<-theta;
            Amax<-A;
            Bmax<-B;
            cat(omega,theta,"A=",A,"B=",B,"R=",R,"Max so Far=",maxR,"omegaMax=",omegaMax,"thetaMax=",thetaMax,"Amax=",Amax,"Bmax=",Bmax,"\n");
        }
      
        
    }
    
}
scatterplot3d(m.X,highlight.3d = TRUE,col.axis="blue", col.grid="lightblue",
              main="scatterplot3d - 2", pch=1)
