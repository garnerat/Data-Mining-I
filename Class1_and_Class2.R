
# Class 1

#### Functions####
square.func <- function(x,y)
{
  x+y^2
  
}

sim <- function(n)
{
  for (i in 1:n)
  {
    x<-rnorm(1)
    y<-rnorm(1)
    z<- rbind(z,5+1.2*x+3*y)
    
  }
}

# Class 2

#### create matrix of randomly generated numbers from normal distribution####

n<- 1000
m<-100
data_sim <- matrix(0,n,m)
for (k in 1:m) {
  data_sim[,k] <- rnorm(n,0,1)
}