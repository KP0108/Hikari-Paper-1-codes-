
em.norm <- function(Y,mit,sit)
{
  Yobs <- Y[!is.na(Y)]
  
  Ymis <- Y[is.na(Y)]
  
  n <- length(c(Yobs, Ymis))
  
  r <- length(Yobs)
  
  mut <- mit
  sit <- sit
  
  ll <- function(y, mu, sigma2, n)
  {
    -.5*n*log(2*pi*sigma2)-.5*sum((y-mu)^2)/sigma2
  }
  
  lltm1 <- ll(Yobs, mut, sit, n)
  
  repeat{
    
    EY <- sum(Yobs) + (n-r)*mut
    EY2 <- sum(Yobs^2)+(n-r)*(mut^2+sit)
    
    mut1 <- EY/n
    sit1 <- EY2/n - mut1^2
    
    mut <- mut1
    sit <- sit1
    
    llt <- ll(Yobs, mut, sit, n)
    
    cat(mut, sit, llt, "\n")
    
    if (abs(lltm1-llt) < 0.1) break
    
    lltm1 <- lltm1
    
  }
  
  return(c(mut,sit))
  
}


em.norm(Y,0,0.2)


set.seed(1234)
x <- rnorm(20,5)
x[16:20] <- NA

xobs <- x[!is.na(x)]
  
xmis <- x[is.na(x)]
  
r <- length(xobs)
r
  
mit <- mean(xobs)
sit <- var(xobs)*(r-1)/r

d <- em.norm(x,mit,sit)

d1 <- em.norm(x,0,0.2)

x
pred