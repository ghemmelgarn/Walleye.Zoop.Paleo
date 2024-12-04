#this simulates data for zooplankton and fish over time
#created with Jeremiah 12/4/24 and the numbers we used are almost totally random

library(ggplot2)


z1 <- rnorm(1,30,1)
f1 <- rnorm(1,20+0.4*z1,5)
z2 <- rnorm(1, 0.5*z1-0.1*f1,1)
f2 <- rnorm(1, 0.4*z2+0.9*f1, 5)

data.frame(z=c(z1,z2), f=c(f1,f2), t=c(1,2))

x <- data.frame(z=c(z1,z2), f=c(f1,f2), t=c(1,2))

ggplot(data=x, aes(x=t))+
  geom_point(aes(y=z),color = "red")+
  geom_point(aes(y=f),color = "blue")


  


