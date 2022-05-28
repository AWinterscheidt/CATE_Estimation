library(readxl)
library(aod)
library(ggplot2)
library(readxl)
library(dplyr)
library(loess)
data <- read_excel("Desktop/FILE_5188.xlsx")
d1 = subset(data,wb!=2)
d2 = subset(data,wa!=2)
d3 = subset(data,wh!=2)



grouper <- function(df, n) {
  
  # create a random number for each row
  random <- sample(1:nrow(df), replace = FALSE, nrow(df))
  
  # divide the random number by the group size
  d<- df$group_number <- ceiling(random / (nrow(df) / n))
  
  return(d)  
}

d1$s<-grouper(d1,3)
d2$s<-grouper(d2,3)
d3$s<-grouper(d3,3)


d1a= subset(d1,s==1)
d2a= subset(d2,s==1)
d3a= subset(d3,s==1)

d1b= subset(d1,s==2)
d2b= subset(d2,s==2)
d3b= subset(d3,s==2)

d1c= subset(d1,s==3)
d2c= subset(d2,s==3)
d3c= subset(d3,s==3)


#Black
    #1
  pihat <- predict(glm(wb~pd,family=binomial(link="logit"),data=d1a),newdata=d1,type='response')  
  mu1hat <- predict(smooth.spline(d1$pd[d1$wb==1&d1$s==2],d1$pt[d1$wb==1&d1$s==2]),d1$pd)$y
  mu0hat <- predict(smooth.spline(d1$pd[d1$wb==0&d1$s==2],d1$pt[d1$wb==0&d1$s==2]),d1$pd)$y
  ## construct estimators
  d1$pseudo <- ((d1$wb-pihat)/(pihat*(1-pihat)))*(d1$pt-d1$wb*mu1hat-(1-d1$wb)*mu0hat) + mu1hat-mu0hat
 
  d1$drl <- predict(smooth.spline(d1$pd[d1$s==3],d1$pseudo[d1$s==3]),d1$pd)$y
    #2
  pihatb <- predict(glm(wb~pd,family=binomial(link="logit"),data=d1b),newdata=d1,type='response')  
  mu1hatb <- predict(smooth.spline(d1$pd[d1$wb==1&d1$s==3],d1$pt[d1$wb==1&d1$s==3]),d1$pd)$y
  mu0hatb <- predict(smooth.spline(d1$pd[d1$wb==0&d1$s==3],d1$pt[d1$wb==0&d1$s==3]),d1$pd)$y
  
  d1$pseudo2 <- ((d1$wb-pihatb)/(pihatb*(1-pihatb)))*(d1$pt-d1$wb*mu1hatb-(1-d1$wb)*mu0hatb) + mu1hatb-mu0hatb
  
  d1$drl2 <- predict(smooth.spline(d1$pd[d1$s==1],d1$pseudo2[d1$s==1]),d1$pd)$y
    #3
  pihatc <- predict(glm(wb~pd,family=binomial(link="logit"),data=d1a),newdata=d1,type='response')  
  mu1hatc <- predict(smooth.spline(d1$pd[d1$wb==1&d1$s==3],d1$pt[d1$wb==1&d1$s==3]),d1$pd)$y
  mu0hatc <- predict(smooth.spline(d1$pd[d1$wb==0&d1$s==3],d1$pt[d1$wb==0&d1$s==3]),d1$pd)$y
    
  d1$pseudo3 <- ((d1$wb-pihatc)/(pihatc*(1-pihatc)))*(d1$pt-d1$wb*mu1hatc-(1-d1$wb)*mu0hatc) + mu1hatc-mu0hatc
  
  d1$drl3 <- predict(smooth.spline(d1$pd[d1$s==2],d1$pseudo3[d1$s==2]),d1$pd)$y
  
  for (i in 1:100) {
    d1$drlavg[i]<-(d1$drl[i]+d1$drl2[i]+d1$drl3[i])/3
  }
  ggplot(data=d1,aes(pd,drlavg))+
    geom_smooth(show.legend = TRUE, colour="darkorchid4")+
    geom_point(colour = "mediumorchid2")+
    xlab("Covid Fatality Rate")+
    ylab(expression(paste(tau,"(x)")))+
    ggtitle("Difference in turnout conditioned on covid fatality rates in Black voters vs White voters")
  
#Asian
    #1
  d2$pihat2 <- predict(glm(wa~pd,family=binomial(link="logit"),data=d2a),newdata=d2,type='response')  
  d2$mu1hat2 <- predict(smooth.spline(d2$pd[d2$wa==1&d2$s==2],d2$pt[d2$wa==1&d2$s==2]),d2$pd)$y
  d2$mu0hat2 <- predict(smooth.spline(d2$pd[d2$wa==0&d2$s==2],d2$pt[d2$wa==0&d2$s==2]),d2$pd)$y
 
  for (i in 1:100) {
    d2$pseudo[i] <- ((d2$wa[i]-d2$pihat2[i])/(d2$pihat2[i]*(1-d2$pihat2[i])))  *  (d2$pt[i]-d2$wa[i]*d2$mu1hat2[i]-(1-d2$wa[i])*d2$mu0hat2[i]) + d2$mu1hat2[i]-d2$mu0hat2[i]
  }
 
  
  d2$drl <- predict(smooth.spline(d2$pd[d2$s==3],d2$pseudo[d2$s==3]),d2$pd)$y
  
    #2
 d2$pihat2b <- predict(glm(wa~pd,family=binomial(link="logit"),data=d2b),newdata=d2,type='response')  
  d2$mu1hat2b <- predict(smooth.spline(d2$pd[d2$wa==1&d2$s==3],d2$pt[d2$wa==1&d2$s==3]),d2$pd)$y
  d2$mu0hat2b <- predict(smooth.spline(d2$pd[d2$wa==0&d2$s==3],d2$pt[d2$wa==0&d2$s==3]),d2$pd)$y
  
  d2$pseudo2 <- ((d2$wa-pihat2b)/(pihat2b*(1-pihat2b)))*(d2$pt-d2$wa*mu1hat2b-(1-d2$wa)*mu0hat2b) + mu1hat2b-mu0hat2b
  
  d2$drl2 <- predict(smooth.spline(d2$pd[d2$s==1],d2$pseudo2[d2$s==1]),d2$pd)$y
  
    #3
  pihat2c <- predict(glm(wa~pd,family=binomial(link="logit"),data=d2a),newdata=d2,type='response')  
  mu1hat2c <- predict(smooth.spline(d2$pd[d2$wa==1&d2$s==3],d2$pt[d2$wa==1&d2$s==3]),d2$pd)$y
  mu0hat2c <- predict(smooth.spline(d2$pd[d2$wa==0&d2$s==3],d2$pt[d2$wa==0&d2$s==3]),d2$pd)$y

  d2$pseudo3 <- ((d2$wa-pihat2c)/(pihat2c*(1-pihat2c)))*(d2$pt-d2$wa*mu1hat2c-(1-d2$wa)*mu0hat2c) + mu1hat2c-mu0hat2c
  
  d2$drl3 <- predict(smooth.spline(d2$pd[d2$s==2],d2$pseudo3[d2$s==2]),d2$pd)$y
  
  for (i in 1:100) {
    d2$drlavg[i]<-(d2$drl[i]+d2$drl2[i]+d2$drl3[i])/3
  }
  ggplot(data=d2,aes(pd,drl))+
    geom_smooth(show.legend = TRUE, colour="chocolate3")+
    geom_point(colour = "darkorange")+
    xlab("Covid Fatality Rate")+
    ylab(expression(paste(tau,"(x)")))+
    ggtitle("Difference in turnout conditioned on covid fatality rates in Asian voters vs White voters")
  
  
##Hispanic  
    #1
  pihat3 <- predict(glm(wh~pd,family=binomial(link="logit"),data=d3a),newdata=d3,type='response')  
  mu1hat3 <- predict(smooth.spline(d3$pd[d3$wh==1&d3$s==2],d3$pt[d3$wh==1&d3$s==2]),d3$pd)$y
  mu0hat3 <- predict(smooth.spline(d3$pd[d3$wh==0&d3$s==2],d3$pt[d3$wh==0&d3$s==2]),d3$pd)$y

  d3$pseudo <- ((d3$wh-pihat3)/(pihat3*(1-pihat3)))*(d3$pt-d3$wh*mu1hat3-(1-d3$wh)*mu0hat3) + mu1hat3-mu0hat3
  
  d3$drl <- predict(smooth.spline(d3$pd[d3$s==3],d3$pseudo[d3$s==3]),d3$pd)$y
    #2
  pihat3b <- predict(glm(wh~pd,family=binomial(link="logit"),data=d3b),newdata=d3,type='response')  
  mu1hat3b <- predict(smooth.spline(d3$pd[d3$wh==1&d3$s==3],d3$pt[d3$wh==1&d3$s==3]),d3$pd)$y
  mu0hat3b <- predict(smooth.spline(d3$pd[d3$wh==0&d3$s==3],d3$pt[d3$wh==0&d3$s==3]),d3$pd)$y
  
  d3$pseudo2 <- ((d3$wh-pihat3b)/(pihat3b*(1-pihat3b)))*(d3$pt-d3$wh*mu1hat3b-(1-d3$wh)*mu0hat3b) + mu1hat3b-mu0hat3b
  
  d3$drl2 <- predict(smooth.spline(d3$pd[d3$s==1],d3$pseudo2[d3$s==1]),d3$pd)$y

    #3

  pihat3c <- predict(glm(wh~pd,family=binomial(link="logit"),data=d3a),newdata=d3,type='response')  
  mu1hat3c <- predict(smooth.spline(d3$pd[d3$wh==1&d3$s==3],d3$pt[d3$wh==1&d3$s==3]),d3$pd)$y
  mu0hat3c <- predict(smooth.spline(d3$pd[d3$wh==0&d3$s==3],d3$pt[d3$wh==0&d3$s==3]),d3$pd)$y
  
  d3$pseudo3 <- ((d3$wh-pihat3c)/(pihat3c*(1-pihat3c)))*(d3$pt-d3$wh*mu1hat3c-(1-d3$wh)*mu0hat3c) + mu1hat3c-mu0hat3c
  
  d3$drl3 <- predict(smooth.spline(d3$pd[d3$s==2],d3$pseudo3[d3$s==2]),d3$pd)$y
  for (i in 1:100) {
    d3$drlavg[i]<-(d3$drl[i]+d3$drl2[i]+d3$drl3[i])/3
  }
  ggplot(data=d3,aes(pd,drlavg))+
    geom_smooth(show.legend = TRUE, colour="aquamarine4")+
    geom_point(colour = "mediumaquamarine")+
    xlab("Covid Fatality Rate")+
    ylab(expression(paste(tau,"(x)")))+
    ggtitle("Difference in turnout conditioned on covid fatality rates in Hispanic voters vs White voters")


