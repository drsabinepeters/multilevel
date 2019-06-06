##set working directory 
setwd('C:/Users/peterss/surfdrive/Work/T3/R')

##Load libraryies
library (ggplot2)
library (nlme)
library(boot)

##Read in data, adjust datafile name. Data is now called 'longidata'
longidata<-read.csv2('DataBraintime_voorR21april2017.csv',header=T)
longidata_FBover70<-subset(longidata,longidata$learningrate>70)
longidata_FBover70_not100<-subset(longidata,longidata$learningrate>70&longidata$learningrate<100)
longidata_FBover70_under95<-subset(longidata,longidata$learningrate>70&longidata$learningrate<95)


#check if it looks OK
str(longidata)
tail(longidata_FBover70_not100)



















##_________________________________________________________________
##testing polynomials ENEP_CP

RandomIntercept<-lme(anataccumbensLR_ENEP_CP~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anataccumbensLR_ENEP_CP~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anataccumbensLR_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anataccumbensLR_ENEP_CP~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)


RandomIntercept<-lme(anatdorsalcaudateLR_ENEP_CP~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatdorsalcaudateLR_ENEP_CP~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatdorsalcaudateLR_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatdorsalcaudateLR_ENEP_CP~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)

RandomIntercept<-lme(anatventralcaudateLR_ENEP_CP~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatventralcaudateLR_ENEP_CP~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatventralcaudateLR_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatventralcaudateLR_ENEP_CP~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)





RandomIntercept<-lme(anatACC_ENEP_CP~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatACC_ENEP_CP~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatACC_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatACC_ENEP_CP~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomIntercept)

RandomIntercept<-lme(anatMFG_ENEP_CP~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatMFG_ENEP_CP~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatMFG_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatMFG_ENEP_CP~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)


RandomIntercept<-lme(anatSMA_ENEP_CP~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatSMA_ENEP_CP~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatSMA_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatSMA_ENEP_CP~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)

RandomIntercept<-lme(anatSPL_ENEP_CP~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatSPL_ENEP_CP~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatSPL_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatSPL_ENEP_CP~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)




##_________________________________________________________________
##testing polynomials EP_EN


RandomIntercept<-lme(anataccumbensLR_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anataccumbensLR_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anataccumbensLR_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anataccumbensLR_EP_EN~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)

RandomIntercept<-lme(anatdorsalcaudateLR_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatdorsalcaudateLR_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatdorsalcaudateLR_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatdorsalcaudateLR_EP_EN~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomIntercept)


RandomIntercept<-lme(anatventralcaudateLR_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatventralcaudateLR_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatventralcaudateLR_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatventralcaudateLR_EP_EN~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_lin)




RandomIntercept<-lme(anatACC_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatACC_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatACC_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatACC_EP_EN~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

RandomIntercept<-lme(anatMFG_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatMFG_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatMFG_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatMFG_EP_EN~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

RandomIntercept<-lme(anatSMA_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatSMA_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatSMA_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatSMA_EP_EN~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

RandomIntercept<-lme(anatSPL_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(anatSPL_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatSPL_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatSPL_EP_EN~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

























##_____________________________________________________________
##spaghetti ENEP_CP

g1 <- ggplot(data = longidata, aes(x = age, y = anataccumbensLR_ENEP_CP, group = subject))
g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("N.Accumbens")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)

#without lines
g1 <- ggplot(data = longidata, aes(x = age, y = anataccumbensLR_ENEP_CP, group = subject))
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("N.Accumbens")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)


#without lines
g1 <- ggplot(data = longidata, aes(x = age, y = anatdorsalcaudateLR_ENEP_CP, group = subject))
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Dorsal caudate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)


g1 <- ggplot(data = longidata, aes(x = age, y = anatventralcaudateLR_ENEP_CP, group = subject))
g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Ventral caudate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)

#without lines
g1 <- ggplot(data = longidata, aes(x = age, y = anatventralcaudateLR_ENEP_CP, group = subject))
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Ventral caudate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)











































##_____________________________________________________________
##spaghetti EP_EN

g1 <- ggplot(data = longidata, aes(x = age, y = anataccumbensLR_EP_EN, group = subject))
g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("N.Accumbens")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)

#withoutlines
g1 <- ggplot(data = longidata, aes(x = age, y = anataccumbensLR_EP_EN, group = subject))
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("N.Accumbens")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)


g1 <- ggplot(data = longidata, aes(x = age, y = anatdorsalcaudateLR_EP_EN, group = subject))
g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Dorsal caudate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)

#without lines
g1 <- ggplot(data = longidata, aes(x = age, y = anatdorsalcaudateLR_EP_EN, group = subject))
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Dorsal caudate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)


g1 <- ggplot(data = longidata, aes(x = age, y = anatventralcaudateLR_EP_EN, group = subject))
g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Ventral caudate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)

#without lines
g1 <- ggplot(data = longidata, aes(x = age, y = anatventralcaudateLR_EP_EN, group = subject))
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Ventral caudate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(-8,8)
print (g1)










































##___________________________
##predicted ENEP_CP 





#accumbens
predictedvalue<-lme(anataccumbensLR_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Accumbens")+
  ylim(-1.5,1)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))




#dorsalcaudate
predictedvalue<-lme(anatdorsalcaudateLR_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 

g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Dorsal caudate")+
  ylim(0, 2)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))





#ventralcaudate
predictedvalue<-lme(anatventralcaudateLR_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 



g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Ventral caudate")+
  ylim(-0.5,2)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))



#ACC
predictedvalue<-lme(anatACC_ENEP_CP~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("ACC")+
  ylim(-1.7,1)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))



#MFG
predictedvalue<-lme(anatMFG_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("MFG")+
  ylim(0,3)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))





#SMA
predictedvalue<-lme(anatSMA_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("SMA")+
  ylim(-1.5,2)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))






#SPL
predictedvalue<-lme(anatSPL_ENEP_CP~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("SPL")+
  ylim(-2,2)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))




















##___________________________
##predicted EP_EN 

#accumbens
predictedvalue<-lme(anataccumbensLR_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Accumbens")+
  ylim(-0.5,1.7)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))




#dorsalcaudate
predictedvalue<-lme(anatdorsalcaudateLR_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 

g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Dorsal caudate")+
  ylim(-1.5,0)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))




#ventralcaudate
predictedvalue<-lme(anatventralcaudateLR_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Ventral caudate")+
  ylim(-2.0,0.5)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))












#ACC
predictedvalue<-lme(anatACC_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("ACC")+
  ylim(-1.5,1)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))



#MFG
predictedvalue<-lme(anatMFG_EP_EN~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("MFG")+
  ylim(-1.5,1.5)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))



#SMA
predictedvalue<-lme(anatSMA_EP_EN~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("SMA")+
  ylim(-2,2.5)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))



#SPL
predictedvalue<-lme(anatSPL_EP_EN~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue_predictedvalue<-predict(predictedvalue, longidata, level=0)
longidata.designmat_predictedvalue<-model.matrix(eval(eval(predictedvalue$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue_predictedvalue<-sqrt(diag(longidata.designmat_predictedvalue %*% predictedvalue$varFix %*% t(longidata.designmat_predictedvalue)))
longidata$lowerCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue-(1.96*longidata$SDvalue_predictedvalue))
longidata$upperCIvalue_predictedvalue<-(longidata$longidata.predvalue_predictedvalue+(1.96*longidata$SDvalue_predictedvalue)) 


g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("SPL")+
  ylim(-1,1.5)+
  
  geom_line(aes(x=age, y = longidata.predvalue_predictedvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue_predictedvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue_predictedvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))






















##______________learning rate without outliers (FB>70)





g1 <- ggplot(data = longidata_FBover70, aes(x = age, y = learningrate, group = subject))
g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Learning rate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(60,100)
print (g1)

#without lines
g1 <- ggplot(data = longidata_FBover70, aes(x = age, y = learningrate, group = subject))
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Learning rate")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(60,100)
print (g1)

RandomIntercept<-lme(learningrate~1, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(learningrate~poly(age,1), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
#RandomInterceptAge_cub<-lme(learningrate~poly(age,3), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)

anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad)
summary(RandomInterceptAge_quad)

RandomInterceptAge_quad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$longidata_FBover70.predvalue<-predict(RandomInterceptAge_quad, longidata_FBover70, level=0)
longidata_FBover70.designmat<-model.matrix(eval(eval(RandomInterceptAge_quad$call$fixed)[-2]), longidata_FBover70[-3]) 
longidata_FBover70$SDvalue<-sqrt(diag(longidata_FBover70.designmat %*% RandomInterceptAge_quad$varFix %*% t(longidata_FBover70.designmat)))
longidata_FBover70$lowerCIvalue<-(longidata_FBover70$longidata_FBover70.predvalue-(1.96*longidata_FBover70$SDvalue))
longidata_FBover70$upperCIvalue<-(longidata_FBover70$longidata_FBover70.predvalue+(1.96*longidata_FBover70$SDvalue)) 

g1 <- ggplot(longidata_FBover70) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Performance")+
  ylim(80,100)+
  geom_line(aes(x=age, y = longidata_FBover70.predvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue), linetype="dotted")  + 
  geom_line(aes(x = age, y=upperCIvalue), linetype="dotted") +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))



##Plotje in main fig1 (main manuscript ipv supplement)

RandomInterceptAge_quad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$longidata_FBover70.predvalue<-predict(RandomInterceptAge_quad, longidata_FBover70, level=0)
longidata_FBover70.designmat<-model.matrix(eval(eval(RandomInterceptAge_quad$call$fixed)[-2]), longidata_FBover70[-3]) 
longidata_FBover70$SDvalue<-sqrt(diag(longidata_FBover70.designmat %*% RandomInterceptAge_quad$varFix %*% t(longidata_FBover70.designmat)))
longidata_FBover70$lowerCIvalue<-(longidata_FBover70$longidata_FBover70.predvalue-(1.96*longidata_FBover70$SDvalue))
longidata_FBover70$upperCIvalue<-(longidata_FBover70$longidata_FBover70.predvalue+(1.96*longidata_FBover70$SDvalue)) 

g1 <- ggplot(longidata_FBover70) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Learning performance")+
  ylim(80,100)+
  geom_line(aes(x=age, y = longidata_FBover70.predvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue), linetype="dotted",color="grey60")  +
  geom_line(aes(x = age, y=upperCIvalue), linetype="dotted",color="grey60") +
  
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 12), axis.text.y = element_text (size = 12), axis.title.y = element_text (size = 12), axis.title.x = element_text (size = 12), legend.text = element_text (size = 14))
















































#__________________________________________________________________________________
# learning rate with outliers


g1 <- ggplot(data = longidata, aes(x = age, y = learningrate, group = subject))
g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Learning Performance")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(45,100)
print (g1)

#without lines
g1 <- ggplot(data = longidata, aes(x = age, y = learningrate, group = subject))
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Learning Performance")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(45,100)
print (g1)

RandomIntercept<-lme(learningrate~1, data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(learningrate~poly(age,1), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(learningrate~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
#RandomInterceptAge_cub<-lme(learningrate~poly(age,3), data=longidata,random=~1|subject,method="M",na.action=na.exclude)

#anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad)


RandomInterceptAge_quad<-lme(learningrate~poly(age,2), data=longidata,random=~1|subject,method="M",na.action=na.exclude)
longidata$longidata.predvalue<-predict(RandomInterceptAge_quad, longidata, level=0)
longidata.designmat<-model.matrix(eval(eval(RandomInterceptAge_quad$call$fixed)[-2]), longidata[-3]) 
longidata$SDvalue<-sqrt(diag(longidata.designmat %*% RandomInterceptAge_quad$varFix %*% t(longidata.designmat)))
longidata$lowerCIvalue<-(longidata$longidata.predvalue-(1.96*longidata$SDvalue))
longidata$upperCIvalue<-(longidata$longidata.predvalue+(1.96*longidata$SDvalue)) 

g1 <- ggplot(longidata) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Learning Performance")+
  ylim(80,100)+
  geom_line(aes(x=age, y = longidata.predvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue), linetype="dotted")  + 
  geom_line(aes(x = age, y=upperCIvalue), linetype="dotted") +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))














#learning rate over 70 and under 100



g1 <- ggplot(data = longidata_FBover70_not100, aes(x = age, y = learningrate, group = subject))
g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))
g1 <- g1 + xlab("Age")
g1 <- g1 + ylab("Learning Performance")
g1 <- g1 + xlim(8,30)
g1 <- g1 + ylim(60,100)
print (g1)


RandomIntercept<-lme(learningrate~1, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_lin<-lme(learningrate~poly(age,1), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
RandomInterceptAge_quad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
#RandomInterceptAge_cub<-lme(learningrate~poly(age,3), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)

#anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)


anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad)

RandomInterceptAge_quad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$longidata_FBover70_not100.predvalue<-predict(RandomInterceptAge_quad, longidata_FBover70_not100, level=0)
longidata_FBover70_not100.designmat<-model.matrix(eval(eval(RandomInterceptAge_quad$call$fixed)[-2]), longidata_FBover70_not100[-3]) 
longidata_FBover70_not100$SDvalue<-sqrt(diag(longidata_FBover70_not100.designmat %*% RandomInterceptAge_quad$varFix %*% t(longidata_FBover70_not100.designmat)))
longidata_FBover70_not100$lowerCIvalue<-(longidata_FBover70_not100$longidata_FBover70_not100.predvalue-(1.96*longidata_FBover70_not100$SDvalue))
longidata_FBover70_not100$upperCIvalue<-(longidata_FBover70_not100$longidata_FBover70_not100.predvalue+(1.96*longidata_FBover70_not100$SDvalue)) 

g1 <- ggplot(longidata_FBover70_not100) 
g1 +
  xlab("Age")+
  xlim(8,30)+
  ylab("Learning Performance")+
  ylim(80,100)+
  geom_line(aes(x=age, y = longidata_FBover70_not100.predvalue), linetype="solid", size=1.2)+
  geom_line(aes(x=age, y = lowerCIvalue), linetype="dotted")  + 
  geom_line(aes(x = age, y=upperCIvalue), linetype="dotted") +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 18), axis.text.y = element_text (size = 18), axis.title.y = element_text (size = 18), axis.title.x = element_text (size = 18), legend.text = element_text (size = 18))




















##_______________________________________
#predicting learning rate from neural activity ENEP_CP, FB>70


#withresiduals
#ENEP_CP




#accumbens
RandomInterceptAge_quad<-lme(anataccumbensLR_ENEP_CP~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$accumbens_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_accumbens_residuals<-lme(learningrate~poly(age,2)+accumbens_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_accumbens_residuals)
summary(learningrate_agequad_accumbens_residuals)



#dorsalcaudate
RandomInterceptAge_quad<-lme(anatdorsalcaudateLR_ENEP_CP~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$dorsalcaudate_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_dorsalcaudate_residuals<-lme(learningrate~poly(age,2)+dorsalcaudate_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_dorsalcaudate_residuals)
summary(learningrate_agequad_dorsalcaudate_residuals)

#ventralcaudate
RandomInterceptAge_quad<-lme(anatventralcaudateLR_ENEP_CP~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$ventralcaudate_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_ventralcaudate_residuals<-lme(learningrate~poly(age,2)+ventralcaudate_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_ventralcaudate_residuals)
summary(learningrate_agequad_ventralcaudate_residuals)




#FBover70_not100


#accumbens
RandomInterceptAge_quad<-lme(anataccumbensLR_ENEP_CP~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$accumbens_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_accumbens_residuals<-lme(learningrate~poly(age,2)+accumbens_ENEP_CP_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_accumbens_residuals)
summary(learningrate_agequad_accumbens_residuals)



#dorsalcaudate
RandomInterceptAge_quad<-lme(anatdorsalcaudateLR_ENEP_CP~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$dorsalcaudate_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_dorsalcaudate_residuals<-lme(learningrate~poly(age,2)+dorsalcaudate_ENEP_CP_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_dorsalcaudate_residuals)
summary(learningrate_agequad_dorsalcaudate_residuals)

#ventralcaudate
RandomInterceptAge_quad<-lme(anatventralcaudateLR_ENEP_CP~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$ventralcaudate_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_ventralcaudate_residuals<-lme(learningrate~poly(age,2)+ventralcaudate_ENEP_CP_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_ventralcaudate_residuals)
summary(learningrate_agequad_ventralcaudate_residuals)















#EP_EN

#accumbens
RandomInterceptAge_quad<-lme(anataccumbensLR_EP_EN~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$accumbens_EP_EN_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_accumbens_residuals<-lme(learningrate~poly(age,2)+accumbens_EP_EN_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_accumbens_residuals)
summary(learningrate_agequad_accumbens_residuals)


#dorsalcaudate
RandomInterceptAge_quad<-lme(anatdorsalcaudateLR_EP_EN~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$dorsalcaudate_EP_EN_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_dorsalcaudate_residuals<-lme(learningrate~poly(age,2)+dorsalcaudate_EP_EN_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_dorsalcaudate_residuals)
summary(learningrate_agequad_dorsalcaudate_residuals)


#ventralcaudate
RandomInterceptAge_quad<-lme(anatventralcaudateLR_EP_EN~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$ventralcaudate_EP_EN_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_ventralcaudate_residuals<-lme(learningrate~poly(age,2)+ventralcaudate_EP_EN_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_ventralcaudate_residuals)
summary(learningrate_agequad_ventralcaudate_residuals)





















#FBover70_not100


#accumbens
RandomInterceptAge_quad<-lme(anataccumbensLR_EP_EN~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$accumbens_EP_EN_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_accumbens_residuals<-lme(learningrate~poly(age,2)+accumbens_EP_EN_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_accumbens_residuals)
summary(learningrate_agequad_accumbens_residuals)


#dorsalcaudate
RandomInterceptAge_quad<-lme(anatdorsalcaudateLR_EP_EN~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$dorsalcaudate_EP_EN_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_dorsalcaudate_residuals<-lme(learningrate~poly(age,2)+dorsalcaudate_EP_EN_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_dorsalcaudate_residuals)
summary(learningrate_agequad_dorsalcaudate_residuals)


#ventralcaudate
RandomInterceptAge_quad<-lme(anatventralcaudateLR_EP_EN~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$ventralcaudate_EP_EN_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_ventralcaudate_residuals<-lme(learningrate~poly(age,2)+ventralcaudate_EP_EN_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_ventralcaudate_residuals)
summary(learningrate_agequad_ventralcaudate_residuals)
































































#CORTICAL


##_______________________________________
#predicting learning rate from neural activity ENEP_CP, FB>70


#withresiduals
#ENEP_CP




#MFG
RandomInterceptAge_quad<-lme(anatMFG_ENEP_CP~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$MFG_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_MFG_residuals<-lme(learningrate~poly(age,2)+MFG_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_MFG_residuals)
summary(learningrate_agequad_MFG_residuals)



#SMA
RandomInterceptAge_quad<-lme(anatSMA_ENEP_CP~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$SMA_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_SMA_residuals<-lme(learningrate~poly(age,2)+SMA_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_SMA_residuals)
summary(learningrate_agequad_SMA_residuals)

#SPL
RandomInterceptAge_quad<-lme(anatSPL_ENEP_CP~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$SPL_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_SPL_residuals<-lme(learningrate~poly(age,2)+SPL_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_SPL_residuals)
summary(learningrate_agequad_SPL_residuals)


#ACC
RandomIntercept<-lme(anatACC_ENEP_CP~1, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$ACC_ENEP_CP_residuals<-as.numeric(residuals(RandomIntercept, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_ACC_residuals<-lme(learningrate~poly(age,2)+ACC_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_ACC_residuals)
summary(learningrate_agequad_ACC_residuals)



#FBover70_not100


#MFG
RandomInterceptAge_quad<-lme(anatMFG_ENEP_CP~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$MFG_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_MFG_residuals<-lme(learningrate~poly(age,2)+MFG_ENEP_CP_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_MFG_residuals)
summary(learningrate_agequad_MFG_residuals)



#SMA
RandomInterceptAge_quad<-lme(anatSMA_ENEP_CP~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$SMA_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_SMA_residuals<-lme(learningrate~poly(age,2)+SMA_ENEP_CP_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_SMA_residuals)
summary(learningrate_agequad_SMA_residuals)

#SPL
RandomInterceptAge_quad<-lme(anatSPL_ENEP_CP~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$SPL_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_SPL_residuals<-lme(learningrate~poly(age,2)+SPL_ENEP_CP_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_SPL_residuals)
summary(learningrate_agequad_SPL_residuals)



#ACC
RandomIntercept<-lme(anatACC_ENEP_CP~1, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70_not100$ACC_ENEP_CP_residuals<-as.numeric(residuals(RandomIntercept, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70_not100,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_ACC_residuals<-lme(learningrate~poly(age,2)+ACC_ENEP_CP_residuals, data=longidata_FBover70_not100,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_ACC_residuals)
summary(learningrate_agequad_ACC_residuals)























































##Scatter learning rate en striatum residuals


#dorsalcaudate
RandomInterceptAge_quad<-lme(anatdorsalcaudateLR_ENEP_CP~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$dorsalcaudate_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_dorsalcaudate_residuals<-lme(learningrate~poly(age,2)+dorsalcaudate_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_dorsalcaudate_residuals)
summary(learningrate_agequad_dorsalcaudate_residuals)



RandomInterceptAge_quad_learningrate<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$learningrate_agequad_residuals<-as.numeric(residuals(RandomInterceptAge_quad_learningrate, type = "pearson"))
g1 <- ggplot(data = longidata_FBover70, aes(x = dorsalcaudate_ENEP_CP_residuals, y = learningrate_agequad_residuals, group = subject))
#g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 11), axis.text.y = element_text (size = 11), axis.title.y = element_text (size = 11), axis.title.x = element_text (size = 11), legend.text = element_text (size = 11))
g1 <- g1 + xlab("Dorsal caudate (res).")
g1 <- g1 + ylab("Learning rate (res).")
#g1 <- g1 + xlim(8,30)
#g1 <- g1 + ylim(60,100)
print (g1)










#ventralcaudate
RandomInterceptAge_quad<-lme(anatventralcaudateLR_ENEP_CP~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$ventralcaudate_ENEP_CP_residuals<-as.numeric(residuals(RandomInterceptAge_quad, type = "pearson"))
learningrate_agequad<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method = "M",na.action=na.exclude)
learningrate_agequad_ventralcaudate_residuals<-lme(learningrate~poly(age,2)+ventralcaudate_ENEP_CP_residuals, data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
anova(learningrate_agequad,learningrate_agequad_ventralcaudate_residuals)
summary(learningrate_agequad_ventralcaudate_residuals)


RandomInterceptAge_quad_learningrate<-lme(learningrate~poly(age,2), data=longidata_FBover70,random=~1|subject,method="M",na.action=na.exclude)
longidata_FBover70$learningrate_agequad_residuals<-as.numeric(residuals(RandomInterceptAge_quad_learningrate, type = "pearson"))
g1 <- ggplot(data = longidata_FBover70, aes(x = ventralcaudate_ENEP_CP_residuals, y = learningrate_agequad_residuals, group = subject))
#g1 <- g1 + geom_line(aes (colour = factor (sex))) 
g1 <- g1 + geom_point (aes (colour = factor (sex))) 
g1 <- g1 +  scale_color_manual(values=c("black","black"), labels=c("female", "male"))
g1 <- g1 + theme_bw() 
g1 <- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text (size = 11), axis.text.y = element_text (size = 11), axis.title.y = element_text (size = 11), axis.title.x = element_text (size = 11), legend.text = element_text (size = 11))
g1 <- g1 + xlab("Ventral caudate (res).")
g1 <- g1 + ylab("Learning rate (res).")
#g1 <- g1 + xlim(8,30)
#g1 <- g1 + ylim(60,100)
print (g1)
