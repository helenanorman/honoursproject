model<- glm(eggs~treatment, data=fecundity_stats, family=binomial)
summary(model)
cf<- summary(model)$coefficients
p<-ggplot()+geom_jitter(data=fecundity_stats, aes(x=treatment, y=eggs), 
                        width=0.2, height=0.025)

p<- p +theme_bw()
p<- p + ylim(0,1)
p
a<- plogis(cf[1])

#back transformed estaimte for b
b<- plogis(cf[1]+cf[2])
#back transformed estimate for c
c<- plogis(cf[1]+cf[3])

#back transformed estimate for d
d<- plogis(cf[1]+cf[4])

estimates<- data.frame(Treatment=c("a", "b", "c", "d"), est_eggs=c(a, b, c, d))

p<- p + geom_point(data=estimates, aes(x=Treatment, y=est_eggs), col=2, size=3, pch=16)

#finding upper and lower standard error values for SE bars (a)
aupper<- plogis(cf[1]+cf[4])
alower<- plogis(cf[1]-cf[4])
#relevelling data to find SE values for b
newdata<- fecundity_stats
newdata$treatment<- as.factor(newdata$treatment)
newdata$treatment<- relevel(newdata$treatment, ref="b")
model2<- glm(eggs~treatment, data=newdata, family=binomial)
summary(model2)
cf2<- summary(model2)$coefficients
bupper<- plogis(cf2[1]+cf2[4])
blower<- plogis(cf2[1]-cf2[4])

#relevelling data to find SE values for c
newdata2<- fecundity_stats
newdata2$treatment<- as.factor(newdata2$treatment)
newdata2$treatment<- relevel(newdata2$treatment, ref="c")
model3<- glm(eggs~treatment, data=newdata2, family=binomial)
summary(model3)
cf3<- summary(model3)$coefficients
cupper<- plogis(cf3[1]+cf3[4])
clower<- plogis(cf3[1]-cf3[4])

estimates$upper <- c(aupper, bupper, cupper)
estimates$lower <- c(alower, blower, clower)

#relevelling to find SE values for D
newdata3<- fecundity_stats
newdata3$treatment<- as.factor(newdata3$treatment)
newdata3$treatment<- relevel(newdata3$treatment, ref="d")
model4<- glm(eggs~treatment, data=newdata3, family=binomial)
summary(model4)
cf4<- summary(model4)$coefficients
dupper<- plogis(cf4[1]+cf4[4])
dlower<- plogis(cf4[1]-cf4[4])

estimates$upper <- c(aupper, bupper, cupper, dupper)
estimates$lower <- c(alower, blower, clower, dlower)

#adding standard error bars to the plot

p<- p+ geom_errorbar(data=estimates, aes(x=estimates$Treatment, ymax=upper, ymin=lower), width=0.1)

p<- p + xlab("treatment") +ylab("fecundity")

p<- p + scale_x_discrete(labels=c("a" = "18", "b" = "22", "c" = "25", "d"="28"))

p
summary(model)
anova(model)
