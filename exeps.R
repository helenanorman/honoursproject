
library(ggplot2)
RESULTS$ftype <- as.factor(RESULTS$ftype)
p<-ggplot(RESULTS) + geom_point(aes(treatment, exep, col=ftype), shape=21)
p<- p + scale_x_discrete(labels=c("a" = "18", "b" = "22",
                                  "c" = "25", "d"="28")) + ylab("Exceptional offspring per cross")
p<- p +   geom_point(data=RESULTS, aes(treatment, exep,col = ftype),
                     
                     stat="summary",fun="mean", size=3)
p


mod5<- glm(exep ~treatment * ftype, data=RESULTS, family= "poisson")
summary(mod5)

figmatrix5 <- expand.grid(treatment = c("a", "b", "c", "d"), ftype = c("andro", "gyno"))

#extract model estimates
figmatrix5$estimates<- predict(mod5, newdata=figmatrix5, type="response", se.fit = F)
figmatrix5$se<- predict(mod5, newdata=figmatrix5, type="response", se.fit = T)[[2]]

p<- ggplot(data=RESULTS) + geom_point(aes(treatment, exep, col=ftype), position= position_jitterdodge(  jitter.width = 0.15,
                                                                                                        dodge.width = 0.3),  shape=21)


p#add model estimates
p<- p + geom_point(data=figmatrix5, aes(treatment, estimates, col=ftype), size=4, position=position_dodge(width=0.3))
p <- p + theme_bw() + ylim(0,4)

p


#calculating 95CI for error bars
figmatrix5$est1 <- predict(mod5, newdata=figmatrix5, type="response")
figmatrix5$se1 <- predict(mod5, newdata=figmatrix5, type="response", se.fit=T)[[2]]

figmatrix5$upper <- with(figmatrix5, est1+se1)
figmatrix5$lower <- with(figmatrix5, est1-se1)


#adding error bars to figure

p<- p + geom_errorbar(data=figmatrix5, aes(x=treatment, ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(width=0.5))

p


p<- p + scale_x_discrete(labels=c("a" = "18", "b" = "22",
                                  "c" = "25", d="28")) + ylab("Exceptional offspring per cross")

p              




