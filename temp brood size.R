
library(ggplot2)
RESULTS$ftype <- as.factor(RESULTS$ftype)
p<-ggplot(RESULTS) + geom_point(aes(treatment, brood, col=ftype), shape=21)
p<- p + scale_x_discrete(labels=c("a" = "18", "b" = "22",
                                  "c" = "25")) + ylab("Brood Size")
p<- p +   geom_point(data=RESULTS, aes(treatment, brood,col = ftype),
                             
                             stat="summary",fun="mean", size=3)
p


mod4<- glm(brood ~treatment * ftype, data=RESULTS, family= "poisson")
summary(mod4)

figmatrix4 <- expand.grid(treatment = c("a", "b", "c", "d"), ftype = c("andro", "gyno"))

#extract model estimates
figmatrix4$estimates<- predict(mod4, newdata=figmatrix4, type="response", se.fit = F)
figmatrix4$se<- predict(mod4, newdata=figmatrix4, type="response", se.fit = T)[[2]]

p<- ggplot(data=RESULTS) + geom_point(aes(treatment, brood, col=ftype), position= position_jitterdodge(
  jitter.width = 0.15,
  dodge.width = 0.3),
  shape=21)


#add model estimates
p<- p + geom_point(data=figmatrix4, aes(treatment, estimates, col=ftype), size=4, position=position_dodge(width=0.3))
p <- p + theme_bw() + ylim(0,90)

p


#calculating 95CI for error bars
figmatrix4$est1 <- predict(mod4, newdata=figmatrix4, type="response")
figmatrix4$se1 <- predict(mod4, newdata=figmatrix4, type="response", se.fit=T)[[2]]

figmatrix4$upper <- with(figmatrix4, est1+se1)
figmatrix4$lower <- with(figmatrix4, est1-se1)


#adding error bars to figure

p<- p + geom_errorbar(data=figmatrix4, aes(x=treatment, ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(width=0.3))

p


p<- p + scale_x_discrete(labels=c("a" = "18", "b" = "22",
                                  "c" = "25", d="28")) + ylab("Brood size")

p              






cannibals<- lm(brood~exep, data=RESULTS)
summary(cannibals)
p<-ggplot(RESULTS) + geom_point(aes(brood, exep, col=treatment), shape=21)
p
