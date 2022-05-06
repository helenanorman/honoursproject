#anova
avgmod<- aov(prop~treatment, data=androgyno)
summary(avgmod)
p<-ggplot(androgyno) + geom_point(aes(treatment, prop), shape=21)
p<- p + scale_x_discrete(labels=c("a" = "18", "b" = "22",
                                  "c" = "25")) + ylab("Androgenic/Progeny")
p<- p +   geom_point(data=androgyno, aes(treatment, prop), stat="summary",fun="mean", size=3)
p

#or glm ?????
p<- ggplot() + geom_point(data=androgyno, aes(treatment, prop), position=position_dodge(width=0.3), shape=21)
p <- p+ theme_bw() + ylim(0,1)
p<- p + ylab("Proportion Andogenic")
p

#generalised linear model
mod1<- glm (prop ~ treatment, data=androgyno, binomial)
summary(mod1)

#put realtive intercepts into figure matrix
#to find each treatment mean
#build data frame to build figure
figmatrix <- expand.grid(treatment = c("a", "b", "c"), ftype = c("andro", "gyno"))

#extract model estimates
figmatrix$estimates<- predict(mod1, newdata=figmatrix, type="response", se.fit = F)
figmatrix$se<- predict(mod1, newdata=figmatrix, type="response", se.fit = T)[[2]]


#build base plot
p<- ggplot(data=androgyno) + geom_point(aes(treatment, prop), position= position_jitterdodge(
  jitter.width = 0.15,
  dodge.width = 0.3),
  shape=21)


#add model estimates
p<- p + geom_point(data=figmatrix, aes(treatment, estimates), size=4, position=position_dodge(width=0.3))
p <- p + theme_bw()

p


#calculating 95CI for error bars
figmatrix$est1 <- predict(mod1, newdata=figmatrix, type="response")
figmatrix$se1 <- predict(mod1, newdata=figmatrix, type="response", se.fit=T)[[2]]

figmatrix$upper <- with(figmatrix, est1+se1)
figmatrix$lower <- with(figmatrix, est1-se1)


#adding error bars to figure

p<- p + geom_errorbar(data=figmatrix, aes(x=treatment, ymin=lower, ymax=upper, col=ftype), width=.2,
                      position=position_dodge(width=0.3))

p


p<- p + scale_x_discrete(labels=c("a" = "18", "b" = "22",
                                  "c" = "25")) + ylab("Proportion exception offspring")

p                        

