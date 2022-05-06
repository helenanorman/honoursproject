#t tests
#22C normal 1:1 vs white eye 1:1
#prop exeptionals
dataviability$prop<- dataviability$exep/dataviability$brood
t.test(prop~type, data=dataviability)

#brood size
t.test(brood~type, data=dataviability)

#fecunidty 
t.test(dataviability1$holo2, dataviability1$w14)

#andro gyno proportions
prop<- dataviability2$andro/(dataviability2$andro + dataviability2$gyno)
t.test(prop~type, data=dataviability2)

# 22C normal 1:1 vs w14 5:1
#prop exeptionals
dataviability3$prop<- dataviability3$exep/dataviability3$brood
t.test(prop~type, data=dataviability3)

#brood size
t.test(brood~type, data=dataviability3)
chisq.test(dataviability3)

#fecunidty 
t.test(dataviability4$one, dataviability4$five)

#25C, holo2 vs w14
#prop exeptionals
dataviability5$prop<- dataviability5$exep/dataviability5$brood
t.test(prop~type, data=dataviability5)

#brood size
t.test(brood~type, data=dataviability5)

#fecunidty 
t.test(dataviability6$holo2, dataviability6$w14)


#andro gyno proportions
prop<- dataviability7$andro/(dataviability7$andro + dataviability7$gyno)
t.test(prop~type, data=dataviability7)

