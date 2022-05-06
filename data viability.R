#t tests
#22C normal 1:1 vs white eye 1:1
#prop exeptionals
dataviability$prop<- dataviability$exep/dataviability$brood
t.test(prop~type, data=dataviability)

#brood size
t.test(brood~type, data=dataviability)

#fecunidty 
t.test(dataviability$holo2, dataviability$w14)

#andro gyno proportions
prop<- dataviability$andro/(dataviability$andro + dataviability$gyno)
t.test(prop~type, data=dataviability)

# 22C normal 1:1 vs w14 5:1
#prop exeptionals
dataviability$prop<- dataviability$exep/dataviability$brood
t.test(prop~type, data=dataviability)

#brood size
t.test(brood~type, data=dataviability)
chisq.test(dataviability)

#fecunidty 
t.test(dataviability$one, dataviability$five)

#25C, holo2 vs w14
#prop exeptionals
dataviability$prop<- dataviability$exep/dataviability$brood
t.test(prop~type, data=dataviability)

#brood size
t.test(brood~type, data=dataviability)

#fecunidty 
t.test(dataviability$holo2, dataviability$w14)


#andro gyno proportions
prop<- dataviability$andro/(dataviability$andro + dataviability$gyno)
t.test(prop~type, data=dataviability)

