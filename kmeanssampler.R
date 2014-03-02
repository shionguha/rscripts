#loadinglibraries
library(mclust)

#randomly simulating gaussian mixture model
simdata = sim(modelName = "V",parameters = list(pro = c(0.95, 0.05),mean = c(0, 0),variance = list(modelName = "V",d = 1,G = 2,sigmasq = c(0, 36))),n = 10)

#fitting k means
fitk<-kmeans(simdata,3)

#plotting k means
clusplot(simdata, fitk$cluster, color=TRUE, shade=TRUE,labels=2, lines=0,main="k-means")
