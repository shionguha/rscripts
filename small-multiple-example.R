#packages here

require(ggplot2)
require(reshape2)

#do necessary imports here. This is an example. Replace with your own
jcmcqpanels <- read.csv("C:/Users/Shion/Downloads/jcmc-analysis-qpanels.csv")

#snippet which creates pretty facetted ggplot 2  graphs from the probability outputs of a multinomial logistic regression
# this could be construed as an example of Tufte's small multiple principle
# the output example is small-multiple-example-plot.png

#melt the data
molten = melt(jcmcqpanels,id="Variable")

#oddly detailed graph
ggplot(data=molten, aes(x=variable,y=value)) + geom_bar(stat="identity") +facet_wrap(~Variable) + ggtitle("Multinomial Regression: Qualtrics Panels Sample") + theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) + theme(axis.text.x = element_text(angle=90))

#minimalist graph
ggplot(data=molten_qpanels, aes(x=variable,y=value,fill=variable)) + geom_bar(stat="identity") + facet_wrap(~Variable) + geom_text(aes(label=value,size=8))