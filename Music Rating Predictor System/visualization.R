install.packages("wordcloud")
library(wordcloud)
require(Rtools)
install.packages("dendextend")
library(dendextend)


# WordCloud for Male vs Female respondents with respect to the kind of words they use.
men=trainfeats[trainfeats$GENDER=="Male",]
women=trainfeats[trainfeats$GENDER=="Female",]

wordfreqmen=rep(0,89)
wordfreqwomen=rep(0,89)

for (i in 9:89)
{
  wordfreqmen[i]=sum(men[,i])
  wordfreqwomen[i]=sum(women[,i])
}

menwc=cbind(colnames(men)[1:89],as.numeric(wordfreqmen))
womenwc=cbind(colnames(women)[1:89],as.numeric(wordfreqwomen))
menwc=menwc[order(-wordfreqmen),]
womenwc=womenwc[order(-wordfreqwomen),]


wordcloud(menwc[,1],as.numeric(menwc[,2]),random.order=FALSE,colors=brewer.pal(8, "Dark2"))  
wordcloud(womenwc[,1],as.numeric(womenwc[,2]),random.order=FALSE,colors=brewer.pal(8, "Dark2")) 


# Barplot for words used to describe the artist and the average track ratinngs
wordratings=rep(0,89)
for(i in 9:89)
{
  temp=trainfeats[trainfeats[,i]==1,]
  wordratings[i]=mean(as.numeric(temp$Rating))
  
}

wr=cbind(colnames(trainfeats)[1:89],as.numeric(wordratings))
wr=wr[order(-wordratings),]
barplot(height=as.numeric(wr[1:81,2]),space=c(0,2),names.arg=wr[1:81,1],beside=TRUE,horiz=TRUE,cex.names=0.5, las=1, xlab="Rating")



# Track ratings by Age
wordcloud(wr[,1],as.numeric(wr[,2]),random.order=FALSE,colors=brewer.pal(8, "Dark2"), scale=c(2.5,.2))
ageRatings1=trainfeats[trainfeats$AGE<20,]
ageRatings2=trainfeats[trainfeats$AGE>=20 & trainfeats$AGE<30,]
ageRatings3=trainfeats[trainfeats$AGE>=30 & trainfeats$AGE<40,]
ageRatings4=trainfeats[trainfeats$AGE>=40 & trainfeats$AGE<50,]
ageRatings5=trainfeats[trainfeats$AGE>=50 & trainfeats$AGE<60,]
ageRatings6=trainfeats[trainfeats$AGE>=60,]
ageRatings=c(mean(ageRatings1$Rating), mean(ageRatings2$Rating), mean(ageRatings3$Rating), mean(ageRatings4$Rating), mean(ageRatings5$Rating), mean(ageRatings6$Rating))
names=c("Below 20","between 20 to 29","between 30 to 39","between 40 to 49","between 50 to 59","60 and above")

par(mar=c(5,7,1,1))
barplot(height=ageRatings,space=c(0,2),names.arg=names,beside=TRUE,horiz=TRUE,cex.names=0.6, las=1, xlab="Rating", xlim=c(0,100))


# Track ratings by Occupation.
aggdata <-aggregate(x=trainfeats, by=list(trainfeats$WORKING), 
                    FUN=mean, na.rm=TRUE)
aggdata=aggdata[-1,]
par(mar=c(5,16,1,1))
barplot(height=aggdata$Rating,space=c(0,2),names.arg=aggdata[,1],beside=TRUE,horiz=TRUE,xlim=c(0,100),cex.names=0.6, las=1, xlab="Rating")



# Word Similarity Hierarchy by Artist Word Co-Occurance
temp=words[,6:86]
tr=t(temp)
mysample <- tr[sample(1:nrow(tr), 30, replace=FALSE),]
hc=as.dendrogram(hclust(dist(mysample),method="complete"))
d1=color_branches(hc,k=2, col = c("red","blue"))
plot(d1,horiz=T)


# Final results tabulation
names=c("Linear Model","Linear Model by Artist","GBM","Random Forest","Random Forest by Artist")
values=c(27.6154,24.76308,25.08098,25.46859,23.87086)
barplot(values, names.arg=names, horiz=T,las=1, xlab="RMSE", col="blue")
par(cex=1.5,font=2,mar=c(2,5,2,2))


