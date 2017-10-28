#Textmining

required<-c("tm","SnowballC","ggplot2","wordcloud")
install.packages(required)
library("tm")
library("SnowballC")
library("ggplot2")
library("wordcloud")
docs<-Corpus(DirSource("textmining"))
writeLines(as.character(docs[[30]]))
getwd()
#text formating
getTransformations()
toSpace<-content_transformer(function(x,pattern){return(gsub(pattern," ",x))})
docs<-tm_map(docs,toSpace,"-")
docs<-tm_map(docs,toSpace,":")
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,toSpace,"’")
docs<-tm_map(docs,toSpace,"‘")
docs<-tm_map(docs,toSpace,"-")
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords,stopwords("english"))
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,stemDocument)
docs<-tm_map(docs,content_transformer(gsub),pattern = "organiz",replacement = "organ")
docs<-tm_map(docs,content_transformer(gsub),pattern = "organis",replacement = "organ")
docs<-tm_map(docs,content_transformer(gsub),pattern = "andgovern",replacement = "govern")
docs<-tm_map(docs,content_transformer(gsub),pattern = "inenterpris",replacement = "enterpris")
docs<-tm_map(docs,content_transformer(gsub),pattern = "team-",replacement = "team")
dtm<-DocumentTermMatrix(docs)
dtm
inspect(dtm[1:2,1000:1005])
freq<-colSums(as.matrix(dtm))
length(freq)
ord<-order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]
dtmr<-DocumentTermMatrix(docs,control=list(wordLengths=c(4, 20),bounds = list(global = c(3,27))))
dtmr
freqr<-colSums(as.matrix(dtmr))
length(freqr)
ordr<-order(freqr,decreasing=TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]
findFreqTerms(dtmr,lowfreq=80)
findAssocs(dtmr,"project",0.6)
findAssocs(dtmr,"enterpris",0.6)
findAssocs(dtmr,"system",0.6)

#plot
wf=data.frame(term=names(freqr),occurrences=freqr)
p<-ggplot(subset(wf,freqr>100),aes(term,occurrences))
p<-p + geom_bar(stat="identity")
p<-p + theme(axis.text.x=element_text(angle=45,hjust=1))
p

#wordcloud
set.seed(42)
wordcloud(names(freqr),freqr,min.freq=70)
wordcloud(names(freqr),freqr,min.freq=70,colors=brewer.pal(6,"Dark2"))

