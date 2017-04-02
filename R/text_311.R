library(tm)
library(ggplot2)
library(wordcloud)

transistor_album3 <- read.csv("C:\\path\\to\\file\\transistor_album3.csv") #configure to ASCII on import wizard
#trans <- paste(text, collapse = " ")
trans.vec <- VectorSource(transistor_album3$Lyrics)
#corpus
trans.corpus <- Corpus(trans.vec)
#remove stuff
trans.corpus <- tm_map(trans.corpus, content_transformer(tolower))
trans.corpus <- tm_map(trans.corpus, removePunctuation)
trans.corpus <- tm_map(trans.corpus, removeNumbers)
trans.corpus <- tm_map(trans.corpus, removeWords, stopwords("english"))
trans.corpus <- tm_map(trans.corpus, removeWords, c("dont")) 

#stemming
#trans.corpus <- tm_map(trans.corpus, stemDocument)
#trans.corpus <- tm_map(trans.corpus, stripWhitespace)

#document term matrix
dtm <- DocumentTermMatrix(trans.corpus)

#term document
tdm <- TermDocumentMatrix(trans.corpus) 

#check it out
findFreqTerms(dtm, 5)
findAssocs(dtm, "life", 0.7)

freq <- colSums(as.matrix(dtm)) 
#ord <- order(freq)
wrdfreq<- data.frame(word=names(freq), freq=freq) 
#wrdfreq_sort<-wrdfreq[order(wrdfreq$freq),]
p <- ggplot(subset(wrdfreq, freq>13), aes(word, freq)) + 
  geom_bar(stat="identity", color = "purple", fill = "purple") +  
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Frequency of Words in 'Transistor' by 311")
p


m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#wordcloud
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#############################################################################

#quantitative
#lyric density
density<-NULL
for (i in 1:nrow(transistor_album3)){
  df<-data.frame(table(unlist(strsplit(tolower(transistor_album3[i,2]), " "))))
  num<-sum(df$Freq)
  song<-transistor_album3[i,1]
  leng<-transistor_album3[i,3]
  tog<-data.frame(song, num, leng)
  density<-rbind(density, tog)
}

density$song<- factor(density$song, levels = density$song)
density$density<-density$num/density$leng


b<-ggplot(data=density, aes(x=song, y=leng, group=1)) +
  geom_bar(stat="identity", color = "purple", fill= "purple") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Length of Song", x = "Song", y="Length (sec)")
b


b2<-ggplot(data=density, aes(x=song, y=num)) +
  geom_bar(stat="identity", color = "blue", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Number of Words per Song", x = "Song", y="Frequency of Words")

b2

b3<-ggplot(data=density, aes(x=song, y=density)) +
  geom_bar(stat="identity", color = "yellow", fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Lyric Density", x = "Song", y="Density")

b3




#best interest not to use this
#plot(density$leng,col="purple", type="b",pch=19, xlab="song index", ylab="length", main = "Song Length and Lyric Density",lty=1)
#par(new=TRUE)
#plot(density$density,col="blue",, type = "b", xaxt="n",yaxt="n",xlab="",ylab="", pch=24, lty=2)
#axis(4)
#mtext("density",side=4,line=3)
#legend("topleft",col=c("purple","blue"),lty=1:2,legend=c("length","density"))
