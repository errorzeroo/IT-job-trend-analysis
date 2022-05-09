library(stringr)
library(tidyverse)
library(tidytext)
library(KoNLP)
library(ggplot2)
library(lubridate)
library(tm)
library(topicmodels)
library(readr)
library(dplyr)
library(readxl)
library(topicmodels)

useSejongDic()

#데이터 불러오기
keyword <- iconv(final2$keyword, "utf-8","UTF-8")

# TDM
options(mc.cores=1)
keycorpus <- VCorpus(VectorSource(keyword))
tdm <- TermDocumentMatrix(keycorpus,
                          control = list(
                                         removePunctuation= T,
                                         removeNumbers = T))

                       
nTerms(tdm)

nDocs(tdm)

head(Terms(tdm))

dtm<- as.DocumentTermMatrix(tdm)

rowTotals <- apply(dtm, 1, sum)
dtm.new <- dtm[rowTotals >0, ]
dtm<- dtm.new
dtm

lda <- LDA(dtm, k=4, control = list(seed=12345))
term <- terms(lda,20)

term

summary(lda)

# 그래프
x <- posterior(lda)$terms
y <- data.frame(t(x[,apply(x,2,max)>0.03]))
z <- data.frame(type=paste("Topic",1),
                keyword=rownames(y), posterior=y[,1])

for (i in 2:6) {
  z <- rbind(z, data.frame(type=paste("Topic",i),
                           keyword=rownames(y), posterior=y[,i]))
}

ggplot(z, aes(keyword,posterior, fill=as.factor(keyword))) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip()+
  facet_wrap(~type, nrow=1) +
  theme(legend.position = "none")
