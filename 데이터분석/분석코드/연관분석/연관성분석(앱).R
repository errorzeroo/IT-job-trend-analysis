install.packages("tidyverse")
install.packages("tidytext")
install.packages("topicmodels")
install.packages("LDAvis")
install.packages("servr")
install.packages("LDAvisData")
library(topicmodels)
library(tidyverse)
library(tidytext)
library(tm)
library(LDAvis)
library(servr)
library(LDAvisData)
library(KoNLP)


library(dplyr)

table(final2$edu)

library(Sejong)
library(hash)
library(tau)
library(KoNLP)
library(devtools)

final2_app <- filter(final2,job == "앱")

lword <- Map(extractNoun, final2_app$keyword) # 변수에서 명사단위로 추출


length(lword)

lword <- unique(lword)
length(lword)

str(lword)
head(lword)

library(arules)

wordtran <- as(lword, "transactions")
wordtran

wordtable <- crossTable(wordtran)
wordtable

transrules <- apriori(wordtran, parameter = list(support=0.1, conf=0.01)) # 5개의 규칙을 찾음

inspect(transrules)

# 연관 단어 시각화를 위해서 자료 구조 변경
rules <- labels(transrules, ruleSep = " ")
rules

# 문자열로 묶인 연관단어를 행렬 구조 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
class(rules)

# 행단위 묶어서 matrix 반환
rulemat <- do.call("rbind", rules)
rulemat

# 연관어 시각화를 위한 igrpah 패키지
library(igraph)

relueg <- graph.edgelist(rulemat[c(14:40),], directed = F)
relueg

plot.igraph(relueg)
