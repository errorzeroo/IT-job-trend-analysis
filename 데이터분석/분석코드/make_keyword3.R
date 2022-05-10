
#package loading
library(remotes)
library(multilinguer)
#library(wordcloud)
library(KoNLP)
library(dplyr)
library(stringr)
library(readxl)
useSejongDic()


### data loading ### 
total <- read_xlsx('final2.xlsx')
View(total)

final<- data.frame(total)

#key_total<- total$keyword

#pre_1<-function(key_total){
  key_total<-str_split(key_total,split=',')
  key_total<-unlist(key_total)
  key_total<-tolower(key_total)
  key_total<- str_replace_all(key_total, ' ','')
  
  return(key_total)
}

###   preprocessing   ###
#pre<-function(key_total){
  key_total<- gsub('자바', 'java', key_total)
  key_total<- gsub('java스크립트', 'javascript', key_total)
  key_total<- gsub('database','데이터베이스',key_total)
  key_total<- gsub('리액트','react',key_total)
  key_total<- gsub('스프링부트','springboot',key_total)
  key_total<- gsub('오라클','oracle',key_total)
  key_total<- gsub('mssql','ms-sql',key_total)
  key_total<- gsub('유닉스','unix',key_total)
  
  key_total<- gsub('리눅스', 'linux', key_total)
  key_total<- gsub('html5','html',key_total)
  key_total<- gsub('html코딩','html',key_total)
  key_total<- gsub('css3','css',key_total)
  key_total<- gsub('빅데이터','bigdata',key_total)
  key_total<- gsub('인공지능.ai.','ai',key_total)
  key_total<- gsub('ai.ai.','ai',key_total)
  key_total<- gsub('인공지능', 'ai', key_total)
  key_total<- gsub('파이썬', 'python', key_total)
  key_total<- gsub('deeplearning', '딥러닝', key_total)
  key_total<- gsub('deep-learning', '딥러닝', key_total)
  key_total<- gsub('머신러닝.ml.', '머신러닝', key_total)
  key_total<- gsub('machinelearning','머신러닝', key_total)
  key_total<- gsub('machine-learning','머신러닝', key_total)
  key_total<- gsub('ui·ux디자인','ui·ux', key_total)
  key_total<- gsub('bigdata','빅데이터', key_total)
  key_total<- gsub('ui·ux디자인','ui·ux', key_total)
  key_total<- gsub('sm','시스템매니지먼트', key_total)
  key_total<- gsub('ai','인공지능', key_total)
  key_total<- gsub('포토샵','photoshop', key_total)
  key_total<- gsub('pm','프로젝트매니지먼트', key_total)
  key_total<- gsub('ui·ux디자인','ui·ux', key_total)
  
  #이건 한글 없앨 때만
  key_total<- gsub('[가-힣]','',key_total)
  key_total<- gsub('\\·','',key_total)
  
  #key_total<- na.omit(key_total)
  
  return(key_total)
}



keyword2=NULL
for(i in final$keyword){
  keyword2<-rbind(keyword2,i)
}

####
temp<-str_split(keyword2,',',simplify = T)
View(temp)


keyword3=NULL
for(i in 1:15588){
  keyword3<-rbind(keyword3,pre(temp[i,]))
  print(i)
}





###################################################


colSums(keyword3=='')


keyword3<- data.frame(keyword3)

####  save & read table   ###

write_xlsx(keyword3,path='keyword3.xlsx')
write_xlsx(final4, path='final4.xlsx')
                                                              

