### survey ###

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
survey <- read_xlsx('survey_2.xlsx')
View(survey)

###########################
sur
sur <- data.frame(survey)


sur_l<-rbind(sur$job1,sur$job2, sur$language, sur$skill, sur$loc, sur$test)







#####################################
###   preprocessing function   ###

pre<-function(sul_l){
  
  sur_l<-strsplit(sur_l, split=',')
  sur_l<-unlist(sur_l)
  sur_l<- str_replace_all(sur_l, ' ','')
  
  sur_l<-gsub('생각없음', '지역무관', sur_l)
  sur_l<-gsub('잘모르거나', '지역무관', sur_l)
  sur_l<-gsub('씨언어', 'C', sur_l)
  sur_l<-gsub('빅분기', '빅데이터분석기사', sur_l)
  sur_l<-gsub('씨언어', 'C', sur_l)
  sur_l<-gsub('빅데이터관련또는AI전문가', '빅데이터또는AI전문가', sur_l)
  sur_l<-gsub('Tensorflow', 'TensorFlow', sur_l)
  sur_l<-gsub('빅데이터분석기사(취득)', '빅데이터분석기사', sur_l)
  sur_l<-gsub('Scikit-learn', 'scikit-learn', sur_l)
  sur_l<-gsub('Pytoch', 'Pytorch', sur_l)
  
  
  sur_l<-na.omit(sur_l)
  
  
}
sur_l<-pre(sul_l)

#69줄



####  save & read table   ###
write(sur_l,'./mini/sur_l.txt')
sur_key <- read.table('./mini/sur_l.txt')
head(sur_key)




table(sur_key)


########### visualizing   ############
library(wordcloud2)
sur_key_top20 <- count(sur_key,V1) %>% arrange(desc(n)) %>% head(20)
sur_key_top50<- count(sur_key,V1) %>% arrange(desc(n)) %>% head(50)



wordcloud2(sur_key_top50)
letterCloud(sur_key_top50, word = 'A', wordSize = 0.8)

wordcloud2(data=sur_key_top50, figPath='user.png',size=1)



