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

sur_test<-sur$test
sur_lang<- sur$language




#####################################
###   preprocessing function   ###

pre<-function(sur_test){
  
  sur_test<-strsplit(sur_test, split=',')
  sur_test<-unlist(sur_test)
  sur_test<- str_replace_all(sur_test, ' ','')
  
  sur_test<-gsub('생각없음', '지역무관', sur_test)
  sur_test<-gsub('잘모르거나', '지역무관', sur_test)
  sur_test<-gsub('씨언어', 'C', sur_test)
  sur_test<-gsub('빅분기', '빅데이터분석기사', sur_test)
  sur_test<-gsub('씨언어', 'C', sur_test)
  sur_test<-gsub('빅데이터관련또는AI전문가', '빅데이터또는AI전문가', sur_test)
  sur_test<-gsub('Tensorflow', 'TensorFlow', sur_test)
  sur_test<-gsub('디이터분석준전문가', '빅데이터분석기사', sur_test)
  sur_test<-gsub('빅데이터분석기사(취득)', '빅데이터분석기사', sur_test)
  sur_test<-gsub('ADSP(취득)', 'ADsP', sur_test)
  sur_test<-gsub('sqld', 'SQLD', sur_test)
  sur_test<-gsub('Sqld', 'SQLD', sur_test)
  
  sur_test<-gsub('ADSP', 'ADsP', sur_test)
  sur_test<-gsub('adsp', 'ADsP', sur_test)
  sur_test<-gsub('SQLD(준비중)', 'SQLD', sur_test)
  sur_test<-gsub('ADsP(취득)', 'ADsP', sur_test)
  sur_test<-gsub('Adsp', 'ADsP', sur_test)
  
  sur_test<-gsub('Scikit-learn', 'scikit-learn', sur_test)
  sur_test<-gsub('Pytoch', 'Pytorch', sur_test)
  
  
  sur_test<-na.omit(sur_test)
  
  return(sur_test)
  
}


## 변수 지정 
sur_l<-pre(sul_l)
sur_test<- pre(sur_test)
sur_lang<- pre(sur_lang)




####  save & read table   ###
write(sur_l,'./mini/sur_l.txt')
sur_key <- read.table('./mini/sur_l.txt')
head(sur_key)

write(sur_test,'./mini/sur_test.txt')
sur_test<- read.table('./mini/sur_test.txt')
head(sur_test,20)

write(sur_lang,'./mini/sur_lang.txt')
sur_lang<- read.table('./mini/sur_lang.txt')
head(sur_lang,20)

table(sur_key)


########### visualizing   ############
library(wordcloud2)
sur_key_top20 <- count(sur_key,V1) %>% arrange(desc(n)) %>% head(20)
sur_key_top50<- count(sur_key,V1) %>% arrange(desc(n)) %>% head(50)

sur_test_top5 <- count(sur_test,V1) %>% arrange(desc(n)) %>% head(5)
sur_lang_top5 <- count(sur_lang,V1) %>% arrange(desc(n)) %>% head(5)
sur_lang_top8 <- count(sur_lang,V1) %>% arrange(desc(n)) %>% head(8)

sur_lang_top10 <- count(sur_lang,V1) %>% arrange(desc(n)) %>% head(10)




wordcloud2(sur_key_top50)
wordcloud2(sur_test_top20)
wordcloud2(sur_lang_top8)

wordcloud2(sur_lang_top10)


#letterCloud(sur_key_top50, word = 'A', wordSize = 0.8)






## library / setting  ##
library(ggplot2)
library(scales)
library(RColorBrewer)



###       bar chart       ###
chart <- arrange(sur_test_top5, n)$V1
ggplot(data=sur_test_top5, aes(x=V1, y=n, fill=n)) +
  ggtitle('SURVEY test Top6') +
  labs(x="Frequency", y="Keyword") +
  ylim(0,40) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit=chart) +
  geom_text(aes(label=n), hjust=-0.3)+
  scale_fill_viridis_c(option = "cividis",begin = 0.3, end=0.6, direction = -1)
#option : magma, inferno, plasma, viridis, cividis


## chart language (프로그래밍 언어)
chart <- arrange(sur_lang_top8, n)$V1

ggplot(data=sur_lang_top8, aes(x=V1, y=n, fill=n)) +
  ggtitle('Survery Programming Language Top8') +
  labs(x="Frequency", y="Keyword") +
  ylim(0,70) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit=chart) +
  geom_text(aes(label=n), hjust=-0.3)+
  scale_fill_viridis_c(option = "viridis",begin = 0.5, end=0.8, direction = -1)
#option : magma, inferno, plasma, viridis, cividis




