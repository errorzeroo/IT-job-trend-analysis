library(readxl)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)
library(scales)
library(RColorBrewer)

## 데이터 불러오기 ##
setwd("C/Bdata/mini")
survey2 <- read_excel("mini/Rows (1) (1).xlsx")
survey <- read_excel("mini/survey.xlsx")
str(survey)
View(survey)
str(survey2)
View(survey2)

# 컬럼명 변경
names(survey)<- c("sex", "age", "education", "job_state", "task_1", "task_2", "std_lan", "std_fw", "salary",
                 "company", "emp_type", "workplace", "ITcamp", "ITcamp_emp", "ITporpolio", "codingtest",
                 "certificate_q", "certificate", "language", "interview", "site")
names(survey2) <- c(letters[1:26])

## 1. IT 재직자와 싸이트운영의 상관관계
site <- data.frame(survey$job_state, survey$site)
View(site)

# 싸이트 구분하기
names(site) <- c("work", "y/n")
site$`y/n`<-ifelse(site$`y/n` == "없음", "없음", "있음")
table(site$`y/n`)


# 시각화
ggplot(data=site, aes(x=site$work, fill=`y/n`))+
  geom_bar()+
  xlab("사이트 운영 유무")+
  ggtitle("사이트 운영 비율")

## 2. 준비 자격증과 채용시 자격증
certificate <- data.frame(survey$certificate)
View(certificate)

# , 구분
cer <- strsplit(certificate$survey.certificate, ", ")
cer_a <- unlist(cer)
cer_a <- sort(table(cer_a), decreasing = T)
cer_a <- data.frame(cer_a)
names(cer_a) <- c("Var1", "Freq")
View(cer_a)

# 채용시 자격증
cer_b <- data.frame(survey2$s,survey2$v,survey2$w,survey2$x,survey2$y,survey2$t,survey2$u)
View(cer_b)

# NA 제거
cer_b <-table(str_replace_all(c(cer_b$survey2.s, cer_b$survey2.v, cer_b$survey2.w, cer_b$survey2.x,
                                cer_b$survey2.y, cer_b$survey2.t, cer_b$survey2.u)," ",""))
cer_b <-data.frame(cer_b)
# 시각화
ggplot(cer_a, aes(x=Var1, y=Freq)) +geom_col()+
  scale_x_discrete(limits=c("정보처리기사", "빅데이터분석기사","ADsP", "SQLD", "ADP", 
                            "사회조사분석사 2급", "AWS", "감리사", "정보관기술사", "정보처리산업기사"))+
  scale_fill_brewer(palette= 'green')
ggplot(cer_b, aes(x=Var1,y=Freq, fill=Var1))+geom_col()+
  scale_x_discrete(limits=c("정보처리기사", "빅데이터분석기사","ADsP", "SQLD", "클라우드관련자격증", "OCP", "CCNA"))

## 3-1. 준비언어와 개발 언어
View(survey)
lan <- survey$std_lan
View(lan)
lan <- data.frame(lan)

# 설문조사 , 구분
lan <- strsplit(lan$lan, ", ")
lan <- unlist(lan)
lan <- sort(table(lan), decreasing = T)
lan <- data.frame(lan)
View(lan)

## 3-1. 현직자 개발언어
View(survey2)
lan2 <- data.frame(survey2$m, survey2$n, survey2$o)
View(lan2)
lan2 <-table(str_replace_all(c(lan2$survey2.m, lan2$survey2.n, lan2$survey2.o)," ",""))
lan2 <- data.frame(lan2)

# 시각화
ggplot(lan, aes(x=lan, y=Freq))+geom_col()+
  scale_x_discrete(limits=c("Python", "Java","Javascript", "HTML/CSS", "SQL", "R", "C", "C++",
                            "NOSQL", "C#", "Go", "Kotlin", "Rust", "Scala", "PHP", "ProgreSQL",
                            "Swift", "Typescript"))
ggplot(lan2, aes(x=Var1,y=Freq))+geom_col()+
  scale_x_discrete(limits=c("Python", "JAVA","Javascript,CSS,HTML"))

## 3-2. 구직자 희망직무
task <- data.frame(survey$task_1, survey$task_2)
View(task)
task_test <- data.frame(survey$task_2)
View(task_test)
task_test<- gsub('빅데이터 관련 또는 AI 전문가', '빅데이터 또는 AI 전문가', task_test$survey.task_2)
task_test <-data.frame(task_test)
task <- cbind(task$survey.task_1, task_test$task_test)
task<- str_replace_all(task, ' ','')
task <- data.frame(task)

ggplot(task, aes(x=task))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_x_discrete(limits=c("빅데이터또는AI전문가", "웹백엔드개발자", "웹프론트엔드개발자","DBA", "모바일앱개발자", 
                            "시스템프로그래머", "없음", "시스템엔지니어","시스템보안", "데이터분석가", "고민중",
                            "테스트엔지니어", "모바일게임개발자", "네트워크엔지니어", "영상처리", "풀스택개발자",
                            "임베디드", "컨설턴트","펌웨어엔지니어","퍼블릭클라우드환경에서데이터엔지니어",
                            "온프레미스환경에서데이터엔지니어"))

## 3-2. 현직자 채용 직무
task2 <- data.frame(survey2$g, survey2$h, survey2$i, survey2$j, survey2$k, survey2$l)
View(task2)
#공백 제거
task2 <-table(str_replace_all(c(survey2$g, survey2$h, survey2$i, survey2$j, survey2$k, survey2$l)," ",""))
#다시 데이터 프레임화
task2 <-data.frame(task2)
#시각화
ggplot(task2,aes(x=Var1,y=Freq))+geom_col()+
  scale_x_discrete(limits=c("빅데이터", "인공지능개발자","백엔드개발자", "프론트엔드개발자", "클라우드개발자",
                            "클라우드엔지니어"))

## 4. 설문 국비교육/부트캠프
ITcamp <-data.frame(survey$ITcamp)

ITcamp<-ifelse(ITcamp == "아니오 (14번 이동)", "아니요","예")
ITcamp<-data.frame(ITcamp)

# 시각화
ggplot(ITcamp, aes(x=survey.ITcamp))+geom_bar()

## 4. 교육과정 참여
ITcamp2 <- data.frame(survey2$d)
ITcamp2 <-table(na.omit(survey2$d))
ITcamp2 <-data.frame(ITcamp2)

#시각화
ggplot(ITcamp2,aes(x=Var1,y=Freq))+geom_col()


# 색 고정
palette <-c("정보처리기사"="red", "빅데이터분석기사"="yellow","ADsP"="green", "SQLD"="violet", "ADP"="gray", 
            "사회조사분석사 2급"="gray", "AWS"="gray", "감리사"="gray", "정보관기술사"="gray", "정보처리산업기사"="gray",
            "클라우드관련자격증"="gray", "OCP"="gray", "CCNA"="gray")