library(readxl)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)
library(scales)
library(RColorBrewer)
install.packages('openxlsx')
library(openxlsx)

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
site<-table(site)
site<-data.frame(site)
names(site) <- c("work", "y/n","Freq")

# 시각화 + 비율
site$ratio<-(site$Freq/sum(site$Freq))
site$ratio<-round(site$ratio*100, digits = 1)

ggplot(site, aes(x=work, y=Freq, fill=`y/n`))+
  geom_bar(stat='identity', position='dodge')+
  xlab("사이트 운영 유무")+
  ylab("count")+
  ggtitle("사이트 운영 비율")+
  geom_text(data=site, aes(label=paste(ratio,'%')), position=position_dodge(width=1), vjust=-0.5)

## 2. 준비 자격증과 채용시 자격증
certificate <- data.frame(survey$certificate)
View(certificate)

# , 구분
cer <- strsplit(certificate$survey.certificate, ", ")
cer_a <- unlist(cer)
cer_a <- sort(table(cer_a), decreasing = T)
cer_a <- data.frame(cer_a)
names(cer_a) <- c("자격증", "Freq")
View(cer_a)

# 채용시 자격증
cer_b <- data.frame(survey2$s,survey2$v,survey2$w,survey2$x,survey2$y,survey2$t,survey2$u)
View(cer_b)

# NA 제거
cer_b <-table(str_replace_all(c(cer_b$survey2.s, cer_b$survey2.v, cer_b$survey2.w, cer_b$survey2.x,
                                cer_b$survey2.y, cer_b$survey2.t, cer_b$survey2.u)," ",""))
cer_b <-data.frame(cer_b)
names(cer_b) <- c("자격증", "Freq")

# 시각화 + 비율
cer_a$ratio<-(cer_a$Freq/sum(cer_a$Freq))
cer_a$ratio<-round(cer_a$ratio*100, digits = 1)
fillPalette <-c("#1E1350","#85130C","#189B9B","#F7DA09","#6E6E6E",
                 "#6E6E6E","#6E6E6E","#6E6E6E","#6E6E6E","#6E6E6E")

ggplot(cer_a, aes(x=`자격증`, y=Freq, fill=`자격증`)) +geom_bar(stat='identity')+
  scale_x_discrete(limits=c("정보처리기사", "빅데이터분석기사","ADsP", "SQLD", "ADP", 
                            "사회조사분석사 2급", "AWS", "감리사", "정보관기술사", "정보처리산업기사"))+
  geom_text(data=cer_a, aes(x=`자격증`, y=Freq, label=paste(ratio,'%')), vjust=-0.5)+
  scale_fill_manual(values = fillPalette)+
  xlab("자격증")+
  ylab("count")+
  ggtitle("준비 또는 취득 자격증")


cer_b$ratio<-(cer_b$Freq/sum(cer_b$Freq))
cer_b$ratio<-round(cer_b$ratio*100, digits = 1)
fillPalette2 <-c("#1E1350","#6E6E6E","#6E6E6E","#F7DA09","#85130C",
                "#189B9B","#E8BCEA")

ggplot(cer_b, aes(x=`자격증`,y=Freq, fill=`자격증`))+geom_bar(stat='identity')+
  scale_x_discrete(limits=c("정보처리기사", "빅데이터분석기사","ADsP", "SQLD", "클라우드관련자격증", "OCP", "CCNA"))+
  geom_text(data=cer_b, aes(x=`자격증`, y=Freq, label=paste(ratio,'%')), vjust=-0.5)+
  scale_fill_manual(values = fillPalette2)+
  xlab("자격증")+
  ylab("count")+
  ggtitle("채용시 필요 자격증")

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

# 시각화 + 비율
lan$ratio<-(lan$Freq/sum(lan$Freq))
lan$ratio<-round(lan$ratio*100, digits = 1)
color_lan <-c("#900C3F","#bb6879","#ce8d99","#FF5733","#DAF7A6",
                 "#6E6E6E","#DAF7A6","#6E6E6E","#6E6E6E","#6E6E6E","#6E6E6E",
              "#6E6E6E","#6E6E6E","#6E6E6E","#6E6E6E","#6E6E6E","#6E6E6E","#6E6E6E")

ggplot(lan, aes(x=lan, y=Freq, fill=lan))+geom_bar(stat='identity')+
  scale_x_discrete(limits=c("Python", "Java","Javascript", "HTML/CSS", "SQL", "R", "C", "C++",
                            "NOSQL", "C#", "Go", "Kotlin", "Rust", "Scala", "PHP", "ProgreSQL",
                            "Swift", "Typescript"))+
  geom_text(data=lan, aes(x=lan, y=Freq, label=paste(ratio,'%')), vjust=-0.5)+
  scale_fill_manual(values = color_lan)+
  xlab("프로그래밍 언어")+
  ylab("count")+
  ggtitle("준비 언어")

lan2$ratio<-(lan2$Freq/sum(lan2$Freq))
lan2$ratio<-round(lan2$ratio*100, digits = 0)
color_lan2 <-c("#FF5733", "#DAF7A6","#900C3F")

ggplot(lan2, aes(x=Var1,y=Freq, fill=Var1))+geom_bar(stat='identity')+
  scale_x_discrete(limits=c("Python", "JAVA","Javascript,CSS,HTML"))+
  geom_text(data=lan2, aes(x=Var1, y=Freq, label=paste(ratio,'%')), vjust=-0.5)+
  scale_fill_manual(values = color_lan2)+
  xlab("개발 언어")+
  ylab("count")+
  ggtitle("채용시 필요 역량 언어")

## 3-2. 구직자 희망직무
task <- data.frame(survey$task_1, survey$task_2)
View(task)
task_test <- data.frame(survey$task_2)
View(task_test)
task_test<- gsub('빅데이터 관련 또는 AI 전문가', '빅데이터 또는 AI 전문가', task_test$survey.task_2)
task_test <-data.frame(task_test)
task <- cbind(task$survey.task_1, task_test$task_test)
task<- str_replace_all(task, ' ','')
task <- table(task)
task <- data.frame(task)

# 시각화 + 비율
task$ratio<-(task$Freq/sum(task$Freq))
task$ratio<-round(task$ratio*100, digits = 1)

ggplot(task, aes(x=task, y=Freq, fill=task))+geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=50, hjust=1))+
  scale_x_discrete(limits=c("빅데이터또는AI전문가", "웹백엔드개발자", "웹프론트엔드개발자","DBA", "모바일앱개발자", 
                            "시스템프로그래머", "없음", "시스템엔지니어","시스템보안", "데이터분석가", "고민중",
                            "테스트엔지니어", "모바일게임개발자", "네트워크엔지니어", "영상처리", "풀스택개발자",
                            "임베디드", "컨설턴트","펌웨어엔지니어","퍼블릭클라우드환경에서데이터엔지니어",
                            "온프레미스환경에서데이터엔지니어"))+
  geom_text(data=task, aes(x=task, y=Freq, label=paste(ratio,'%')), vjust=-0.5)+
  scale_fill_manual(values=c("#81c147", "#6E6E6E", "#97938d","#81c147", "#415c28", "#415c28",
                             "#6a8518", "#2e453b", "#2e453b","#2e453b", "#6E6E6E", "#6E6E6E",
                             "#b0b0b0", "#83dcb7", "#bfff00","#6E6E6E", "#6E6E6E", "#97938d",
                             "#b0b0b0", "#97938d", "#6E6E6E"))+
  xlab("직무")+
  ylab("count")+
  ggtitle("희망 직무")

## 3-2. 현직자 채용 직무
task2 <- data.frame(survey2$g, survey2$h, survey2$i, survey2$j, survey2$k, survey2$l)
View(task2)
#공백 제거
task2 <-table(str_replace_all(c(survey2$g, survey2$h, survey2$i, survey2$j, survey2$k, survey2$l)," ",""))
#다시 데이터 프레임화
task2 <-data.frame(task2)
#시각화 + 비율
task2$ratio<-(task2$Freq/sum(task2$Freq))
task2$ratio<-round(task2$ratio*100, digits = 1)

ggplot(task2,aes(x=Var1,y=Freq, fill=Var1))+geom_bar(stat='identity')+
  scale_x_discrete(limits=c("빅데이터", "인공지능개발자","백엔드개발자", "프론트엔드개발자", "클라우드개발자",
                            "클라우드엔지니어"))+
  geom_text(data=task2, aes(x=Var1, y=Freq, label=paste(ratio,'%')), vjust=-0.5)+
  scale_fill_manual(values=c("#83dcb7", "#6a8518", "#6a8518","#6E6E6E", "#6E6E6E", "#bfff00"))+
  xlab("직무")+
  ylab("count")+
  ggtitle("채용 직무")

## 4. 설문 국비교육/부트캠프
ITcamp <-data.frame(survey$ITcamp)

ITcamp<-ifelse(ITcamp == "아니오 (14번 이동)", "아니요","예")
ITcamp<-table(ITcamp)
ITcamp<-data.frame(ITcamp)
names(ITcamp) <- c("y/n", "Freq", "ratio")

# 시각화 + 비율
ITcamp$ratio<-(ITcamp$Freq/sum(ITcamp$Freq))
ITcamp$ratio<-round(ITcamp$ratio*100, digits = 1)

ggplot(ITcamp,aes(x="", y=Freq, fill=`y/n`))+geom_bar(width = 10,stat = 'identity')+
  geom_text(aes(label=paste(ratio,'%')), vjust=-2)+
  coord_polar("y")+
  xlab("")+
  ylab("")+
  ggtitle("IT 교육과정 참여 여부")

## 4. 교육과정 참여
ITcamp2 <- data.frame(survey2$d)
ITcamp2 <-table(na.omit(survey2$d))
ITcamp2 <-data.frame(ITcamp2)
names(ITcamp2) <- c("y/n", "Freq", "ratio")

#시각화 + 비율
ITcamp2$ratio<-(ITcamp2$Freq/sum(ITcamp2$Freq))
ITcamp2$ratio<-round(ITcamp2$ratio*100, digits = 1)

ggplot(ITcamp2,aes(x="",y=Freq, fill=`y/n`))+geom_bar(width = 10,stat = 'identity')+
  geom_text(aes(x=`y/n`, y=Freq, label=paste(ratio,'%')), hjust=1)+
  coord_polar("y")+
  xlab("")+
  ylab("")+
  ggtitle("IT 교육과정 채용 여부")

## 엑셀파일 만들기
write.xlsx(x= survey, file="00. 구직자 설문조사.xlsx", rowNames=TRUE)
write.xlsx(x= survey2, file="00. 현직자 설문조사.xlsx", rowNames=TRUE)
write.xlsx(x= site, file="01. 싸이트 운영 비율.xlsx", rowNames=TRUE)
write.xlsx(x= cer_a, file="02. 준비 자격증.xlsx", rowNames=TRUE)
write.xlsx(x= cer_b, file="02. 채용 자격증.xlsx", rowNames=TRUE)
write.xlsx(x= lan, file="03. 준비 언어.xlsx", rowNames=TRUE)
write.xlsx(x= lan2, file="03. 채용 언어.xlsx", rowNames=TRUE)
write.xlsx(x= task, file="04. 구직자 희망직무.xlsx", rowNames=TRUE)
write.xlsx(x= task2, file="04. 현직자 채용직무.xlsx", rowNames=TRUE)
write.xlsx(x= ITcamp, file="05. 구직자 IT캠프 참여.xlsx", rowNames=TRUE)
write.xlsx(x= ITcamp2, file="05. 현직자 IT캠프 참여.xlsx", rowNames=TRUE)

## 색 고정
palette <-c("정보처리기사"="red", "빅데이터분석기사"="yellow","ADsP"="green", "SQLD"="violet", "ADP"="gray", 
            "사회조사분석사 2급"="gray", "AWS"="gray", "감리사"="gray", "정보관기술사"="gray", "정보처리산업기사"="gray",
            "클라우드관련자격증"="gray", "OCP"="gray", "CCNA"="gray")
