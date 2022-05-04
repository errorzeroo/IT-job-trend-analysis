#####################   worknet    #######################

# 검색 설정 
# 데이터네트워크 및 시스템 운영 / 소프트웨어 /  컴퓨터하드웨어통신공학 /  컴퓨터시스템 + 50개씩 보기
#url <- "https://www.work.go.kr/empInfo/empInfoSrch/list/dtlEmpSrchList.do?careerTo=&keywordJobCd=&occupation=025%2C024%2C022%2C023&templateInfo=&shsyWorkSecd=&rot2WorkYn=&payGbn=&resultCnt=50&keywordJobCont=&cert=&cloDateStdt=&moreCon=&minPay=&codeDepth2Info=11000&isChkLocCall=&sortFieldInfo=DATE&major=&resrDutyExcYn=&eodwYn=&sortField=DATE&staArea=&sortOrderBy=DESC&keyword=&termSearchGbn=all&carrEssYns=&benefitSrchAndOr=O&disableEmpHopeGbn=&webIsOut=job&actServExcYn=&maxPay=&keywordStaAreaNm=&emailApplyYn=&listCookieInfo=DTL&pageCode=&codeDepth1Info=11000&keywordEtcYn=&publDutyExcYn=&keywordJobCdSeqNo=&exJobsCd=&templateDepthNmInfo=&computerPreferential=&regDateStdt=&employGbn=&empTpGbcd=&region=&infaYn=&resultCntInfo=50&siteClcd=all&cloDateEndt=&sortOrderByInfo=DESC&currntPageNo=1&indArea=&careerTypes=&searchOn=Y&tlmgYn=&subEmpHopeYn=&academicGbn=&templateDepthNoInfo=&foriegn=&mealOfferClcd=&station=&moerButtonYn=Y&holidayGbn=&enterPriseGbn=all&academicGbnoEdu=noEdu&cloTermSearchGbn=all&keywordWantedTitle=&stationNm=&benefitGbn=&keywordFlag=&essCertChk=&isEmptyHeader=&depth2SelCode=&_csrf=ad005d53-68ee-4d01-a50f-3db37bf78908&keywordBusiNm=&preferentialGbn=&rot3WorkYn=&pfMatterPreferential=&regDateEndt=&staAreaLineInfo1=11000&staAreaLineInfo2=1&pageIndex=1&termContractMmcnt=&careerFrom=&laborHrShortYn=#viewSPL"

#####################   package laod    #######################

#패키지 로드
library(remotes)
library(multilinguer)
library(KoNLP)
library(dplyr)
library(rvest)
useNIADic()


#####################   url detail    #######################

#url 앞부분
url_p<- "https://www.work.go.kr/empInfo/empInfoSrch/list/dtlEmpSrchList.do?careerTo=&keywordJobCd=&occupation=025%2C024%2C022%2C023&templateInfo=&shsyWorkSecd=&rot2WorkYn=&payGbn=&resultCnt=50&keywordJobCont=&cert=&cloDateStdt=&moreCon=&minPay=&codeDepth2Info=11000&isChkLocCall=&sortFieldInfo=DATE&major=&resrDutyExcYn=&eodwYn=&sortField=DATE&staArea=&sortOrderBy=DESC&keyword=&termSearchGbn=all&carrEssYns=&benefitSrchAndOr=O&disableEmpHopeGbn=&webIsOut=job&actServExcYn=&maxPay=&keywordStaAreaNm=&emailApplyYn=&listCookieInfo=DTL&pageCode=&codeDepth1Info=11000&keywordEtcYn=&publDutyExcYn=&keywordJobCdSeqNo=&exJobsCd=&templateDepthNmInfo=&computerPreferential=&regDateStdt=&employGbn=&empTpGbcd=&region=&infaYn=&resultCntInfo=50&siteClcd=all&cloDateEndt=&sortOrderByInfo=DESC&currntPageNo=1&indArea=&careerTypes=&searchOn=Y&tlmgYn=&subEmpHopeYn=&academicGbn=&templateDepthNoInfo=&foriegn=&mealOfferClcd=&station=&moerButtonYn=Y&holidayGbn=&enterPriseGbn=all&academicGbnoEdu=noEdu&cloTermSearchGbn=all&keywordWantedTitle=&stationNm=&benefitGbn=&keywordFlag=&essCertChk=&isEmptyHeader=&depth2SelCode=&_csrf=ad005d53-68ee-4d01-a50f-3db37bf78908&keywordBusiNm=&preferentialGbn=&rot3WorkYn=&pfMatterPreferential=&regDateEndt=&staAreaLineInfo1=11000&staAreaLineInfo2=1&pageIndex="

#url 뒷부분
tail<-"&termContractMmcnt=&careerFrom=&laborHrShortYn=#viewSPL"

#url_p2 앞부분 (#...숫자로 끝나는 경우)
url_p2<-'https://www.work.go.kr/empInfo/empInfoSrch/detail/retrivePriEmpDtlView.do?searchInfoType=CIN&iorgGbcd=CIN&wantedAuthNo='



#####################   전체 페이지    #######################
 

# page set (page 개수)
page.start <- 1
page.end <- 45


##  title_total, link_total 생성
link_total=NULL
title_total=NULL

for(p in page.start:page.end){
  #페이지 URL설정
  
  url <- paste(url_p, p, tail, sep="")
  html <- read_html(url)
  
  ## 전체 블럭
  cell <- html_nodes(html, ".board-list tbody")
  cell 
  
  ## link
  link <- html_nodes(cell, "tr td .cp-info-in a") %>%  html_attr('href')
  link_total <- c(link_total, link)
  
  # title
  title <- html_nodes(cell, ".cp-info-in a") %>% html_text()
  title <- gsub("[|\r|\n|\t]", '', title)
  title <- ifelse(title=='', NA, title)
  title_total <- c(title_total, title)
  
}



#df : title, link 
df <- data.frame(title_total, link_total)

#link 정제
df$link_total<-ifelse(nchar(df$link_total)<38, NA, ifelse(substring(df$link_total,1,1)=='/', paste('https://www.work.go.kr',df$link_total, sep=''), paste(url_p2, substring(df$link_total,26), sep='')))
df <- na.omit(df) #na제거
View(df) #1140 row



#####################   세부 페이지    #######################

#####################   1~66개  #######################

## link 들어가서 페이지 크롤링 부분
careers.1=NULL
careers.2=NULL
careers.3=NULL
careers.4=NULL
careers.5=NULL
careers.6=NULL


for(i in 1:66){
  full_link <- df$link[i]
  
  #page
  html_2 <- read_html(full_link)
  page <- html_nodes(html_2, "#contents .careers .careers-area")
  
  #자격요건, 모집요강
  careers_table <- html_nodes(html_2, '.careers-table table tbody td') %>% html_text()
  
  Sys.setlocale("LC_ALL", "English")  #인코딩언어변경
  careers_table_2 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(5) > table') %>%
    html_table(fill=TRUE)
  careers_table_3 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(6) > table') %>% 
    html_table(fill=TRUE)
  careers_table_4 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(8) > table') %>% 
    html_table(fill=TRUE)
  careers_table_5 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(11) > table') %>% 
    html_table(fill=TRUE)
  careers_table_6 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(14) > table') %>%
    html_table(fill=TRUE)
  Sys.setlocale("LC_ALL", "Korean")  #다시 한글로 인코딩언어 변경
  
  
  #print(careers_table[,1])
  #print(careers_table_2$경력조건)
  #print(careers_table_3$직종키워드)
  #print(careers_table_4$임금조건)
  #print(careers_table_5$접수마감일)
  #print(careers_table_6$전공)
  
  # 테이블 저장
  careers.1 <- rbind(careers.1, careers_table)
  careers.2 <- rbind(careers.2, careers_table_2)
  careers.3 <- rbind(careers.3, careers_table_3)
  careers.4 <- rbind(careers.4, careers_table_4)
  careers.5 <- rbind(careers.5, careers_table_5)
  careers.6 <- rbind(careers.6, careers_table_6)
}

#아래에서 확인
View(df)
View(careers.1)  
View(careers.2) 
View(careers.3)
View(careers.4)
View(careers.5)
View(careers.6)





##################     67~       ##############
## 세부 페이지에서 careers.1 ~ careers.6 까지 자격요건 등 세부사항 저장
for(i in 67:nrow(df)){
  full_link <- df$link[i]
  
  #page
  html_2 <- read_html(full_link)
  page <- html_nodes(html_2, "#contents .careers .careers-area")
  
  #자격요건, 모집요강
  careers_table <- html_nodes(html_2, '.careers-table table tbody td') %>% html_text()
  
  Sys.setlocale("LC_ALL", "English")  #인코딩언어변경
  careers_table_2 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(5) > table') %>%
    html_table(fill=TRUE)
  #contents > div > div.careers-area > div:nth-child(5) > table > tbody > tr
  careers_table_3 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(6) > table') %>% 
    html_table(fill=TRUE)
  
  careers_table_4 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(8) > table') %>% 
    html_table(fill=TRUE)
  
  careers_table_5 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(11) > table') %>% 
    html_table(fill=TRUE)
  
  careers_table_6 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(14) > table') %>%
    html_table(fill=TRUE)
  Sys.setlocale("LC_ALL", "Korean")  #다시 한글로 인코딩언어 변경
  
  
  #print(careers_table[,1])
  #print(careers_table_2$경력조건)
  #print(careers_table_3$직종키워드)
  #print(careers_table_4$임금조건)
  #print(careers_table_5$접수마감일)
  #print(careers_table_6$전공)
  
  # 테이블 저장
  careers.1 <- rbind(careers.1, careers_table)
  careers.2 <- rbind(careers.2, careers_table_2)
  careers.3 <- rbind(careers.3, careers_table_3)
  careers.4 <- rbind(careers.4, careers_table_4)
  careers.5 <- rbind(careers.5, careers_table_5)
  careers.6 <- rbind(careers.6, careers_table_6)
}




#함수 careers  #사용X
careers<-function(df){
  for(i in 67:nrow(df)){
    full_link <- df$link[i]
    
    #page
    html_2 <- read_html(full_link)
    page <- html_nodes(html_2, "#contents .careers .careers-area")
    
    #자격요건, 모집요강
    careers_table <- html_nodes(html_2, '.careers-table table tbody td') %>% html_text()
    
    Sys.setlocale("LC_ALL", "English")  #인코딩언어변경
    careers_table_2 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(5) > table') %>%
      html_table(fill=TRUE)
    #contents > div > div.careers-area > div:nth-child(5) > table > tbody > tr
    careers_table_3 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(6) > table') %>% 
      html_table(fill=TRUE)
    
    careers_table_4 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(8) > table') %>% 
      html_table(fill=TRUE)
    
    careers_table_5 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(11) > table') %>% 
      html_table(fill=TRUE)
    
    careers_table_6 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(14) > table') %>%
      html_table(fill=TRUE)
    Sys.setlocale("LC_ALL", "Korean")  #다시 한글로 인코딩언어 변경
    
    
    # 테이블 저장
    careers.1 <- rbind(careers.1, careers_table)
    careers.2 <- rbind(careers.2, careers_table_2)
    careers.3 <- rbind(careers.3, careers_table_3)
    careers.4 <- rbind(careers.4, careers_table_4)
    careers.5 <- rbind(careers.5, careers_table_5)
    careers.6 <- rbind(careers.6, careers_table_6)
  }
}



#예외처리 시도 
# 66번째 이후부터--> pdf로 태그 각각 다름

tryCatch(
   {for(i in 1:nrow(df)){
    
    full_link <- df$link[i]
    
    #page
    html_2 <- read_html(full_link)
    page <- html_nodes(html_2, "#contents .careers .careers-area")
    
    #자격요건, 모집요강
    careers_table <- html_nodes(html_2, '.careers-table table tbody td') %>% html_text()
    
    Sys.setlocale("LC_ALL", "English")  #인코딩언어변경
    careers_table_2 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(5) > table') %>%
      html_table(fill=TRUE)
    #contents > div > div.careers-area > div:nth-child(5) > table > tbody > tr
    careers_table_3 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(6) > table') %>% 
      html_table(fill=TRUE)
    
    careers_table_4 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(8) > table') %>% 
      html_table(fill=TRUE)
    
    careers_table_5 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(11) > table') %>% 
      html_table(fill=TRUE)
    
    careers_table_6 <- html_node(html_2, '#contents > div > div.careers-area > div:nth-child(14) > table') %>%
      html_table(fill=TRUE)
    Sys.setlocale("LC_ALL", "Korean")  #다시 한글로 인코딩언어 변경
    
    # 테이블 저장
    careers.1 <- rbind(careers.1, careers_table)
    careers.2 <- rbind(careers.2, careers_table_2)
    careers.3 <- rbind(careers.3, careers_table_3)
    careers.4 <- rbind(careers.4, careers_table_4)
    careers.5 <- rbind(careers.5, careers_table_5)
    careers.6 <- rbind(careers.6, careers_table_6)
    
   }}
    ,error= function(e){ }
    
)
   


### worknet careers 합치기  ###
worknet<- cbind.data.frame(df$title_total[1:66],careers.1, careers.2, careers.3, careers.4, careers.5, careers.6)




#####################   컬럼/변수명  #######################
names(worknet)
# 컬럼 정제
worknet_01<-worknet[c("df$title_total[1:66]",7,8,9,10,21,22,23,27,28,'경력조건', '학력','근무예정지','직종키워드')]


View(worknet_01)


#회사이름 coname
#공고이름 title
#스택   stack
#자격요건 qualif
#선호조건 prefer
#경력  careers
#마감일자 end
#지역 loc
#직무 job





#####################   엑셀로 저장    #######################
install.packages("writexl")
library(writexl)
write_xlsx(x = worknet_01, path = 'worknet.xlsx')

