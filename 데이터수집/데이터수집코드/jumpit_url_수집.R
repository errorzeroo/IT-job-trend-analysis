library(rvest)
library(stringr)

#상세 페이지 id 따올 주소
url1<-"https://www.jumpit.co.kr/api/positions?page="
url3<-"&jobCategory="
url5<-"&sort=reg_dt"

#상세 페이지 id와 붙을 주소
url0<-"https://www.jumpit.co.kr/position/"

#직무 카테고리 (14: None)
z<-c(1:13,15:21) 
for (i in z){ #직무별
  #초기화
  x<-NULL
  j<-0
  
  #더이상 긁히지 않을 때까지 스크래핑
  while (TRUE){
    j<-j+1 #page
    url<-paste0(url1,j,url3,i,url5) #타겟 url 만들기
    page<-read_html(url) %>% html_text()
    loc<-str_locate_all(page,'"id') #상세페이지 추출
    loc.<-loc[[1]][,2]
    pagenum<-str_sub(page,loc.,(loc.+10))
    pagenum<-gsub("\\W","",pagenum)
    pagenum<-as.integer(str_replace_all(pagenum,"[:alpha:]",""))
    print(pagenum)
    x<-c(x,pagenum)
    if (nchar(loc)<20) #더이상 긁히는 게 없으면 종료
      break
  }
  #직무별로 저장
  name<-paste0("job_",i) 
  t<-assign(name,paste0(url0,x))
  write.csv(t,paste0("C:/R/url_",i,".csv"))
}