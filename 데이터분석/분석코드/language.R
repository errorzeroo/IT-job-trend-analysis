## 통계표 시각화 ##
library(readxl)
library(dplyr)
library(ggplot2)

lan <- read_excel("C:/Bdata/mini/table/1-3-1-8. 개발언어별 SW 전문인력.xlsx")
View(lan)

# 컬럼명 변경
names(lan) <- c("language", "2018", "2019", "2020") 

str(lan$`2018`)

# 숫자형으로 변경
lan$`2018` <- ifelse(lan$`2018` == "-", 0, lan$`2018`)
lan$`2018` <-as.numeric(lan$`2018`)

lan$`2020`<-gsub("\\*","", lan$`2020`)
lan$`2020`<-as.numeric(lan$`2020`)

# 소수점 한자리 값으로 변경
lan$`2018`<-round(lan$`2018`, 1)
lan$`2019`<-round(lan$`2019`, 1)
lan$`2020`<-round(lan$`2020`, 1)

# 행 열 교체
library(reshape2)

lan_melt<-melt(lan)
View(lan_melt)

# 시각화
ggplot(data = lan_melt, aes(x=lan_melt$language, y=lan_melt$value, group = lan_melt$variable))+
  geom_line(color=lan_melt$variable)+
  geom_point(color=lan_melt$variable, size=2)+
  xlab("개발언어")+
  ylab("연도")+
  ggtitle("개발언어별 SW 전문인력")
