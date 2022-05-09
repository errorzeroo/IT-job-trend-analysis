# SQL 코드
install.packages("RMySQL")

library(DBI)

con <- dbConnect(
  drv = RMySQL::MySQL(),
  user = "admin",
  password = "12341234",
  host = "database-1.cplzaomaeuxr.ap-northeast-2.rds.amazonaws.com",
  dbname = "mini",
  )

#한글설정
dbSendQuery(con, "SET NAMES utf8;") 
dbSendQuery(con, "SET CHARACTER SET utf8;") 
dbSendQuery(con, "SET character_set_connection=utf8;")


a <- dbGetQuery(con,
            "select * from final2;")


a


dbDisconnect(con)

# 설문지 SQL 코드
install.packages("RMySQL")

library(DBI)

con <- dbConnect(
  drv = RMySQL::MySQL(),
  user = "admin",
  password = "12341234",
  host = "database-1.cplzaomaeuxr.ap-northeast-2.rds.amazonaws.com",
  dbname = "mini",
)

#한글설정
dbSendQuery(con, "SET NAMES utf8;") 
dbSendQuery(con, "SET CHARACTER SET utf8;") 
dbSendQuery(con, "SET character_set_connection=utf8;")


a <- dbGetQuery(con,
                "select * from IT 취업준비생 구직활동 조사(응답);")


b <- dbGetQuery(con,
                "select * from IT 현직자 조사;")


a


dbDisconnect(con)
