keyword<-iconv(data$keyword2, "utf-8","UTF-8")
options(mc.cores=1)
keycorpus <- VCorpus(VectorSource(keyword))
tdm <- TermDocumentMatrix(keycorpus,
                          control = list(
                            removePunctuation= T,
                            removeNumbers = T))

nTerms(tdm)
nDocs(tdm)
head(Terms(tdm))
dtm<- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm, 1, sum)
dtm.new <- dtm[rowTotals >0, ]
dtm<- dtm.new
dtm
lda <- LDA(dtm, k=4, control = list(seed=12345))
term <- terms(lda,20)

summary(lda)
# 그래프
x <- posterior(lda)$terms
y <- data.frame(t(x[,apply(x,2,max)>0.03]))
z <- data.frame(type=paste("Topic",1),
                keyword=rownames(y), posterior=y[,1])

for (i in 1:4) {
  z <- rbind(z, data.frame(type=paste("Topic",i),
                           keyword=rownames(y), posterior=y[,i]))
}


ggplot(z, aes(keyword,posterior, fill=as.factor(keyword))) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip()+
  facet_wrap(~type, nrow=1) +
  theme(legend.position = "none")


#최적 토픽 개수 k 찾기
library(topicmodels)
library(ldatuning)
library(LDAvis)
library(tm)
library(slam)
library(dplyr)
ndtm<-dtm[row_sums(dtm)>0,]
opt<-FindTopicsNumber(ndtm,topics=2:20,metrics="Griffiths2004",
                      method="Gibbs",control=list(),mc.cores=4L)
opt
FindTopicsNumber_plot(opt)

hm<-function(x,precision=2000L){
  library(Rmpfr)
  llmed<-median(x)
  as.double(llmed-log(mean(exp(-mpfr(x,prec=precision)+llmed))))
}
k=2
burnin=1000
iter=1000
keep=50
fitted<-LDA(ndtm,k=k,method="Gibbs",
            control=list(burnin=burnin,iter=iter,keep=keep))

logliks<-fitted@logLiks[-c(1:(burnin/keep))]
hm(logliks)
seqq<-seq(2,30,1)
fitted_m<-lapply(seqq,function(k){LDA(ndtm,k=k,method="Gibbs",control=list(burnin=burnin,iter=iter,keep=keep))})
logliks_m<-lapply(fitted_m,function(L) L@logLiks[-c(1:(burnin/keep))])

hm_m<-sapply(logliks_m,function(h){hm(h)})
plot(seqq,hm_m,type="l")
seqq[which.max(hm_m)]



#tf-idf 행렬
tf<-data[,c("job","keyword2")]
tf$keyword2<-str_remove_all(tf$keyword2,"[.-]")
tf$keyword2<-ifelse(tf$keyword2==", "|tf$keyword2=="",NA,tf$keyword2)
tf <-tf %>% filter(!is.na(job)&!is.na(keyword2))

tf<-tibble(tf)
str(tf)
tf.<-tf %>% unnest_tokens(input=keyword2,output=word,token=function(x){str_split(x,", ")}) 
tf..<- tf. %>% count(job,word,sort=T)
ti<- tf.. %>% bind_tf_idf(term=word,document = job, n=n) %>% arrange(-tf_idf)
write_xlsx(ti,"tf-itf.xlsx")

a<-which(ti$word==","|ti$word=="ㆍ"|ti$word==""|ti$word==" "|ti$word=="a"|
           ti$word=="3d"|ti$word=="2d"|ti$word=="flash"|ti$word=="rpg"|ti$word=="cad")
ti2<-ti[-a,]

ti_app<-ti %>% filter(job=="앱")
ti_web<-ti %>% filter(job=="웹프로그래머")
ti_data<-ti %>% filter(job=="빅데이터·AI(인공지능)")
ti_sys<-ti %>% filter(job=="DBA+시스템")
table(data$job)

top2<-ti2 %>% group_by(job) %>% slice_max(tf_idf,n=10,with_ties = F)
top2$job<-factor(top2$job,levels=job)
ggplot(top2,aes(x=reorder_within(word,tf_idf,job),y=tf_idf,fill=job))+
  geom_col(show.legend=F)+coord_flip()+
  facet_wrap(~job,scales="free",ncol=2)+
  scale_x_reordered()+labs(x=NULL)
