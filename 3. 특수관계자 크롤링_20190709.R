######################################## 0. 환경설정 #######################################
if(!require(rvest)){install.packages('rvest')}; library(rvest)
if(!require(stringr)){install.packages('stringr')}; library(stringr)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(reshape2)){install.packages('reshape2')}; library(reshape2)


############################## 1. 2개의 회사 sampling ##################################
### random sample
set.seed(22)
sample(1:200, 2) # 61 95

### url 불러오기
kospi200 <- read.csv(file='KOSPI200_web.csv')
sample1 <- as.character(kospi200$링크[61]); kospi200[61,]   # (주)포스코   
sample2 <- as.character(kospi200$링크[96]); kospi200[96,]   # 대적전자(주)   


############################## 2. 특수관계자 파트 테이블 가져오기 ##################################
### 웹크롤링 코드
url <- sample1

html <- read_html(url)
node_document <- html_nodes(html, "body")   

# 특수관계자 목차 알아내기
document <- as.character(node_document)
document <- gsub('</span>', '', document); document <- gsub('<span>', '', document); document <- gsub(":", "", document)
  
text <- str_extract_all(document, '[:digit:]+[.][:space:][가-힣]+') # 특수관계자 파트 구분자 알아내기 
text_which <- which(str_detect(text[[1]], '특수관계자'))
start_text <- text[[1]][text_which]; end_text <- text[[1]][text_which+1]
start_text; end_text

# 특수관계자 파트에 해당하는 테이블 번호 알아내기 
tmp_1 <- str_split(document, start_text)   # start_text 단어를 기준으로 split 해서 시작번호 알아내기
text_1 <- str_split(tmp_1[[1]][1], '<table')   
start <- length(text_1[[1]])
tmp_2 <- str_split(document, end_text)   # end_text 단어를 기준으로 split 해서 끝번호 알아내기
text_2 <- str_split(tmp_2[[1]][1], '<table')
end <- length(text_2[[1]]) - 1
start; end

# 해당되는 부분 테이블 리스트로 저정하기 
table_set <- node_document %>% html_nodes("table")   #웹크롤링 코드
list_df <- list()
for (i in start:end){
    Sys.setlocale("LC_ALL", "English")
    table_tmp <- html_table(table_set[[i]], fill=TRUE, header = NA)
    Sys.setlocale("LC_ALL", "Korean")
    idx <- i - start + 1
    list_df[[idx]] <- table_tmp
}


############################## 3-1. 특수관계자 파트 테이블 정리하기_61번 ##################################
sample1

### list_df의 첫번째 테이블_당기
tmp1 <- list_df[[1]]
col_name <- paste0(gsub("\\s", "", gsub("[(]+주+[0-9]+[)]", "", as.character(tmp1[1,]))), "_", gsub("\\s", "", as.character(tmp1[2,])))
구분 <- c(rep("종속기업", 17), rep("관계기업 및 공동기업",4))
row_name <- paste(구분, "&", gsub("\\s", "",tmp1[c(4:20, 24:27),1]))
tmp1 <- tmp1[c(4:20, 24:27),2:ncol(tmp1)]
tmp1 <- as.data.frame(lapply(tmp1, function(x) as.numeric(gsub(",", "", x)) * 1000000))
rownames(tmp1) <- row_name; colnames(tmp1) <- col_name[-1]

### list_df의 두번째 테이블_불필요
tmp2 <- list_df[[2]]

### list_df의 세번째 테이블_전기
tmp3 <- list_df[[3]]
col_name <- paste0(gsub("\\s", "", gsub("[(]+주+[0-9]+[)]", "", as.character(tmp3[1,]))), "_", gsub("\\s", "", as.character(tmp3[2,])))
구분 <- c(rep("종속기업", 17), rep("관계기업 및 공동기업",5))
row_name <- paste(구분, "&", gsub("\\s", "",tmp3[c(4:20, 24:28),1]))
tmp3 <- tmp3[c(4:20, 24:28),2:ncol(tmp3)]
tmp3 <- as.data.frame(lapply(tmp3, function(x) as.numeric(gsub(",", "", x)) * 1000000))
rownames(tmp3) <- row_name; colnames(tmp3) <- col_name[-1]

### list_df의 네번째 테이블_당기
tmp4 <- list_df[[4]]
col_name <- paste0(gsub("\\s", "", gsub("[(]+주+[0-9]+[)]", "", as.character(tmp4[1,]))), "_", gsub("\\s", "", as.character(tmp4[2,])))
구분 <- c(rep("종속기업", 13), rep("관계기업 및 공동기업",3))
row_name <- paste(구분, "&", gsub("\\s", "", tmp4[c(4:16, 20:22),1]))
tmp4 <- tmp4[c(4:16, 20:22),2:ncol(tmp4)]
tmp4 <- as.data.frame(lapply(tmp4, function(x) as.numeric(gsub(",", "", x)) * 1000000))
rownames(tmp4) <- row_name; colnames(tmp4) <- col_name[-1]

### list_df의 다섯번째 테이블_전기
tmp5 <- list_df[[5]]
col_name <- paste0(gsub("\\s", "", gsub("[(]+주+[0-9]+[)]", "", as.character(tmp5[1,]))), "_", gsub("\\s", "", as.character(tmp5[2,])))
구분 <- c(rep("종속기업", 13), rep("관계기업 및 공동기업",2))
row_name <- paste(구분, "&", gsub("\\s", "", tmp5[c(4:16, 20:21),1]))
tmp5 <- tmp5[c(4:16, 20:21),2:ncol(tmp5)]
tmp5 <- as.data.frame(lapply(tmp5, function(x) as.numeric(gsub(",", "", x)) * 1000000))
rownames(tmp5) <- row_name; colnames(tmp5) <- col_name[-1]

### list_df의 여섯번째 테이블_불필요
tmp6 <- list_df[[6]]

### list_df의 당기 전기 테이블 합치기
# tmp1, tmp4 당기, tmp3, tmp5 전기
tmp1$구분 <- rownames(tmp1); tmp4$구분 <- rownames(tmp4)
final_61_1 <- cbind("당기", merge(tmp1, tmp4, by="구분", all=TRUE))
final_61_1 <- final_61_1[,c(1,2:15)]; colnames(final_61_1)[1] = "전기/당기"

tmp3$구분 <- rownames(tmp3); tmp5$구분 <- rownames(tmp5)
final_61_2 <- cbind("전기", merge(tmp3, tmp5, by="구분", all=TRUE))
final_61_2 <-  final_61_2[,c(1,2:15)]; colnames(final_61_2)[1] = "전기/당기"

final1 <- merge(final_61_1, final_61_2, by=c("구분", "전기/당기"), all=TRUE) 
final1


############################## 3-2. 특수관계자 파트 테이블 정리하기_95번 ##################################
sample2

### list_df의 첫번째 테이블_종속기업 구분
tmp1 <- list_df[[1]]
colnames(tmp1) <- gsub("\\s", "", colnames(tmp1))
tmp1 <- tmp1[c(2:5,7:12),c(1,3)]
tmp1$구분 <- gsub("\\s", "", tmp1$구분)
colnames(tmp1) <- c("회사명", "관계")
tmp1$회사명 <- gsub("[(][*]1[)]", "", tmp1$회사명)
  
### list_df의 두번째 테이블_당기/전기
tmp2 <- list_df[[2]]
colnames(tmp2) <- gsub("\\s", "", colnames(tmp2))
tmp2$회사명 <- gsub("\\s", "", tmp2$회사명); tmp2$회사명 <- gsub("[(][*]1[)]", "", tmp2$회사명)
tmp2 <- melt(tmp2, id=c("회사명", "내역"))
tmp2$value <- as.numeric(gsub(",", "", tmp2$value)) * 1000   #단위:천원
tmp2 <- dcast(tmp2, 회사명+variable~내역)

### list_df의 세번째 테이블_당기/전기
tmp3 <- list_df[[3]]
colnames(tmp3) <- gsub("\\s", "", colnames(tmp3))
tmp3$회사명 <- gsub("\\s", "", tmp3$회사명); tmp3$회사명 <- gsub("[(][*]1[)]", "", tmp3$회사명)
tmp3 <- melt(tmp3, id=c("회사명", "내역"))
tmp3$value <- as.numeric(gsub(",", "", tmp3$value)) * 1000   #단위:천원
tmp3 <- dcast(tmp3, 회사명+variable~내역)

### list_df의 네번째 테이블_불필요
tmp4 <- list_df[[4]]

### list_df의 당기 전기 테이블 합치기_tmp1,tmp2,tmp3
final2 <- merge(tmp1, tmp2, by="회사명", all=TRUE)
final2 <- merge(final2, tmp3, by=c("회사명", "variable"), all=TRUE)
final2 <- final2 %>% filter(variable != "NA")
final2
