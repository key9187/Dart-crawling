######################################## 0. 환경설정 #######################################
if(!require(rvest)){install.packages('rvest')}; library(rvest)
if(!require(stringr)){install.packages('stringr')}; library(stringr)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(reshape2)){install.packages('reshape2')}; library(reshape2)
if(!require(zoo)){install.packages('zoo')}; library(zoo)

############################## 1. url만 알면 특수관계자 파트 테이블 가져오는 함수 ##################################

### 웹크롤링 코드
get_table <- function(url){
  
  html <- read_html(url)
  node_document <- html_nodes(html, "body")   
  
  # 특수관계자 목차 알아내기
  document <- as.character(node_document)
  document <- gsub('</span>', '', document); document <- gsub('<span>', '', document); document <- gsub(":", "", document)
  
  text <- str_extract_all(document, '[:digit:]+[.][:space:][가-힣]+') # 특수관계자 파트 구분자 알아내기 
  text_which <- which(str_detect(text[[1]], '특수관계자'))
  start_text <- text[[1]][text_which]; end_text <- text[[1]][text_which+1]
  
  # 특수관계자 파트에 해당하는 테이블 번호 알아내기 
  tmp_1 <- str_split(document, start_text)   # start_text 단어를 기준으로 split 해서 시작번호 알아내기
  text_1 <- str_split(tmp_1[[1]][1], '<table')   
  start <- length(text_1[[1]])
  tmp_2 <- str_split(document, end_text)   # end_text 단어를 기준으로 split 해서 끝번호 알아내기
  text_2 <- str_split(tmp_2[[1]][1], '<table')
  end <- length(text_2[[1]]) - 1
  
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
  return(list_df)
}

############################## 2. 5개 회사_index 1:5 ##################################
### sample 1:5 url 불러오기
kospi200 <- read.csv(file='KOSPI200_web.csv')
sample1 <- as.character(kospi200$링크[1]); kospi200[1,]   # (주)BNK금융지주
sample2 <- as.character(kospi200$링크[2]); kospi200[2,]   # (주)DB하이텍
sample3 <- as.character(kospi200$링크[3]); kospi200[3,]   # (주)KB금융지주   
sample4 <- as.character(kospi200$링크[4]); kospi200[4,]   # (주)강원랜드
sample5 <- as.character(kospi200$링크[5]); kospi200[5,]   # (주)녹십자


############################## 3-1. 특수관계자 파트 테이블 정리하기_1번 ##################################
list_df <- get_table(sample1); str(list_df)
unit1 <- 1000   # 단위:천원

### (주)BNK금융지주의 첫번째 테이블_종속기업 구분_보류
tmp1 <- list_df[[1]]

### (주)BNK금융지주의 두번째 테이블_불필요
tmp2 <- list_df[[2]]   # 단위:천원

### (주)BNK금융지주의 세번째 테이블_당기
tmp3 <- list_df[[3]]
n_col <- ncol(tmp3)
col_name <- paste0(gsub("\\s", "", tmp3[1,]), "_", gsub("\\s", "", tmp3[2,]))
row_name <- paste(tmp3[,1], "&", gsub("\\s", "", tmp3[,2]))
tmp3 <- as.data.frame(apply(tmp3, 2, function(x) gsub("\\s", "", x)))
tmp3 <- tmp3[3:10,3:n_col]
tmp3 <- as.data.frame(lapply(tmp3, function(x) as.numeric(gsub(",", "", x)) * unit1))
colnames(tmp3) <- col_name[3:n_col]
category <- t(as.data.frame(str_split(row_name[3:10], "&")))
tmp3$구분 <- category[,1]; tmp3$회사명 <- category[,2]; tmp3$당기_전기 <- "당기"
tmp3 <- tmp3[c(n_col-1, n_col, n_col+1, 1:(n_col-2))]
tmp3

### (주)BNK금융지주의 네번째 테이블_불필요
tmp4 <- list_df[[4]]   

### (주)BNK금융지주의 다섯번째 테이블_전기
tmp5 <- list_df[[5]]   
n_col <- ncol(tmp5)
col_name <- paste0(gsub("\\s", "", tmp5[1,]), "_", gsub("\\s", "", tmp5[2,]))
row_name <- paste(gsub("\\s", "", tmp5[,1]), "&", gsub("\\s", "", tmp5[,2]))
tmp5 <- as.data.frame(apply(tmp5, 2, function(x) gsub("\\s", "", x)))
tmp5 <- tmp5[3:10,3:n_col]
tmp5 <- as.data.frame(lapply(tmp5, function(x) as.numeric(gsub(",", "", x)) * unit1))
colnames(tmp5) <- col_name[3:n_col]
category <- t(as.data.frame(str_split(row_name[3:10], "&")))
tmp5$구분 <- category[,1]; tmp5$회사명 <- category[,2]; tmp5$당기_전기 <- "전기"
tmp5 <- tmp5[c(n_col-1, n_col, n_col+1, 1:(n_col-2))]
tmp5

### (주)BNK금융지주의 여섯번째 테이블_불필요
tmp6 <- list_df[[6]]   

### (주)BNK금융지주의 일곱번째 테이블_당기
tmp7 <- list_df[[7]]   
n_col <- ncol(tmp7)
col_name <- paste0(gsub("\\s", "", tmp7[1,]), "_", gsub("\\s", "", tmp7[2,]))
row_name <- paste(gsub("\\s", "", tmp7[,1]), "&", gsub("\\s", "", tmp7[,2]))
tmp7 <- as.data.frame(apply(tmp7, 2, function(x) gsub("\\s", "", x)))
tmp7 <- tmp7[3:5,3:n_col]
tmp7 <- as.data.frame(lapply(tmp7, function(x) as.numeric(gsub(",", "", x)) * unit1))
colnames(tmp7) <- col_name[3:n_col]
category <- t(as.data.frame(str_split(row_name[3:5], "&")))
tmp7$구분 <- category[,1]; tmp7$회사명 <- category[,2]; tmp7$당기_전기 <- "당기"
tmp7 <- tmp7[c(n_col-1, n_col, n_col+1, 1:(n_col-2))]
tmp7

### (주)BNK금융지주의 여덟번째 테이블_불필요
tmp8 <- list_df[[8]]   

### (주)BNK금융지주의 아홉번째 테이블_전기
tmp9 <- list_df[[9]]   
n_col <- ncol(tmp9)
col_name <- paste0(gsub("\\s", "", tmp9[1,]), "_", gsub("\\s", "", tmp9[2,]))
row_name <- paste(gsub("\\s", "", tmp9[,1]), "&", gsub("\\s", "", tmp9[,2]))
tmp9 <- as.data.frame(apply(tmp9, 2, function(x) gsub("\\s", "", x)))
tmp9 <- tmp9[3:4,3:n_col]
tmp9 <- as.data.frame(lapply(tmp9, function(x) as.numeric(gsub(",", "", x)) * unit1))
colnames(tmp9) <- col_name[3:n_col]
category <- t(as.data.frame(str_split(row_name[3:4], "&")))
tmp9$구분 <- category[,1]; tmp9$회사명 <- category[,2]; tmp9$당기_전기 <- "전기"
tmp9 <- tmp9[c(n_col-1, n_col, n_col+1, 1:(n_col-2))]
tmp9

### (주)BNK금융지주의 열번째 테이블_불필요
tmp10 <- list_df[[10]]   

### (주)BNK금융지주의 열한번째 테이블_당기말(?)
tmp11 <- list_df[[11]]   
n_col <- ncol(tmp11)
col_name <- paste0(gsub("\\s", "", tmp11[1,]), "_", gsub("\\s", "", tmp11[2,]))
row_name <- paste(gsub("\\s", "", tmp11[,1]), "&", gsub("\\s", "", tmp11[,2]))
tmp11 <- as.data.frame(apply(tmp11, 2, function(x) gsub("\\s", "", x)))
tmp11 <- tmp11[3:10,3:n_col]
tmp11 <- as.data.frame(lapply(tmp11, function(x) as.numeric(gsub(",", "", x)) * unit1))
colnames(tmp11) <- col_name[3:n_col]
category <- t(as.data.frame(str_split(row_name[3:10], "&")))
tmp11$구분 <- category[,1]; tmp11$회사명 <- category[,2]; tmp11$당기_전기 <- "당기"
tmp11 <- tmp11[c(n_col-1, n_col, n_col+1, 1:(n_col-2))]
tmp11

### (주)BNK금융지주의 열두번째 테이블_불필요
tmp12 <- list_df[[12]]   

### (주)BNK금융지주의 열세번째 테이블_전기말(?)
tmp13 <- list_df[[13]]   
n_col <- ncol(tmp13)
col_name <- paste0(gsub("\\s", "", tmp13[1,]), "_", gsub("\\s", "", tmp13[2,]))
row_name <- paste(gsub("\\s", "", tmp13[,1]), "&", gsub("\\s", "", tmp13[,2]))
tmp13 <- as.data.frame(apply(tmp13, 2, function(x) gsub("\\s", "", x)))
tmp13 <- tmp13[3:10,3:n_col]
tmp13 <- as.data.frame(lapply(tmp13, function(x) as.numeric(gsub(",", "", x)) * unit1))
colnames(tmp13) <- col_name[3:n_col]
category <- t(as.data.frame(str_split(row_name[3:10], "&")))
tmp13$구분 <- category[,1]; tmp13$회사명 <- category[,2]; tmp13$당기_전기 <- "전기"
tmp13 <- tmp13[c(n_col-1, n_col, n_col+1, 1:(n_col-2))]
tmp13

### (주)BNK금융지주의 열네번째 테이블_불필요
tmp14 <- list_df[[14]]   

### (주)BNK금융지주의 열다섯번째 테이블_불필요
tmp15 <- list_df[[15]]   

### list_df의 당기 전기 테이블 합치기
# 당기: tmp3, tmp7, tmp11
# 전기: tmp5, tmp9, tmp13
final1_c <- merge(tmp3, tmp7, by=c("구분", "회사명", "당기_전기"), all=TRUE)
final1_c <- merge(final1_c, tmp11, by=c("구분", "회사명", "당기_전기"), all=TRUE)
final1_p <- merge(tmp5, tmp9, by=c("구분", "회사명", "당기_전기"), all=TRUE)
final1_p <- merge(final1_p, tmp13, by=c("구분", "회사명", "당기_전기"), all=TRUE)

final1 <- rbind(final1_c, final1_p); final1



############################## 3-2. 특수관계자 파트 테이블 정리하기_2번 ##################################
list_df <- get_table(sample2); str(list_df)
unit2 <- 1000000

### (주)DB하이텍의 첫번째 테이블_불필요
tmp1 <- list_df[[1]]; tmp1

### (주)DB하이텍의 두번째 테이블_불필요
tmp2 <- list_df[[2]]; tmp2

### (주)DB하이텍의 세번째 테이블_불필요
tmp3 <- list_df[[3]]; tmp3

### (주)DB하이텍의 네번째 테이블
tmp4 <- list_df[[4]]; tmp4
n_col <- ncol(tmp4)
col_name_1 <- ifelse(grepl("당", tmp4[1,]), "당기", ifelse(grepl("전", tmp4[1,]), "전기", "구분"))
col_name_2 <- gsub("\\s", "", tmp4[2,])
row_name <- paste(gsub("\\s", "", tmp4[,1]), "&", gsub("[(][*]1[)]", "", tmp4[,2]))
tmp4 <- tmp4[3:5,3:n_col]
tmp4 <- as.data.frame(lapply(tmp4, function(x) as.numeric(gsub(",", "", x)) * unit2))
category1 <- t(as.data.frame(str_split(row_name[3:5], "&")))
tmp4$구분 <- category1[,1]; tmp4$회사명 <- category1[,2]
tmp4 <- tmp4[c(n_col-1, n_col, 1:(n_col-2))]
colnames(tmp4) <- paste0(col_name_1, "&", col_name_2)
colnames(tmp4)[1] <- "구분"; colnames(tmp4)[2] <- "회사명"
tmp4 <- melt(tmp4, id=c("구분", "회사명"))
category2 <- t(as.data.frame(str_split(tmp4$variable, "&")))
tmp4$당기_전기 <- category2[,1]; tmp4$vari <- category2[,2]; tmp4 <- tmp4[,-3]
tmp4 <- dcast(tmp4, 구분+회사명+당기_전기~vari)
tmp4

### (주)DB하이텍의 다섯번째 테이블_불필요
tmp5 <- list_df[[5]]; tmp5

### (주)DB하이텍의 여섯번째 테이블
tmp6 <- list_df[[6]]; tmp6
n_col <- ncol(tmp6)
col_name_1 <- ifelse(grepl("당", tmp6[1,]), "당기", ifelse(grepl("전", tmp6[1,]), "전기", "구분"))
col_name_2 <- gsub("\\s", "", tmp6[2,])
row_name <- paste(gsub("\\s", "", tmp6[,1]), "&", gsub("[(][*]1[)]", "", tmp6[,2]))
tmp6 <- tmp6[3:5,3:n_col]
tmp6 <- as.data.frame(lapply(tmp6, function(x) as.numeric(gsub(",", "", x)) * unit2))
category1 <- t(as.data.frame(str_split(row_name[3:5], "&")))
tmp6$구분 <- category1[,1]; tmp6$회사명 <- category1[,2]
tmp6 <- tmp6[c(n_col-1, n_col, 1:(n_col-2))]
colnames(tmp6) <- paste0(col_name_1, "&", col_name_2)
colnames(tmp6)[1] <- "구분"; colnames(tmp6)[2] <- "회사명"
tmp6 <- melt(tmp6, id=c("구분", "회사명"))
category2 <- t(as.data.frame(str_split(tmp6$variable, "&")))
tmp6$당기_전기 <- category2[,1]; tmp6$vari <- category2[,2]; tmp6 <- tmp6[,-3]
tmp6 <- dcast(tmp6, 구분+회사명+당기_전기~vari)
tmp6

### (주)DB하이텍의 일곱번째 테이블
tmp7 <- list_df[[7]]; tmp7
n_col <- ncol(tmp7)
col_name_1 <- ifelse(grepl("당", tmp7[2,]), "당기", ifelse(grepl("전", tmp7[2,]), "전기", "구분"))
col_name_2 <- gsub("\\s", "", tmp7[1,])
row_name <- paste(gsub("\\s", "", tmp7[,1]), "&", gsub("[(][*]1[)]", "", tmp7[,2]))
tmp7 <- tmp7[3:5,3:n_col]
tmp7 <- as.data.frame(lapply(tmp7, function(x) as.numeric(gsub(",", "", x)) * unit2))
category1 <- t(as.data.frame(str_split(row_name[3:5], "&")))
tmp7$구분 <- category1[,1]; tmp7$회사명 <- category1[,2]
tmp7 <- tmp7[c(n_col-1, n_col, 1:(n_col-2))]
colnames(tmp7) <- paste0(col_name_1, "&", col_name_2)
colnames(tmp7)[1] <- "구분"; colnames(tmp7)[2] <- "회사명"
tmp7 <- melt(tmp7, id=c("구분", "회사명"))
category2 <- t(as.data.frame(str_split(tmp7$variable, "&")))
tmp7$당기_전기 <- category2[,1]; tmp7$vari <- category2[,2]; tmp7 <- tmp7[,-3]
tmp7 <- dcast(tmp7, 구분+회사명+당기_전기~vari)
tmp7


### (주)DB하이텍의 여덟번째 테이블_불필요
tmp8 <- list_df[[8]]; tmp8

### (주)DB하이텍의 아홉번째 테이블_불필요
tmp9 <- list_df[[9]]; tmp9

### (주)DB하이텍의 열번째 테이블_??_자금거래
tmp10 <- list_df[[10]]; tmp10

### (주)DB하이텍의 열한번째 테이블_불필요
tmp11 <- list_df[[11]]; tmp11

### (주)DB하이텍의 열두번째 테이블_??_자금거래
tmp12 <- list_df[[12]]; tmp12

### (주)DB하이텍의 열세번째 테이블_불필요
tmp13 <- list_df[[13]]; tmp13

### (주)DB하이텍의 열네번째 테이블_??_담보
tmp14 <- list_df[[14]]; tmp14

### list_df의 당기 전기 테이블 합치기
# tmp4, tmp6, tmp7
final2 <- merge(tmp4, tmp6, by=c("구분", "회사명", "당기_전기"), all=TRUE)
final2 <- merge(final2, tmp7, by=c("구분", "회사명", "당기_전기"), all=TRUE)
final2


############################## 3-3. 특수관계자 파트 테이블 정리하기_3번 ##################################
list_df <- get_table(sample3); str(list_df)
unit3 <- 1000000   

### (주)KB금융지주의 첫번째 테이블
tmp1 <- list_df[[1]]; tmp1
n_col <- ncol(tmp1); n_row <- nrow(tmp1)
tmp1[,2] <- na.locf(gsub(tmp1[1,1], NA, tmp1[,2]), fromLast=FALSE)
row_name <- paste0(gsub("//s", "", tmp1[,1]), "&", gsub("\\s", "", tmp1[,2]), "&", gsub("\\s", "", tmp1[,3]))
tmp1 <- tmp1[1:n_row,4:n_col]
tmp1 <- as.data.frame(lapply(tmp1, function(x) as.numeric(gsub(",", "", x)) * unit3))
category <- t(as.data.frame(str_split(row_name, "&")))
tmp1$구분 <- category[,1]; tmp1$회사명 <- category[,2]; tmp1$col_var <- category[,3]
tmp1 <- melt(tmp1, id=c("구분", "회사명", "col_var"))
colnames(tmp1)[4] <- "당기_전기"
tmp1 <- dcast(tmp1, 구분+회사명+당기_전기~col_var, sum)
tmp1

### (주)KB금융지주의 두번째 테이블
tmp2 <- list_df[[2]]; tmp2
n_col <- ncol(tmp2); n_row <- nrow(tmp2)
tmp2[,2] <- na.locf(gsub(tmp2[1,1], NA, tmp2[,2]), fromLast=FALSE)
row_name <- paste0(gsub("//s", "", tmp2[,1]), "&", gsub("\\s", "", tmp2[,2]), "&", gsub("\\s", "", tmp2[,3]))
tmp2 <- tmp2[1:n_row,4:n_col]
tmp2 <- as.data.frame(lapply(tmp2, function(x) as.numeric(gsub(",", "", x)) * unit3))
category <- t(as.data.frame(str_split(row_name, "&")))
tmp2$구분 <- category[,1]; tmp2$회사명 <- category[,2]; tmp2$col_var <- category[,3]
tmp2 <- melt(tmp2, id=c("구분", "회사명", "col_var"))
colnames(tmp2)[4] <- "당기_전기"
tmp2 <- dcast(tmp2, 구분+회사명+당기_전기~col_var, sum)
tmp2

### (주)KB금융지주의 세번째 테이블_??_자금거래
tmp3 <- list_df[[3]]; tmp3

### (주)KB금융지주의 네번째 테이블_??_자금거래
tmp4 <- list_df[[4]]; tmp4

### (주)KB금융지주의 다섯번째 테이블_??_미사용약정
tmp5 <- list_df[[5]]; tmp5

### (주)KB금융지주의 여섯번째 테이블_불필요
tmp6 <- list_df[[6]]; tmp6

### (주)KB금융지주의 일곱번째 테이블_불필요
tmp7 <- list_df[[7]]; tmp7

### list_df의 당기 전기 테이블 합치기
# tmp1, tmp2
final3 <- merge(tmp1, tmp2, by=c("구분", "회사명", "당기_전기"), all=TRUE)
final3


############################## 3-4. 특수관계자 파트 테이블 정리하기_4번 ##################################
list_df <- get_table(sample4); str(list_df)
unit4 <- 1000

### (주)강원랜드의 첫번째 테이블_불필요
tmp1 <- list_df[[1]]; tmp1

### (주)강원랜드의 두번째 테이블_불필요
tmp2 <- list_df[[2]]; tmp2

### (주)강원랜드의 세번째 테이블
tmp3 <- list_df[[3]]; tmp3
n_col <- ncol(tmp3); n_row <- nrow(tmp3)
tmp3[,2] <- gsub("[(][*][)]", "", tmp3[,2])
row_name <- paste0(gsub("//s", "", tmp3[,1]), "&", gsub("\\s", "", tmp3[,2]), "&", gsub("\\s", "", tmp3[,3]), "&", gsub("\\s", "", tmp3[,4]))
col_name <- gsub("\\s", "", tmp3[1,])
tmp3 <- tmp3[2:n_row,5:n_col]
tmp3 <- as.data.frame(lapply(tmp3, function(x) as.numeric(gsub(",", "", x)) * unit4))
category <- t(as.data.frame(str_split(row_name[2:n_row], "&")))
tmp3$구분 <- category[,1]; tmp3$회사명 <- category[,2]; tmp3$col_var <- paste0(category[,3],category[,4])
tmp3 <- tmp3[c(n_col-3, n_col-2, n_col-1, 1:(n_col-4))]
colnames(tmp3) <- col_name[c(1:3,5:6)]
tmp3 <- melt(tmp3, id=c("구분", "회사명", "내용"))
colnames(tmp3)[4] <- "당기_전기"
tmp3 <- dcast(tmp3, 구분+회사명+당기_전기~내용, sum)
tmp3

### (주)강원랜드의 네번째 테이블
tmp4 <- list_df[[4]]; tmp4
n_col <- ncol(tmp4); n_row <- nrow(tmp4)
tmp4[,2] <- gsub("[(][*]+.+[)]", "", tmp4[,2])
row_name <- paste0(gsub("//s", "", tmp4[,1]), "&", gsub("\\s", "", tmp4[,2]), "&", gsub("\\s", "", tmp4[,3]), "&", gsub("\\s", "", tmp4[,4]))
col_name <- gsub("\\s", "", tmp4[1,])
tmp4 <- tmp4[2:n_row,5:n_col]
tmp4 <- as.data.frame(lapply(tmp4, function(x) as.numeric(gsub(",", "", x)) * unit4))
category <- t(as.data.frame(str_split(row_name[2:n_row], "&")))
tmp4$구분 <- category[,1]; tmp4$회사명 <- category[,2]; tmp4$col_var <- paste0(category[,3],category[,4])
tmp4 <- tmp4[c(n_col-3, n_col-2, n_col-1, 1:(n_col-4))]
colnames(tmp4) <- col_name[c(1:3,5:6)]
tmp4 <- melt(tmp4, id=c("구분", "회사명", "내용"))
colnames(tmp4)[4] <- "당기_전기"
tmp4 <- dcast(tmp4, 구분+회사명+당기_전기~내용, sum)
tmp4

### list_df의 당기 전기 테이블 합치기
# tmp3, tmp4
final4 <- merge(tmp3, tmp4, by=c("구분", "회사명", "당기_전기"), all=TRUE)
final4

############################## 3-5. 특수관계자 파트 테이블 정리하기_5번 ##################################
list_df <- get_table(sample5); str(list_df)
unit5 <- 1000000

### (주)녹십자의 첫번째 테이블_불필요
tmp1 <- list_df[[1]]; tmp1

### (주)녹십자의 두번째 테이블_당기
tmp2 <- list_df[[2]]; tmp2
n_row <- nrow(tmp2); n_col <- ncol(tmp2)
col_name <-gsub("\\s", "", tmp2[1,])
row_name <- paste0(gsub("\\s","",tmp2[,1]), "&", gsub("\\s", "", tmp2[,2]))
tmp2 <- tmp2[2:18,3:n_col]
tmp2 <- as.data.frame(lapply(tmp2, function(x) as.numeric(gsub(",", "", x)) * unit5))
category <- t(as.data.frame(str_split(row_name, "&")))
tmp2$구분 <- category[2:18,1]; tmp2$회사명 <- category[2:18,2]
tmp2 <- tmp2[c(n_col-1, n_col, 1:(n_col-2))]
colnames(tmp2) <- col_name
tmp2$당기_전기 <- "당기"
tmp2 <- tmp2[,c(1:2, n_col+1, 3:n_col)]
colnames(tmp2)[2] <- "회사명"
tmp2

### (주)녹십자의 세번째 테이블_전기
tmp3 <- list_df[[3]]; tmp3
n_row <- nrow(tmp3); n_col <- ncol(tmp3)
col_name <-gsub("\\s", "", tmp3[1,])
row_name <- paste0(gsub("\\s","",tmp3[,1]), "&", gsub("\\s", "", tmp3[,2]))
tmp3 <- tmp3[2:(n_row-2),3:n_col]
tmp3 <- as.data.frame(lapply(tmp3, function(x) as.numeric(gsub(",", "", x)) * unit5))
category <- t(as.data.frame(str_split(row_name, "&")))
tmp3$구분 <- category[2:(n_row-2),1]; tmp3$회사명 <- category[2:(n_row-2),2]
tmp3 <- tmp3[c(n_col-1, n_col, 1:(n_col-2))]
colnames(tmp3) <- col_name
tmp3$당기_전기 <- "전기"
tmp3 <- tmp3[,c(1:2, n_col+1, 3:n_col)]
colnames(tmp3)[2] <- "회사명"
tmp3

### (주)녹십자의 네번째 테이블_당기
tmp4 <- list_df[[4]]; tmp4
row_name_1 <- c(rep("지배기업",1), rep("종속기업", 3), rep("관계기업", 3), rep("기타특수관계자", 4))
tmp4 <- tmp4[!grepl("기업", tmp4[,1]),]; tmp4 <- tmp4[!grepl("기타", tmp4[,1]),]
n_row <- nrow(tmp4); n_col <- ncol(tmp4)
row_name_2 <- gsub("\\s", "", tmp4[,1])
col_name <- gsub("\\s", "", tmp4[1,])
tmp4 <- tmp4[2:(n_row-1),2:n_col]
tmp4 <- as.data.frame(lapply(tmp4, function(x) as.numeric(gsub(",", "", x)) * unit5))
tmp4$구분 <- row_name_1; tmp4$회사명 <- row_name_2[2:(n_row-1)]
tmp4 <- tmp4[c(n_col, n_col+1, 1:(n_col-1))]
colnames(tmp4)[3:(n_col+1)] <- col_name[-1]
tmp4$당기_전기 <- "당기"
tmp4 <- tmp4[,c(1:2, n_col+2, 3:(n_col+1))]
tmp4

### (주)녹십자의 다섯번째 테이블_전기
tmp5 <- list_df[[5]]; tmp5
row_name_1 <- c(rep("지배기업",1), rep("종속기업", 3), rep("관계기업", 3), rep("기타특수관계자", 4))
tmp5 <- tmp5[!grepl("기업", tmp5[,1]),]; tmp5 <- tmp5[!grepl("기타", tmp5[,1]),]
n_row <- nrow(tmp5); n_col <- ncol(tmp5)
row_name_2 <- gsub("\\s", "", tmp5[,1])
col_name <- gsub("\\s", "", tmp5[1,])
tmp5 <- tmp5[2:(n_row-1),2:n_col]
tmp5 <- as.data.frame(lapply(tmp5, function(x) as.numeric(gsub(",", "", x)) * unit5))
tmp5$구분 <- row_name_1; tmp5$회사명 <- row_name_2[2:(n_row-1)]
tmp5 <- tmp5[c(n_col, n_col+1, 1:(n_col-1))]
colnames(tmp5)[3:(n_col+1)] <- col_name[-1]
tmp5$당기_전기 <- "전기"
tmp5 <- tmp5[,c(1:2, n_col+2, 3:(n_col+1))]
tmp5

### (주)녹십자의 여섯번째 테이블_불필요
tmp6 <- list_df[[6]]; tmp6

### (주)녹십자의 일곱번째 테이블_??_보증내역
tmp7 <- list_df[[7]]; tmp7

### list_df의 당기 전기 테이블 합치기
# 당기: tmp2, tmp4
# 전기: tmp3, tmp5
final5_c <- merge(tmp2, tmp4, by=c("구분", "회사명", "당기_전기"), all=TRUE)
final5_p <- merge(tmp3, tmp5, by=c("구분", "회사명", "당기_전기"), all=TRUE)

final5 <- merge(final5_c, final5_p, by=c("구분", "회사명", "당기_전기"), all=TRUE); final5


