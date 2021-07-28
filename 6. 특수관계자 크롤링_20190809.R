######################################## 0. 환경설정 #######################################
if(!require(rvest)){install.packages('rvest')}; library(rvest)
if(!require(stringr)){install.packages('stringr')}; library(stringr)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(reshape2)){install.packages('reshape2')}; library(reshape2)
if(!require(zoo)){install.packages('zoo')}; library(zoo)
if(!require(xlsx)){install.package('xlsx')}; library(xlsx)

### url 불러오기
kospi200 <- read.csv(file='KOSPI200_web.csv')
#kospi200 <- read.csv(file='KOSPI200_web.csv')
### 현재수준 정의 가능 회사number: 1~5, 61, 96, (6~10)

############################## 1. 필요한 함수 정의 ##################################

### url만 알면 특수관계자 파트 테이블 가져오는 함수 정의
get_table <- function(url){
  
  html <- read_html(url)
  node_document <- html_nodes(html, "body")   
  
  # 특수관계자 목차 알아내기
  document <- as.character(node_document)
  document <- gsub('</span>', '', document); document <- gsub('<span>', '', document)
  document <- gsub(":", "", document); document <- gsub('"', "", document); document <- gsub('<span style=font-weightbold;>', "", document)
  
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

### 기업명 텍스트 정제하는 함수 정의 
text_clean <- function(tmp_text){
  tmp_clean <- gsub("\\s", "", gsub("[(]+주+[0-9]+[)]", "", as.character(tmp_text)))
  tmp_clean <- gsub('\\[', '', gsub('\\]', '', tmp_clean))
  tmp_clean <- gsub("[(]+\\*+[0-9][)]+[,]+[(]+\\*+[0-9][)]", "", tmp_clean)
  tmp_clean <- gsub("[(][*][0-9][)]", "", tmp_clean)
  tmp_clean <- gsub("[(]+\\*+[)]", "", tmp_clean)
  tmp_clean <- gsub("\\:", "", tmp_clean)
  return(tmp_clean)
}


############################## 2-1. case1 당기/전기 테이블 분리 형태 ##################################

### case 1 함수 
case1_table <- function(path){
  list_df <- get_table(path)
  
  final_table <- data.frame()
  tmp <- data.frame()
  
  for(i in 1:length(list_df)){
    if(list_TF[i] == TRUE){
      ### list_df의 테이블을 tmp에 저장
      tmp <- list_df[[i]]
      ###구분, 회사명 외에 필요없는 구분자가 있을 경우 제거
      tmp <- tmp[,!str_detect(colnames(tmp), "거래내용")]
      ### 구분항목이 행으로 입력되어 있는 경우, 열로 입력되어 있는 경우 나눠서 수행
      real_r <- grepl("기업", tmp[,1]) | grepl("기타", tmp[,1])
      if(str_detect(tmp[real_r,2], "기업")[1] == TRUE){
        ### col, row 이름 명명하기
        col_name <- text_clean(tmp[1,])
        row_name2 <- text_clean(tmp[,1])
        row_name1 <- na.locf(ifelse(str_detect(row_name2, "기업") == TRUE, row_name2, NA), na.rm=FALSE, fromLast = FALSE)
        row_name <- paste0(row_name1, "@", row_name2); row_name <- row_name[!real_r]; row_name <- row_name[start_r:length(row_name)]
        ### 숫자만 있는 테이블로 만들기
        tmp <- tmp[!real_r,]; tmp <- tmp[start_r:nrow(tmp), start_c:ncol(tmp)]
      } else{
        ### col, row 이름 명명하기
        col_name <- text_clean(tmp[1,])
        row_name1 <- text_clean(tmp[,1])
        row_name2 <- text_clean(tmp[,2])
        row_name <- paste0(row_name1, "@", row_name2); row_name <- row_name[start_r:length(row_name)]
        ### 숫자만 있는 테이블로 만들기
        tmp <- tmp[start_r:nrow(tmp), start_c:ncol(tmp)]
      }
      ### 숫자만 있는 테이블로 만들기
      tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(gsub(",", "", gsub("\\s", "", x))) * unit))
      rownames(tmp) <- row_name; colnames(tmp) <- col_name[start_c:length(col_name)]
      ### category를 다시 만들어서 table 재정의_col
      category <- t(as.data.frame(str_split(row_name, "@")))
      tmp$구분 <- category[,1]; tmp$회사명 <- category[,2]
      tmp$당기_전기 <- list_당전[i]
      tmp <- tmp[, c((ncol(tmp)-2):ncol(tmp), 1:(ncol(tmp)-3))]
      ### category를 다시 만들어서 table 재정의_row
      tmp <- tmp[!str_detect(rownames(tmp), paste(c("소계", "합계", "기타"),collapse = '|')),]
      rownames(tmp) <- NULL
    } 
    ### table 합치기
    final_table <- merge(final_table, tmp, all = TRUE)
    if(nrow(final_table)==0){
      final_table <- merge(final_table, tmp, all = TRUE)
    }
  }
  final_table <- aggregate(final_table[,4:ncol(final_table)], by=list(final_table$구분, final_table$회사명, final_table$당기_전기), sum, na.rm=TRUE, na.action=NULL)
  colnames(final_table)[1:3] <- c("구분", "회사명", "당기_전기")
  return(final_table)
}


### '61_(주)포스코'
sample61 <- as.character(kospi200$링크[61]); kospi200[61,]   # (주)포스코   
### 사전 정의가 필요한 내용
unit <- 1000000   
list_TF <- c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
list_당전 <- c("당기", NA, "전기", "당기", "전기", NA)
start_r=3; start_c = 2
### 함수로 최종 posco에 대한 table 얻어내기
posco_table <- case1_table(path=sample61)


# '1_(주)BNK금융지주'
sample1 <- as.character(kospi200$링크[1]); kospi200[1,]   # (주)BNK금융지주
### 사전 정의가 필요한 내용
unit <- 1000   # 단위:천원
list_TF <- rep(FALSE, 15); list_TF[c(3,5,7,9,11,13)] <- TRUE
list_당전 <- rep(NA, 15); list_당전[c(3,7,11)] <- "당기"; list_당전[c(5,9,13)] <- "전기"
start_r=3; start_c = 3
### 함수로 최종 BNK금융지주 대한 table 얻어내기
BNK_table <- case1_table(path=sample1)


# '5_(주)녹십자'
sample5 <- as.character(kospi200$링크[5]); kospi200[5,]   # (주)녹십자
### 사전 정의가 필요한 내용
unit <- 1000000   # 단위:천원
list_TF <- rep(FALSE, 7); list_TF[2:5] <- TRUE
list_당전 <- rep(NA, 15); list_당전[c(2,4)] <- "당기"; list_당전[c(3,5)] <- "전기"
start_r=2; start_c = 3
### 함수로 최종 (주)녹십자에 대한 table 얻어내기
green_table <- case1_table(path=sample5)


### '6_(주)녹십자홀딩스'
sample6 <- as.character(kospi200$링크[6]); kospi200[6,]   
### 사전 정의가 필요한 내용
unit <- 1000000
list_TF <- rep(FALSE, 7); list_TF[2:5] <- TRUE
list_당전 <- c(NA, "당기", "전기", "당기", "전기", NA, NA)
start_r=2; start_c = 3
### 함수로 최종 (주)녹십자홀딩스에 대한 table 얻어내기
green_hol_table <- case1_table(path=sample6)


### '9_(주)대우건설' 
sample9 <- as.character(kospi200$링크[9]); kospi200[9,]
### 사전 정의가 필요한 내용
unit <- 1000000
list_TF <- rep(FALSE, 21); list_TF[c(3,5,7,9)] <- TRUE
list_당전 <- rep(NA, 21); list_당전[c(3,7)] <- "당기"; list_당전[c(5,9)] <- "전기"
start_r = 2; start_c = 3
### 함수로 최종 (주)대우건설에 대한 table 얻어내기
daewoo_table <- case1_table(path=sample9)


### '10_(주)대웅제약'
sample10 <- as.character(kospi200$링크[10]); kospi200[10,]
### 사전 정의가 필요한 내용
unit <- 1000
list_TF <- rep(FALSE, 10); list_TF[3:8] <- TRUE
list_당전 <- rep(NA, 10); list_당전[c(3,5,7)] <- "당기"; list_당전[c(4,6,8)] <- "전기"
start_r = 3; start_c = 3
### 함수로 최종 (주)대웅제약에 대한 table 얻어내기
daewoong_table <- case1_table(path=sample10)


### '11_(주)대한항공'
sample11 <- as.character(kospi200$링크[11]); kospi200[11,]
### 사전 정의가 필요한 내용
unit <- 1000
list_TF <- rep(FALSE, 11); list_TF[2:5] <- TRUE
list_당전 <- rep(NA, 11); list_당전[c(2,4)] <- "당기"; list_당전[c(3,5)] <- "전기"
start_r = 2; start_c = 3
### 함수로 최종 (주)대한항공에 대한 table 얻어내기
daehanair_table <- case1_table(path=sample11)


### '12_(주)동서'
sample12 <- as.character(kospi200$링크[12]); kospi200[12,]
### 사전 정의가 필요한 내용
unit <- 1000
list_TF <- rep(FALSE, 14); list_TF[c(2,4,6,8,10,12)] <- TRUE
list_당전 <- rep(NA, 14); list_당전[c(2,6,10)] <- "당기"; list_당전[c(4,8,12)] <- "전기"
start_r = 2; start_c = 3
### 함수로 최종 (주)동서에 대한 table 얻어내기
dongsoe_table <- case1_table(path=sample12)


############################## 2-2. case2 당기/전기 2개열 형태로 구성된 형태 ##################################

### case2 해당 함수
case2_table <- function(path){
  list_df <- get_table(path)
  ### 종속기업 구분
  contents <- list_df[[1]]
  colnames(contents) <- text_clean(colnames(contents))
  contents <- as.data.frame(lapply(contents, function(x) text_clean(x)))
  contents <- contents[,c(1,3)]
  #contents$구분 <- as.character(contents$구분); contents$당사와의관계 <- as.character(contents$당사와의관계)
  colnames(contents) <- c("회사명", "구분")
  
  final_table <- data.frame()
  tmp <- data.frame()
  
  for(i in 1:(length(list_df))){
    if(list_TF[i] == TRUE){
      ### list_df의 테이블을 tmp에 저장
      tmp <- list_df[[i]]
      ### col, row 이름 명명하기
      colnames(tmp) <- text_clean(colnames(tmp))
      tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)))
      id1 <- colnames(tmp)[1]; id2 <- colnames(tmp)[2]
      ### 테이블 틀 만들기
      tmp <- melt(tmp, id=c(id1, id2))
      tmp$value <- as.numeric(gsub(",", "", tmp$value)) * unit 
      tmp <- dcast(tmp, get(id1) + variable ~ get(id2))
      colnames(tmp)[1:2] <- c("회사명", "당기_전기")
      #tmp$회사명 <- as.character(tmp$회사명)
      tmp <- left_join(tmp, contents, by = "회사명")
      tmp <- tmp[,c(ncol(tmp), 1:(ncol(tmp)-1))]
      rownames(tmp) <- NULL
    } 
    ### table 합치기
    final_table <- merge(final_table, tmp, all = TRUE)
    if(nrow(final_table)==0){
      final_table <- merge(final_table, tmp, all = TRUE)
    }
  }
  return(final_table)
}

### '96_대덕전자(주)'
sample96 <- as.character(kospi200$링크[96]); kospi200[96,]   # 대덕전자(주)   
### 사전 정의가 필요한 내용
unit <- 1000   # 금액단위
list_TF <- c(FALSE, TRUE, TRUE, FALSE)   
### 함수로 최종 대덕전자에 대한 table 얻어내기
daedug_table <- case2_table(path=sample96)




############################## 2-3. case3 당기/전기 하에 category 구분까지 되어 있는 형태 ##################################

### case3 해당 함수
case3_table <- function(path){
  list_df <- get_table(path)
  
  final_table <- data.frame()
  tmp <- data.frame()
  
  for(i in 1:(length(list_df))){
    if(list_TF[i] == TRUE){
      ### list_df의 테이블을 tmp에 저장
      tmp <- list_df[[i]]
      if(i==7){ ### table7의 예외 경우!!!!!!!!!!!!!!!
        tmp <- tmp[c(2,1,3:nrow(tmp)),]
      }      
      ### col, row 이름 명명하기
      col_name1 <- text_clean(ifelse(grepl("당", tmp[1,]), "당기", ifelse(grepl("전", tmp[1,]), "전기", "구분")))
      col_name2 <- text_clean(tmp[2,])
      col_name <- paste0(col_name1, "@", col_name2)
      ### tmp clean 작업
      tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)))
      colnames(tmp) <- col_name
      tmp <- tmp[start_r:nrow(tmp), ]  
      ### 테이블 틀 만들기
      tmp <- melt(tmp, id=c(colnames(tmp)[1], colnames(tmp)[2]))
      tmp$value <- as.numeric(gsub(",", "", tmp$value)) * unit 
      col_ca <- t(as.data.frame(str_split(tmp$variable, "@")))
      tmp$당기_전기 <- col_ca[,1]; tmp$col_var <- col_ca[,2]
      tmp <- tmp[,-3]
      tmp <- dcast(tmp, get(colnames(tmp)[1]) + get(colnames(tmp)[2]) + 당기_전기 ~ col_var)
      colnames(tmp)[1:2] <- c("구분", "회사명")
      ### category를 다시 만들어서 table 재정의_row
      tmp <- tmp[!str_detect(tmp[,2], paste(c("소계", "합계", "기타"),collapse = '|')),]
      rownames(tmp) <- NULL
    } 
    ### table 합치기
    final_table <- merge(final_table, tmp, all = TRUE)
    if(nrow(final_table)==0){
      final_table <- merge(final_table, tmp, all = TRUE)
    }
  }
  final_table <- aggregate(final_table[,4:ncol(final_table)], by=list(final_table$구분, final_table$회사명, final_table$당기_전기), sum, na.rm=TRUE, na.action=NULL)
  colnames(final_table)[1:3] <- c("구분", "회사명", "당기_전기")
  return(final_table)
}

### '2_(주)DB하이텍'
sample2 <- as.character(kospi200$링크[2]); kospi200[2,]   # (주)DB하이텍
### 사전 정의가 필요한 내용
unit <- 1000000   
list_TF <- rep(FALSE, 14); list_TF[c(4,6,7)] <- TRUE   
start_r=3
### 함수로 최종 (주)DB하이텍에 대한 table 얻어내기
dbhitech_table <- case3_table(path=sample2)


############################## 2-4. case4 column이 당기/전기, row가 category인 형태 ##################################

### case4 해당 함수
case4_table <- function(path){
  list_df <- get_table(path)
  final_table <- data.frame()
  tmp <- data.frame()
  
  for(i in 1:(length(list_df))){
    if(list_TF[i] == TRUE){
      ### list_df의 테이블을 tmp에 저장
      tmp <- list_df[[i]]
      ### col, row 이름 명명하기
      if(colname_case == 1){
        colnames(tmp) <- text_clean(ifelse(grepl("당", colnames(tmp)), "당기", ifelse(grepl("전", colnames(tmp)), "전기", "구분")))
      } else if (colname_case == 2){
        colnames(tmp) <- text_clean(ifelse(grepl("당", tmp[1,]), "당기", ifelse(grepl("전", tmp[1,]), "전기", tmp[1,])))
      }
      tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)))
      if(cate_case == 2){
        tmp[,3] <- paste0(tmp[,3], "@", tmp[,4]); tmp <- tmp[,-4]
      }
      id1 <- colnames(tmp)[1]; id2 <- colnames(tmp)[2]; id3 <- colnames(tmp)[3]
      ### 테이블 틀 만들기
      tmp <- tmp[start_r:nrow(tmp),]
      tmp <- melt(tmp, id=c(id1, id2, id3))
      tmp$value <- as.numeric(gsub(",", "", tmp$value)) * unit 
      tmp <- dcast(tmp, get(id1) + get(id2) + variable ~ tmp[,3], sum)
      colnames(tmp)[1:3] <- c("구분", "회사명", "당기_전기")
      ### category를 다시 만들어서 table 재정의_row
      tmp <- tmp[!str_detect(tmp[,2], paste(c("소계", "합계", "기타"),collapse = '|')),]
      rownames(tmp) <- NULL
    } 
    ### table 합치기
    final_table <- merge(final_table, tmp, all = TRUE)
    if(nrow(final_table)==0){
      final_table <- merge(final_table, tmp, all = TRUE)
    }
  }
  final_table <- aggregate(final_table[,4:ncol(final_table)], by=list(final_table$구분, final_table$회사명, final_table$당기_전기), sum, na.rm=TRUE, na.action=NULL)
  colnames(final_table)[1:3] <- c("구분", "회사명", "당기_전기")
  return(final_table)
}


### '3_(주)KB금융지주'
sample3 <- as.character(kospi200$링크[3]); kospi200[3,]   # (주)KB금융지주
### 사전 정의가 필요한 내용
unit <- 1000000   # 금액단위
list_TF <- rep(FALSE ,7); list_TF[1:2] <- TRUE  
start_r <- 1
cate_case <- 1; colname_case <- 1
### 함수로 최종 대덕전자에 대한 table 얻어내기
kbfinan_table <- case4_table(path=sample3)


### '4_(주)강원랜드'
sample4 <- as.character(kospi200$링크[4]); kospi200[4,]   # (주)강원랜드
### 사전 정의가 필요한 내용
unit <- 1000   # 금액단위
list_TF <- rep(FALSE ,4); list_TF[3:4] <- TRUE   
start_r <- 2
cate_case <- 2; colname_case <- 2
### 함수로 최종 (주)강원랜드에 대한 table 얻어내기
gangwon_table <- case4_table(path=sample4)



############################## 참고 ##################################

# ### '7_(주)농심' -> case1 + case3
# sample9 <- as.character(kospi200$링크[7]); kospi200[7,]
# ### 사전 정의가 필요한 내용
# unit <- 1000
# list_TF <- rep(FALSE, 16); list_TF[c(4,6,8,10,12)] <- TRUE
# list_당전 <- rep(NA, 16); list_당전[c(4,8)] <- "당기"; list_당전[c(6,10)] <- "전기"; list_df[12] <- "당기/전기"
# start_r=6; start_c = 2


# ### '8_(주)대교'-> case1 (당기전기 구분이 년도로 되어있음), 5번째 테이블은 case3 형태
# sample10 <- as.character(kospi200$링크[8]); kospi200[8,]
# ### 사전 정의가 필요한 내용
# unit <- 1000000   # 금액단위
# list_TF <- rep(FALSE, 8); list_TF[3:7] <- TRUE   
# list_당전 <- c(NA, NA, "당기", "전기", "당기/전기", "당기", "전기", NA)
# start_r = 2; start_c = 3



############################## 3. 불러들인 자료 엑셀로 저장하기 ##################################
list_df <- list(ls(pattern = "table"))
n <- length(list_df)
write.xlsx(list_df[[1]], file = "example_7.xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:n)
for (j in 2:n){
  write.xlsx(list_df[[j]], file = "example_7.xlsx", sheetName = name_tmp[j-1], append=TRUE)
}

