
########################################### 0. 환경설정 ##################################################
if(!require(rvest)){install.packages('rvest')}; library(rvest)
if(!require(stringr)){install.packages('stringr')}; library(stringr)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(xlsx)){install.packages('xlsx')}; library(xlsx)

### web page 불러오기
ind=77   # 첫번째 회사 불러오기
kospi200 <- read.csv(file='KOSPI200_web.csv')
url <- as.character(kospi200$링크[ind]); url

### 웹크롤링 코드
html <- read_html(url)
node_document <- html_nodes(html, "body")   

### 데이터확인작업
#checkpoint <- node_document %>% html_nodes("table")
#checkpoint[[ind]] %>% html_text(); kospi200$회사명[1]


######################################## step 1. 특수관계자 파트의 테이블 번호 알아내기 #######################################
document <- as.character(node_document %>% html_text())

### 특수관계자 파트 구분자 알아내기 
text <- str_extract_all(document, '[:digit:]+[.][:space:][가-힣]+'); text
text_which <- which(str_detect(text[[1]], '특수관계자'))
start_text <- text[[1]][text_which]; start_text
end_text <- text[[1]][text_which+1]; end_text

### 특수관계자 파트에 해당하는 테이블 번호 알아내기 
tmp_1 <- str_split(document, start_text)   # start_text 단어를 기준으로 split 해서 시작번호 알아내기
text_1 <- str_split(tmp_1[[1]][1], '<table')   
start <- length(text_1[[1]]) + 1; start
tmp_2 <- str_split(document, end_text)   # end_text 단어를 기준으로 split 해서 끝번호 알아내기
text_2 <- str_split(tmp_2[[1]][1], '<table')
end <- length(text_2[[1]]) - 1; end


######################################## step 2. 해당되는 테이블 리스트 저장하기 #######################################

### 해당되는 부분 테이블 불러오기 
table_set <- node_document %>% html_nodes("table")   #웹크롤링 코드
list_df <- list()
for (i in start:end){
  Sys.setlocale("LC_ALL", "English")
  table_tmp <- html_table(table_set[[i]], fill=TRUE, header = NA)
  Sys.setlocale("LC_ALL", "Korean")
  idx <- i - start + 1
  list_df[[idx]] <- table_tmp
}

list_df[[2]]

######################################## step3. 불러들인 자료 엑셀로 저장하기 #######################################
n <- length(list_df)
write.xlsx(list_df[[1]], file = "example_list.xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:n)
for (j in 2:n){
  write.xlsx(list_df[[j]], file = "example_list.xlsx", sheetName = name_tmp[j-1], append=TRUE)
}



