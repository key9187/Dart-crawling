
########################################### 0. 환경설정 ##################################################
if(!require(rvest)){install.package('rvest')}; library(rvest)
if(!require(stringr)){install.package('stringr')}; library(stringr)
if(!require(dplyr)){install.package('dplyr')}; library(dplyr)
if(!require(xlsx)){install.package('xlsx')}; library(xlsx)

url = 'http://dart.fss.or.kr/report/viewer.do?rcpNo=20190401000498&dcmNo=6597259&eleId=4&offset=70645&length=308210&dtd=dart3.xsd'
html <- read_html(url)

##################################### case 1. 문서 전부 불러오기 ############################################
document <- html_nodes(html, "body")
tmp_1 <- str_split(xml_text(document[[1]]), '34. 특수관계자')
tmp_2 <- str_split(tmp_1[[1]][2], '35. 현금흐름표')
data_tmp <- tmp_2[[1]][1]
data_tmp <- html_table(data_tmp)

################################ case 2. 데이터테이블 그대로 불러오기  #####################################
table_set= html_nodes(html, "body") %>% html_nodes("table")
table1 = html_table(table_set[[74]], fill=TRUE)
table2 = html_table(table_set[[75]], fill=TRUE, header = FALSE)

#################################### case 3. 방법 혼합하기 ##################################################
### step1. 읽어들일 테이블 번호 알아내기
node_document <- html_nodes(html, "body")   #웹크롤링 코드
document <- as.character(node_document)
tmp_1 <- str_split(document, '34. 특수관계자')   #'34. 특수관계자' 단어를 기준으로 split 해서 시작번호 알아내기
text_1 <- str_split(tmp_1[[1]][1], '<table')   
start <- length(text_1[[1]]) + 1
tmp_2 <- str_split(document, '35. 현금흐름표')   #'35. 현금흐름표' 단어를 기준으로 split 해서 끝번호 알아내기
text_2 <- str_split(tmp_2[[1]][1], '<table')
end <- length(text_2[[1]]) - 1

### step2. 해당되는 테이블 저장하기
table_set <- node_document %>% html_nodes("table")   #웹크롤링 코드
list_df <- list()
for (i in start:end){
  table_tmp <- html_table(table_set[[i]], fill=TRUE, header = FALSE)
  idx <- i - start + 1
  list_df[[idx]] <- table_tmp
}

### step3. 불러들인 자료 엑셀로 저장하기 
n <- length(list_df)
write.xlsx(list_df[[1]], file = "example_list.xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:n)
for (j in 2:n){
  write.xlsx(list_df[[j]], file = "example_list.xlsx", sheetName = name_tmp[j-1], append=TRUE)
}


