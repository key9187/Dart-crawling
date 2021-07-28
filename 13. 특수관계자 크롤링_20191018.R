######################################## 0. 환경설정 #######################################
if(!require(rvest)){install.packages('rvest')}; library(rvest)
if(!require(stringr)){install.packages('stringr')}; library(stringr)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(reshape2)){install.packages('reshape2')}; library(reshape2)
if(!require(zoo)){install.packages('zoo')}; library(zoo)
if(!require(xlsx)){install.package('xlsx')}; library(xlsx)
if(!require(data.table)){install.package('data.table')}; library(data.table)

kospi200 <- read.csv(file='KOSPI200_web.csv')
alpha_code <- read.csv(file='world_alphabetic_code.csv')
alpha_code <- as.character(alpha_code[,3])

############################## step 1. 필요한 함수 정의 ##################################

### 파이썬에서 크롤링한 결과불러오는 함수 정의 
python_result_read <- function(number, start, end){
  file_link <- paste0('sample', number, '.xlsx')
  result_dat = list()
  for(ind in (start-1):(end-1)){
    tmp_dat <- read.xlsx(file_link, sheetName = paste0("sheet", ind), encoding = "UTF-8")
    tmp_dat <-  as.data.frame(lapply(tmp_dat, as.character), stringsAsFactors = F)
    colnames(tmp_dat) <- as.character(tmp_dat[1,])
    result_dat[[(ind-start+2)]] <- tmp_dat[-1,]
  }
  return(result_dat)
}

### 해당하는 table start, end 번호, unit_data 알아내는 함수 정의
get_startend <- function(url, case_tmp){
  
  html <- read_html(url)
  node_document <- html_nodes(html, "body")   
  
  # 특수관계자 목차 알아내기
  document <- as.character(node_document)
  document <- gsub("\\s","", document)
  document <- gsub('</span>', '', gsub('<span>', '', document))
  document <- gsub(":", "", gsub('"', "", document))
  document <- gsub('<spanstyle=font-weightbold;>', "", document)
  document <- gsub("<spanstyle=font-size11pt;line-height1.6em;font-weightbold;>","", document)
  document <- gsub("<spanstyle=text-decorationunderline;><spanstyle=text-decorationunderline;>","", document)
  
  text <- str_extract_all(document, '[0-9]+[.]+[가-힣]+') # 특수관계자 파트 구분자 알아내기 
  text <- text[[1]][!duplicated(text[[1]])]   #중복값제거
  text <- subset(text, (nchar(text) <= 23) & (as.numeric(str_extract(text, "[0-9]*")) > 5) )
  text_which <- which(str_detect(text, '특수관계자'))
  if(length(text_which) == 1){
    start_text <- text[text_which]
    end_text <- text[which(str_detect(text, paste0(as.numeric(str_extract(text[text_which[1]], "[0-9]*")) + 1,".") ))]
    } else if(length(text_which) == 2){
      if(as.numeric(str_extract(text[text_which[1]+1], "[0-9]*")) - as.numeric(str_extract(text[text_which[1]], "[0-9]*")) == 1){
        start_text <- text[text_which[1]]; end_text <- text[text_which[1]+1]
        document <- gsub(text[text_which[2]], "", document)
      } else if(as.numeric(str_extract(text[text_which[2]+1], "[0-9]*")) - as.numeric(str_extract(text[text_which[2]], "[0-9]*")) == 1){
        start_text <- text[text_which[2]]; end_text <- text[text_which[2]+1]
        document <- gsub(text[text_which[1]], "", document)
      }
  } else if(length(text_which) >= 3){
    print("error:: text_which 특수관계자 수 3개 이상 in get_startend")
  }
  
  # 특수관계자 파트에 해당하는 테이블 번호 알아내기 
  tmp_1 <- str_split(document, start_text)   # start_text 단어를 기준으로 split 해서 시작번호 알아내기
  text_1 <- str_split(tmp_1[[1]][1], '<table')
  start <- length(text_1[[1]])
  # 특수관계자 이후 번호가 없을/있음 경우
  if(length(end_text) != 0){
    tmp_2 <- str_split(document, end_text)   # end_text 단어를 기준으로 split 해서 끝번호 알아내기
    text_2 <- str_split(tmp_2[[1]][1], '<table')
    end <- length(text_2[[1]]) - 1
  } else if(length(end_text) == 0){
    end <- length(str_split(document, '<table')[[1]]) - 1
  } else {print("error:: end_text error in get_startend")}
  
  # 금액단위용 데이터 
  unit_data <- str_split(tmp_1[[1]][2], '단위')
  unit_data <- unit_data[[1]][2]
  
  # 해당되는 부분 테이블 리스트로 저정하기 
  table_set <- node_document %>% html_nodes("table")   #웹크롤링 코드
  list_df <- list()
  for (i in start:end){
    Sys.setlocale("LC_ALL", "English")
    table_tmp <- html_table(table_set[[i]], fill=TRUE, header = NA)
    Sys.setlocale("LC_ALL", "Korean")
    idx <- i - start + 1
    list_df[[idx]] <- table_tmp
  } # case_tmp가 1이면 list_df를 출력하고 case_tmp가 2이면 tmp_1을 출력
  if(case_tmp == 1){
    return(c(start, end))
  } else if(case_tmp == 2){
    return(unit_data)
  }
}

### 기업명 텍스트 정제하는 함수 정의 
text_clean <- function(tmp_text){
  tmp_clean <- gsub("\\s", "", gsub("[(]+주+[0-9]+[)]", "", as.character(tmp_text)))
  tmp_clean <- gsub('\\[', '', gsub('\\]', '', tmp_clean))
  tmp_clean <- gsub("[(]+\\*+[0-9][)]+[,]+[(]+\\*+[0-9][)]", "", tmp_clean)
  tmp_clean <- gsub("[(][*][0-9][)]", "", tmp_clean)
  tmp_clean <- gsub("[(]+\\*+[)]", "", tmp_clean)
  tmp_clean <- gsub("\\:", "", tmp_clean)
  tmp_clean <- gsub("..U.00A0.", "", tmp_clean)
  tmp_clean <- gsub("<U+00A0>", "", tmp_clean)
  tmp_clean <- gsub("<", "", tmp_clean)
  tmp_clean <- gsub(">", "", tmp_clean)
  return(tmp_clean)
}

### (사전정의3) 숫자형변수 starting point 정의 
list_start_fun <- function(tmp_table){
  # tmp_table을 행단위 5개 이하로 자르기
  tmp_table <- tmp_table[1:5,]
  # 먼저 table에서 -를 0으로 대체하고 특수문자를 제거함
  tmp_table_n = as.data.frame(lapply(tmp_table, function(x) gsub("[-]", 0, x)))
  tmp_table_n = as.data.frame(lapply(tmp_table_n, function(x) gsub("[[:punct:]]", "", x)))
  # 숫자로 이루어진 경우 TRUE, 아닌경우 FALSE인 테이블 정의
  tmp_tf = as.data.frame(lapply(tmp_table_n, function(x) str_detect(x, "^[0-9]+$")))
  # 처음으로 TRUE가 나온 행과 열 출력하여 저장
  tmp_c = (which(tmp_tf==TRUE)[1] - 1) %/% nrow(tmp_tf)
  tmp_r = which(tmp_tf==TRUE)[1] - (tmp_c * nrow(tmp_tf))
  # 결과물 출력
  return(c(tmp_r, tmp_c+1))
}

### (사전정의1) 각 테이블 필요성 결정해주는 함수 정의
list_TF_fun <- function(list_data, case_TF=F){
  # 제외 단어(이중적단어): 현금출자, 자금회수, 계약부채, 대여금
  # 꼭 포함되어야 할 단어: 매출 등, 제상품매출, (미지급금, 채권, 채무),
  tmp_list_t <- c("지급수수료", "비용", "예치금","기타채권", "매출채권", "제상품매출", "상품매출", "수익",
                  "미사용약정", "기타자산", "손실충당금", "대출채권", "매입채무", "매출등", "매입등", "유가증권취득",
                  "채권등", "채무등", "재고매입", "광고비", "일반관리비", "기타영업손익", "기타자산", "유형자산", 
                  "재화의매입", "재화의판매", "사업수익", "지분매각", "지분매입", "매출거래", "매입거래", "무형자산",
                  "유무형자산취득", "배당금수령", "고정자산매입", "기타일반관리비", "기타수익", "미지급금", "채권", "채무",
                  "자금대여", "장부금액", "현금출자", "자금거래", "계약자산", "금융리스", "주식배당", "매출", "매입")
  
  tmp_list_f <- c("급여", "주석", "매도가능금융자산의평가", "채권자", "대출채권유동화", "담보권자", "여신금액",
                  "외화단위", "보증기간", "조정사항", "무상할당", "이행보증", "채권최고액", "예금가입", "출자대상회사",
                  "USD", "담보설정액", "공동기업주식매각", "대손상각비", "관계기업현금출자", "지분증권",
                  "잔여재산분배", "수익인식시점", "상각후원가", "보고된금액", "자동연장", "임직원대여금",
                  "매입확약", "자문수수료비용", "투자일임계약잔고금액", "변경되었습니다", "계약기간", "채무잔액이있는",
                  "어음배서보증", "사채매입", "책임준공보증", "환율변동효과", "현금및현물출자", "총보증한도")
  
  list_TF <- c()
  for(i in 1:length(list_data)){
    if(length(list_data[[i]]) <= 2){list_TF[i] = FALSE} else {list_TF[i] = TRUE}
    if(!any(str_detect(colnames(list_data[[i]]), "단위"))){list_data[[i]] <- rbind(colnames(list_data[[i]]), list_data[[i]])}
    list_data[[i]] <- as.data.frame(lapply(list_data[[i]], function(x) text_clean(gsub("UA","", gsub("\\s", "", 
                                          gsub("[0-9]","",gsub("\\.","",x)))))), stringsAsFactors = F)
    if( (nrow(list_data[[i]]) >= 3) & (i==1) ){
      if((i==1) & any(str_detect(list_data[[i]][3:nrow(list_data[[i]]),ncol(list_data[[i]])], "회사"), na.rm=T)){list_TF[i] = FALSE}
    }
  }
  list_TF <- list_TF & str_detect(list_data, paste(tmp_list_t, collapse = "|")) & 
    (!str_detect(list_data, paste(tmp_list_f, collapse = "|")))
  # 외화 통화 코드 있는 테이블 따로 저장하기
  for(i in 1:length(list_data)){
    if(sum(str_detect(as.character(list_data[[i]][,-1]), alpha_code), na.rm=T) >= 2){list_TF[i] = "except"}
  }
  if(case_TF == T){
    return(tmp_list_t)
  } else{
    return(list_TF)
  }
}

### (사전정의2) 각 테이블 필요성 결정해주는 함수 정의
list_class_fun <- function(list_TF_vec, list_case_vec, list_data){
  
  list_class <- c()
  
  for(i in 1:length(list_TF_vec)){
    ### column name을 마지막 행으로 추가
    list_data[[i]] <- rbind(list_data[[i]], colnames(list_data[[i]]))
    
    ### 지급보증 케이스
    if(list_TF_vec[i]==TRUE & any(str_detect(list_data[[i]], paste(c("지급보증", "보증처", "보증제공처", "보증금액", "보증기간"), collapse = "|")))){
      list_class[i] <- "보증"
    } ### 담보제공
    else if(list_TF_vec[i]==TRUE & any(str_detect(list_data[[i]], paste(c("담보제공"), collapse = "|")))){
      list_class[i] <- "담보"
    } ### 당기/전기      
    else if(list_TF_vec[i]==TRUE){
      list_class[i] <- "당기/전기" 
    } ### 해당없음
    else if(list_TF_vec[i] == FALSE){
      list_class[i] <- NA
    }
  }
  
  list_class <- ifelse(list_case_vec %in% c("case1", "case1_3"), list_class, NA)
  tmp_class <- grep("당기/전기", list_class)
  
  if(length(tmp_class) %% 2 == 1){print("error:: class error in list_class_fun")} else{
    for(idx in 1:length(tmp_class)){
      list_class[tmp_class[idx]] <- ifelse(idx %% 2 == 1, "당기", "전기")
    }
  }
  
  return(list_class)
}

### (사전정의4) 각 기업의 금액단위 정의 
list_unit_fun <- function(list_data, index_tmp, unit_data_input){
  tmp <- list_data[[index_tmp]]
  if(!any(str_detect(colnames(tmp), "단위"))){
    #표에 단위에 대한 내용이 없을 경우 수행 
    unit_data <- substr(unit_data_input, 1, 5)
  } else if(any(str_detect(colnames(tmp), "단위"))){
    #표에 단위에 대한 내용이 있을 경우 수행
    unit_data <- colnames(tmp)
  } else{print("error:: no text '단위' in list_unit_fun function!")}
  
  if(any(str_detect(substr(unit_data, 1, 10), "천원"))==TRUE){unit <- 1000
  } else if(any(str_detect(substr(unit_data, 1, 10), "백만원"))==TRUE){unit <- 1000000
  } else if(any(str_detect(substr(unit_data, 1, 10), "원"))==TRUE){unit <- 1
  } else {print("error:: unit range error in list_unit_fun function!")}
  
  return(unit)
}

### (사전정의5) 각 테이블이 해당하는 case 출력해주는 함수 
list_case_fun <- function(tmp_table, col_name, start_c, tmp_list_t){
  tmp_list_t_new <- tmp_list_t[!(tmp_list_t %in% c("채권", "대출채권"))]
  tmp_list_t_new <- c(tmp_list_t_new, "현금출자")
  
  if(!(all(str_detect(paste(col_name, collapse = ""), c("당기", "전기")))
       | all(str_detect(paste(col_name, collapse = " "), c("\\(당\\)기", "\\(전\\)기")))
       | all(str_detect(paste(col_name, collapse = ""), c("2018", "2017"))) 
       | all(str_detect(paste(col_name, collapse = ""), c("당.기", "전.기"))) )){tmp_case <- "case1"
  } else if(any(str_detect(col_name, paste(tmp_list_t_new, collapse = '|'))) == TRUE){tmp_case <- "case2"
  } else if(any(str_detect(tmp_table[,(start_c-2):(start_c-1)], paste(tmp_list_t_new, collapse = '|'))) == TRUE) {tmp_case <- "case3"
  } else {print(paste("error:: no case in list_case_fun function in ", i))}
  
  # case1 내에 case3인 경우 case1_3으로 재검사
  if(tmp_case == "case1"){
    if(any(str_detect(tmp_table[,(start_c-1)], paste(tmp_list_t_new, collapse = '|')), na.rm = T) == TRUE) {tmp_case <- "case1_3"}
  }
  return(tmp_case)
}

### (사전정의6) col_name 정제하는 함수
colname_clean <- function(tmp_table, tmp_r){
  #col_name에 해당하는 table 생성하고 text_clean 수행
  tmp_colname_table <- tmp_table[1:(tmp_r-1),]
  ### tmp_colname_table '.', 숫자, 영어 없애기
  tmp_colname_table <- as.data.frame(lapply(tmp_colname_table, function(x) gsub("2018", "당기", gsub("2017", "전기", x))), stringAsFactors = F)
  tmp_colname_table <- as.data.frame(lapply(tmp_colname_table, function(x) gsub("\\.","",gsub("[0-9]","",gsub("[A-z]","",x)))), stringsAsFactors = F)
  ### tmp_colname_table 당기/전기 한 종류만 있으면 해당하는 행 삭제
  if(sum(c("당기", "전기") %in% colnames(tmp_colname_table)) == 1){
    tmp_colname_table <- tmp_colname_table[-1,]}
  ### table 첫열 값을 NA처리
  tmp_colname_table[,1] <- NA
  
  # 각 table 행마다 ("구분", "회사명", "기업")을 제외한 table 재생성
  tmp_colname_list <- list()
  for(j in 1:nrow(tmp_colname_table)){
    tmp_j <- as.character(tmp_colname_table[j,])
    tmp_colname_list[[j]] <- tmp_j[!(tmp_j %in% c("구분", "회사명", "기업", "특수관계자", "계정과목명", "유형", "특수관계자구분",
                                       "특수관계자명", "기업명", "특수관계구분", "기업명주", "특수관계자의성격",
                                      "회사의명칭", "값없음", "계정과목", "알수없음", "회사", "관계", "상대회사",
                                      "특수관계자의명칭", "대상회사", "특수관계자범주", "특수관계범주", "특수관계자성격",
                                      NA, "NA", "상대회사명"))]
  }
  tmp_colname_table <- as.data.frame(tmp_colname_list, stringsAsFactors = F)   # 여기서 오류나면 list 길이가 맞지 않는 것!
  # 최종적으로 tmp_colname 생성
  tmp_colname <- c()
  for(jj in 1:length(tmp_colname_table)){
    tmp_colname <- paste0(tmp_colname, tmp_colname_table[,jj], "@")
  }
  return(tmp_colname)
}


############################## step 2. 최종함수 dart_get 함수 정의 ##################################
# path <- as.character(kospi200$링크[168]); index = 168
# 확인필요: 168/추후확인: 신한금융지주회사, 유니드, 삼성전기, 한세실업, 한국단자공업
# 169, 172, 177: 새로운유형 정의 필요
### dart_get 함수 
dart_get <- function(path, index){
  ### 크롤링 함수
  startend <- get_startend(path, 1)
  list_df <- python_result_read(number = index-1, start = startend[1], end = startend[2])
  for(i in 1:length(list_df)){
    list_df[[i]] <-  as.data.frame(lapply(list_df[[i]], as.character), stringsAsFactors = F)
  }
  
  ### 사전정의 변수 - 필요 테이블 여부
  list_TF <- list_TF_fun(list_df)
  
  list_case <- rep(NA, length(list_df))
  for(i in 1:length(list_df)){
    if(list_TF[i] == TRUE){
      ### list_df의 테이블을 tmp에 저장
      tmp <- list_df[[i]]
      
      ### tmp text_clean 수행
      tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)), stringsAsFactors = F)
      colnames(tmp) <- text_clean(colnames(tmp))
      ### 특이케이스 제거 !!! - 구분, 회사명 외에 필요없는 구분자가 있을 경우 제거
      # tmp <- tmp[,!str_detect(colnames(tmp), "거래내용")]   #향후수정필요
      ### 2017, 2018 -> 전기, 당기
      tmp[1,] <- gsub("2018", "당기", gsub("2017", "전기", tmp[1,]))
      if(nrow(tmp) >= 2){ tmp[2,] <- gsub("2018", "당기", gsub("2017", "전기", tmp[2,]))}
      ### 2열에 거래내역, 3열에 회사명 있으면 2,3열 바꾸기
      if(colnames(tmp)[2:3] == c("거래내역", "회사명")){
        tmp <- tmp[c(1,3,2,4:ncol(tmp))]
      }
      ### 첫열에 계, 합계 텍스트 있는 행 제거 
      tmp <- tmp[!ifelse(tmp[,1] %in% c("계", "합계", "소계", "채권", "채무", "채무합계", "채권합계", "수익", "비용",
                                        "매출등합계", "매입등합계", "당기", "전기", "당기말", "전기말"), TRUE, FALSE),]

      ### colnames가 단위에 대한 내용이 아니라면 첫행으로 추가하기
      if(!any(str_detect(colnames(tmp), "단위"))){
        tmp <- rbind(colnames(tmp), tmp)}
      
      ### 구분에 대한 첫번째 케이스 - 구분 항목이 행으로 들어가 있는 경우 NA 없애기
      start_c_tmp <- list_start_fun(tmp)[2]
      tmp_col1 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,1]))))
      if(nrow(tmp) >= 2){ tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2])))) } else{
        tmp_col2 <- FALSE }
      if(nrow(tmp) >= 3){ tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3])))) } else{
        tmp_col3 <- FALSE }
      sub_name_tmp <- c("종속기업", "관계기업", "지배기업", "기타특수관계기업",
                        "기타특수관계자", "공동기업", "관계기업및공동기업","대규모기업집단계열회사","대규모기업집단계열회사등",
                        "당사에유의적영향력을행사하는기업", "유의적인영향력을행사할수있는투자자","대기업기업집단계열회사등",
                        "당해기업에중대한영향력을미치는회사", "당사에중요한영향력을행사하는회사", "당사의종속기업", 
                        "유의적영향력을행사하는회사", "기타의특수관계자","상위지배기업의공동기업", "상위지배기업및그종속기업",
                        "종속,관계기업", "대규모기업집단", "신규설립으로인한현금출자및유상증자", "유의적인영향력을행사하는기업")
      
      if( ((start_c_tmp == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
          ((start_c_tmp == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
          ((start_c_tmp == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
        narow_which <- which(tmp_col1 %in% sub_name_tmp)
        for(ii in 1:length(narow_which)){
          tmp[narow_which[ii],is.na(tmp[narow_which[ii],])] <- "값없음"
        }
      }
      
      ### 정리된 테이블에 밀림현상있는지 체크하고 수정(순서바꾸지말것!!!)
      if(( any(is.na(tmp[1:(ifelse(nrow(tmp)<5, nrow(tmp),5)),ncol(tmp)])) & 
             any(str_detect(tmp[,1:(list_start_fun(tmp)[2]-1)], "[0-9]+[,]*")) ) | 
         ( any(is.na(tmp[1:(ifelse(nrow(tmp)<5, nrow(tmp),5)),ncol(tmp)])) & 
           any(is.na(tmp[1:(list_start_fun(tmp)[1]-1),])) ) == TRUE){
        # 밀린데이터 원위치
        # tmp=rbind((tmp[which(tmp[,1]=="특수관계자"),])[,2:ncol(tmp)], (tmp[which(tmp[,1]!="특수관계자"),])[,1:(ncol(tmp)-1)]) # 예외상황
        number_na <- which(!complete.cases(tmp))
        for(ind in 1:length(number_na)){
          rowsum_na <- sum(is.na(tmp[number_na[ind],]))  # 해당 행에 na 갯수 rowsum_na
          tmp[1,] <- gsub("\\s", "", gsub("[0-9]", "", gsub("\\.","",tmp[1,])))
          # text일경우, 숫자일경우 기준 'standard_na'을 다르게 정해야함 
          if(str_detect((list_start_fun(tmp[number_na[ind],])[2]),'[0-9]') %in% c(TRUE) ){ # 숫자일 경우
            standard_na <- list_start_fun(tmp[number_na[ind],])[2] 
          } else if(is.na(list_start_fun(tmp[number_na[ind],])[2]) ) { # text일 경우
            standard_na <- which((str_detect(tmp[c(1:3,number_na[ind]),], paste(c("구분", "회사명","회사의명칭","기업명",
                                                                                 "값없음","알수없음"), collapse = '|')) & 
                                   !(str_detect(tmp[c(1:3,number_na[ind]),], paste(list_TF_fun(list_df, T), collapse = '|')))) == FALSE)[1]
          }
          tmp[number_na[ind],(standard_na+rowsum_na):ncol(tmp)] <- tmp[number_na[ind],standard_na:(ncol(tmp)-rowsum_na)]
          tmp[number_na[ind],standard_na:(standard_na+rowsum_na-1)] <- "값없음"
        }
      }
      
      # 첫번째 케이스 : 구분 항목이 행으로 들어가 있는 경우 행렬변환
      start_c_tmp <- list_start_fun(tmp)[2]
      tmp_col1 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,1]))))
      if(nrow(tmp) >= 2){ tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2])))) } else{
        tmp_col2 <- FALSE }
      if(nrow(tmp) >= 3){  tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3])))) } else{
        tmp_col3 <- FALSE }
      
      if( ((start_c_tmp == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
          ((start_c_tmp == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
          ((start_c_tmp == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
        insert_col <- na.locf(ifelse(str_detect(tmp_col1, paste(sub_name_tmp, collapse = '|')) == TRUE, tmp_col1, NA),
                              na.rm=FALSE, fromLast = FALSE)
        insert_col[is.na(insert_col)] <- "값없음"
        tmp <- cbind(insert_col, tmp)
        tmp <- tmp[!(tmp_col1 %in% sub_name_tmp),]
        colnames(tmp)[1:2] <- c("구분", "기업명")
        tmp[,1] <- as.character(tmp[,1])
      }
      
      # 세번째 케이스 : 구분이 따로 없고 앞선테이블에서 정의되지 않은 케이스 - '알수없음'처리
      if((start_c_tmp == 2) & (!any(sub_name_tmp %in% tmp_col1))|
         ((start_c_tmp == 3) & any(list_TF_fun(list_df,T) %in% tmp_col2) & (!any(sub_name_tmp %in% tmp_col2))) ){
        tmp$구분 <- "알수없음"
        tmp <- tmp[c(ncol(tmp), 1:(ncol(tmp)-1))]
        colnames(tmp)[1:2] <- c("구분", "기업명")
      }
      
      ### 특수관계자성격 열이 2번째 있을때 순서 바꿔주기
      start_r_tmp <- list_start_fun(tmp)[1]
      if(any(sub_name_tmp %in% tmp[,2])){
        tmp[start_r_tmp:nrow(tmp),2] <- na.locf(ifelse(str_detect(tmp[start_r_tmp:nrow(tmp),2], paste(sub_name_tmp, 
                            collapse = '|')) == TRUE, tmp[start_r_tmp:nrow(tmp),2], NA), na.rm=FALSE, fromLast = FALSE)
        tmp <- tmp[,c(2,1,3:ncol(tmp))]
      }
      ### 2열에(기업명) 자리에 특정 문자 속해있을 때 해당 열 삭제하기
      if(any(tmp[,2] %in% c("대규모기업집단계열회사")) ==TRUE){
        tmp <- tmp[,-2]
      }
      
      ### 두번째열에 계, 합계, 기타 텍스트 있는 행 제거 
      tmp <- tmp[!ifelse(tmp[,2] %in% c("계", "합계", "소계", "기타"), TRUE, FALSE),]
      
      ### col_name_tmp 명명하기
      start_c_tmp <- list_start_fun(tmp)[2]
      start_r_tmp2 <- list_start_fun(tmp)[1]
      col_name_tmp <- c("구분", "기업명", colname_clean(tmp, start_r_tmp2))
      
      ### 테이블 케이스 벡터 정의하기
      list_case[i] <- list_case_fun(tmp, col_name_tmp, start_c_tmp, list_TF_fun(list_df, T))
    }
  }
  
  # case1과 case1_3 합계 갯수가 짝수가 아니라면 오류 출력
  if(length(which(list_case %in% c("case1", "case1_3"))) %% 2 == 1){
    for(jj in 1:length(list_case)){
      if((all(c("기초", "대여", "회수", "기말") %in% as.matrix(list_df[[jj]])) & 
          (list_case[jj] == "case1")) %in% c(TRUE)) {
        list_case[jj] <- NA; list_TF[jj] <- FALSE}  
      # case1 내에 "자금대여" 있고 '당기/전기' 구분 없을 경우 case4
      if( (list_case[jj] == "case1") & (any(str_detect(rbind(colnames(list_df[[jj]]),list_df[[jj]]), 
                                  paste(c("자금대여", "자금.대여"), collapse = "|")))) ){
        list_case[jj] <- "case4"}
      }
    if(length(which(list_case %in% c("case1", "case1_3"))) %% 2 == 1){
    print("error:: wrong case number in list_case_fun function!")}
  }
  
  final_table <- data.frame()
  tmp <- data.frame()
  for(i in 1:length(list_df)){
    if(list_TF[i] == TRUE){
      ### list_df의 테이블을 tmp에 저장
      tmp <- list_df[[i]]
      
        ### tmp text_clean 수행
        tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)), stringsAsFactors = F)
        colnames(tmp) <- text_clean(colnames(tmp))
        ### 특이케이스 제거 !!! - 구분, 회사명 외에 필요없는 구분자가 있을 경우 제거
        # tmp <- tmp[,!str_detect(colnames(tmp), "거래내용")]
        ### 2017, 2018 -> 전기, 당기
        tmp[1,] <- gsub("2018", "당기", gsub("2017", "전기", tmp[1,]))
        if(nrow(tmp) >= 2){ tmp[2,] <- gsub("2018", "당기", gsub("2017", "전기", tmp[2,])) }
        ### 2열에 거래내역, 3열에 회사명 있으면 2,3열 바꾸기
        if(colnames(tmp)[2:3] == c("거래내역", "회사명")){
          tmp <- tmp[c(1,3,2,4:ncol(tmp))]
        }
        ### 첫열에 계, 합계 텍스트 있는 행 제거 
        tmp <- tmp[!ifelse(tmp[,1] %in% c("계", "합계", "소계", "채권", "채무", "채무합계", "채권합계", "수익", "비용",
                                          "매출등합계", "매입등합계", "당기", "전기", "당기말", "전기말"), TRUE, FALSE),]
        
        ### colnames가 단위에 대한 내용이 아니라면 첫행으로 추가하기
        if(!any(str_detect(colnames(tmp), "단위"))){
          tmp <- rbind(colnames(tmp), tmp)}
        
        ### 구분에 대한 첫번째 케이스 - 구분 항목이 행으로 들어가 있는 경우 NA 없애기
        start_c <- list_start_fun(tmp)[2]
        tmp_col1 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,1]))))
        if(nrow(tmp) >= 2){ tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2])))) } else{
          tmp_col2 <- FALSE }
        if(nrow(tmp) >= 3){ tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3])))) } else{
          tmp_col3 <- FALSE }
        sub_name_tmp <- c("종속기업", "관계기업", "지배기업", "기타특수관계기업",
                          "기타특수관계자", "공동기업", "관계기업및공동기업","대규모기업집단계열회사","대규모기업집단계열회사등",
                          "당사에유의적영향력을행사하는기업", "유의적인영향력을행사할수있는투자자","대기업기업집단계열회사등",
                          "당해기업에중대한영향력을미치는회사", "당사에중요한영향력을행사하는회사", "당사의종속기업", 
                          "유의적영향력을행사하는회사", "기타의특수관계자","상위지배기업의공동기업", "상위지배기업및그종속기업",
                          "종속,관계기업","대규모기업집단", "신규설립으로인한현금출자및유상증자", "유의적인영향력을행사하는기업")
        
        if( ((start_c == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
            ((start_c == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
            ((start_c == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
          narow_which <- which(tmp_col1 %in% sub_name_tmp)
          for(ii in 1:length(narow_which)){
            tmp[narow_which[ii],is.na(tmp[narow_which[ii],])] <- "값없음"
          }
        }
        
        ### 정리된 테이블에 밀림현상있는지 체크하고 수정(순서바꾸지말것!!!)
        if(( any(is.na(tmp[1:(ifelse(nrow(tmp)<5, nrow(tmp),5)),ncol(tmp)])) & 
             any(str_detect(tmp[,1:(list_start_fun(tmp)[2]-1)], "[0-9]+[,]*")) ) | 
           ( any(is.na(tmp[1:(ifelse(nrow(tmp)<5, nrow(tmp),5)),ncol(tmp)])) & 
             any(is.na(tmp[1:(list_start_fun(tmp)[1]-1),])) ) == TRUE){
          # 밀린데이터 원위치
          # tmp=rbind((tmp[which(tmp[,1]=="특수관계자"),])[,2:ncol(tmp)], (tmp[which(tmp[,1]!="특수관계자"),])[,1:(ncol(tmp)-1)]) # 예외상황
          number_na <- which(!complete.cases(tmp))
          for(ind in 1:length(number_na)){
            rowsum_na <- sum(is.na(tmp[number_na[ind],]))  # 해당 행에 na 갯수 rowsum_na
            tmp[1,] <- gsub("\\s", "", gsub("[0-9]", "", gsub("\\.","",tmp[1,])))
            # text일경우, 숫자일경우 기준 'standard_na'을 다르게 정해야함 
            if(str_detect((list_start_fun(tmp[number_na[ind],])[2]),'[0-9]') %in% c(TRUE) ){ # 숫자일 경우
              standard_na <- list_start_fun(tmp[number_na[ind],])[2] 
            } else if(is.na(list_start_fun(tmp[number_na[ind],])[2]) ) { # text일 경우
              standard_na <- which((str_detect(tmp[c(1:3,number_na[ind]),], paste(c("구분", "회사명","회사의명칭","기업명",
                            "값없음","알수없음"), collapse = '|')) & 
                            !(str_detect(tmp[c(1:3,number_na[ind]),], paste(list_TF_fun(list_df, T), collapse = '|')))) == FALSE)[1]
              }
            tmp[number_na[ind],(standard_na+rowsum_na):ncol(tmp)] <- tmp[number_na[ind],standard_na:(ncol(tmp)-rowsum_na)]
            tmp[number_na[ind],standard_na:(standard_na+rowsum_na-1)] <- "값없음"
          }
        }
        
        # 첫번째 케이스 : 구분 항목이 행으로 들어가 있는 경우 행렬변환
        start_c <- list_start_fun(tmp)[2]
        tmp_col1 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,1]))))
        if(nrow(tmp) >= 2){ tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2])))) } else{
          tmp_col2 <- FALSE }
        if(nrow(tmp) >= 3){ tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3])))) } else{
          tmp_col3 <- FALSE }
        
        if( ((start_c == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
            ((start_c == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
            ((start_c == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
          insert_col <- na.locf(ifelse(str_detect(tmp_col1, paste(sub_name_tmp, collapse = '|')) == TRUE, tmp_col1, "값없음"),
                                na.rm=FALSE, fromLast = FALSE)
          insert_col[is.na(insert_col)] <- "값없음"
          tmp <- cbind(insert_col, tmp)
          tmp <- tmp[!(tmp_col1 %in% sub_name_tmp),]
          colnames(tmp)[1:2] <- c("구분", "회사명")
          tmp[,1] <- as.character(tmp[,1])
        }
        # # 두번째 케이스 : 구분이 따로 없고 앞선테이블에서 정의된 케이스
        # if((start_c == 2) & (!any(c("종속기업", "관계기업", "기타특수관계자", "공동기업") %in% tmp[,1]))){
        #   list_contents <- contents_name(list_df)
        #   colnames(tmp)[1] <- "기업명"
        #   tmp <- left_join(tmp, list_contents, by = "기업명")
        #   tmp <- tmp[c(ncol(tmp), 1:(ncol(tmp)-1))]
        #   tmp[,1] <- as.character(tmp[,1])
        #   tmp[is.na(tmp[,1]),1] <- "값없음"
        # }
        # 세번째 케이스 : 구분이 따로 없고 앞선테이블에서 정의되지 않은 케이스 - '알수없음'처리
        if((start_c == 2) & (!any(sub_name_tmp %in% tmp_col1))|
           ((start_c == 3) & any(list_TF_fun(list_df,T) %in% tmp_col2) & (!any(sub_name_tmp %in% tmp_col2))) ){
          tmp$new <- "알수없음"
          tmp <- tmp[c(ncol(tmp), 1:(ncol(tmp)-1))]
          colnames(tmp)[1:2] <- c("구분", "회사명")
        }
        
        ### 특수관계자성격 열이 2번째 있을때 순서 바꿔주기
        start_r <- list_start_fun(tmp)[1]
        if(any(sub_name_tmp %in% tmp[,2])){
          tmp[start_r:nrow(tmp),2] <- na.locf(ifelse(str_detect(tmp[start_r:nrow(tmp),2], paste(sub_name_tmp, 
                            collapse = '|')) == TRUE, tmp[start_r:nrow(tmp),2], NA), na.rm=FALSE, fromLast = FALSE)
          tmp <- tmp[,c(2,1,3:ncol(tmp))]
        }
        ### 2열에(기업명) 자리에 특정 문자 속해있을 때 해당 열 삭제하기
        if(c("대규모기업집단계열회사") %in%tmp[,2]==TRUE){
          tmp <- tmp[,-2]
        }
        
        ### 두번째열에 계, 합계, 기타 텍스트 있는 행 제거 
        tmp <- tmp[!ifelse(tmp[,2] %in% c("계", "합계", "소계", "기타"), TRUE, FALSE),]
        
        ### col_name_tmp 명명하기
        start_c <- list_start_fun(tmp)[2]
        start_r_after <- list_start_fun(tmp)[1]
        col_name <- c("구분", "기업명", colname_clean(tmp, start_r_after))
        
        ### 해당 테이블의 case 정하기
        tmp_case <- list_case[i]
        
        ### 케이스별로 table정리 
        if(tmp_case %in% c("case1", "case1_3")){
          # 사전정의 함수 - 당기/전기 구분
          list_class <- list_class_fun(list_TF, list_case, list_df)
          
          ### 해당 테이블이 case1일때 수행하는 함수
          if (tmp_case == "case1"){
            # row 이름 명명하기
            row_name1 <- text_clean(tmp[,1])
            row_name2 <- text_clean(tmp[,2])
            row_name <- paste0(row_name1, "@", row_name2); row_name <- row_name[start_r_after:length(row_name)]
            # 숫자만 있는 테이블로 만들고 ()는 음수처리
            unit <- list_unit_fun(list_df, i, get_startend(path, 2))
            if(nrow(tmp) - start_r_after == 1){
              tmp <- tmp[start_r_after:nrow(tmp), start_c:ncol(tmp)]
              tmp <- as.data.frame(lapply(tmp, function(x) ifelse(x =="-", 0, x)), stringsAsFactors = F)
              tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(gsub(",", "", gsub("\\s", "", gsub("\\)","",
                                                   gsub("\\(","-",x))))) * unit), stringsAsFactors = F)
              colnames(tmp) <- col_name[start_c:length(col_name)]
              # category1를 다시 만들어서 table 재정의_col
              category1 <- t(as.data.frame(str_split(row_name, "@")))
              tmp <- cbind(category1[,1], category1[,2], list_class[i], tmp)
              colnames(tmp)[1:3] <- c("구분", "회사명", "당기_전기")
              tmp[,1:start_c] <- as.data.frame(lapply(tmp[,1:start_c], as.character), stringsAsFactors = F)
              } else{
              tmp <- tmp[start_r_after:nrow(tmp), start_c:ncol(tmp)]
              tmp <- as.data.frame(lapply(tmp, function(x) ifelse(x =="-", 0, x)), stringsAsFactors = F)
              tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(gsub(",", "", gsub("\\s", "", gsub("\\)","",
                                                                 gsub("\\(","-",x))))) * unit), stringsAsFactors = F)
              colnames(tmp) <- col_name[start_c:length(col_name)]
              # category1를 다시 만들어서 table 재정의_col
              category1 <- t(as.data.frame(str_split(row_name, "@")))
              tmp$구분 <- category1[,1]; tmp$회사명 <- category1[,2]
              tmp$당기_전기 <- list_class[i]
              }
            tmp <- tmp[, c((ncol(tmp)-2):ncol(tmp), 1:(ncol(tmp)-3))]
            rownames(tmp) <- NULL
          } else if(tmp_case == "case1_3"){
            
            ### 해당 테이블이 case1_3일때 수행하는 함수
            if(dim(tmp)[2] == 5){
              tmp[,3] <- paste0(tmp[,3], "@", tmp[,4]); tmp <- tmp[,-4]
              col_name <- col_name[-3]
            }
            # 테이블 틀 만들기
            tmp <- tmp[start_r_after:nrow(tmp),]
            # 숫자만 있는 4열 () 음수처리
            unit <- list_unit_fun(list_df, i, get_startend(path, 2))
            tmp[,4] <- as.numeric(lapply(tmp[,4], function(x) gsub("\\)","",gsub("\\(","-",gsub(",","",gsub("-",0,x)))) )) * unit
            colnames(tmp)[1:4] <- c("구분", "회사명", "계정과목", "value")
            tmp$당기_전기 <- list_class[i]
            tmp <- dcast(tmp, 구분 + 회사명 + 당기_전기 ~ 계정과목, sum)
          }
          # category1를 다시 만들어서 table 재정의_row
          tmp <- tmp[!str_detect(rownames(tmp), paste(c("소계", "합계", "기타"), collapse = '|')),]
          rownames(tmp) <- NULL
        } else if (tmp_case == "case2"){
          
          ### 해당 테이블이 case2일때 수행하는 함수
          # 당기/전기 구분이 2행에 되어 있는 예외적인 경우 수행
          if(any(grepl(c("당기", "전기"), gsub("\\.","",gsub("[0-9]","", gsub("\\(","", gsub(")","",tmp[2,])))))) == TRUE){
            tmp <- tmp[c(2,1,3:nrow(tmp)),]}     
          if(any(grepl(c("2017", "2018"), tmp[2,])) == TRUE){tmp <- tmp[c(2,1,3:nrow(tmp)),]}
          # col, row 이름 명명하기
          col_name1 <- ifelse(grepl("당", tmp[1,]), "당기", ifelse(grepl("전", tmp[1,]), "전기",
                                                                ifelse(grepl("2018", tmp[1,]), "당기", ifelse(grepl("2017", tmp[1,]), "전기", "구분"))))
          if(start_r_after==2){col_name <- col_name1} else if(start_r_after==3){
            col_name <- paste0(col_name1, "@", tmp[2,])} else if(start_r_after == 4){
              col_name <- paste0(col_name1, "@", tmp[2,], "@", tmp[3,])
            } else {print(paste("error:: col_name error in dart_get case2!", i, "단계에서"))}
          # tmp clean 작업
          tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)), stringsAsFactors = F)
          colnames(tmp) <- col_name
          tmp <- tmp[start_r_after:nrow(tmp), ]  
          # 테이블 틀 만들기
          tmp <- melt(tmp, id=c(colnames(tmp)[1], colnames(tmp)[2]))
          # 숫자만 있는 value ()는 음수처리
          unit <- list_unit_fun(list_df, i,  get_startend(path, 2))
          tmp$value <- as.numeric(lapply(tmp$value, function(x) gsub("\\)","",gsub("\\(","-",gsub(",","",gsub("-",0,x)))) )) * unit
          category2 <- t(as.data.frame(str_split(tmp$variable, "@")))
          tmp$당기_전기 <- category2[,1]
          if(dim(category2)[2] == 2){ tmp$col_var <- category2[,2]
          } else if(dim(category2)[2] == 3){tmp$col_var <- paste0(category2[,2], "@", category2[,3])
          }
          tmp <- tmp[,-3]
          colnames(tmp)[1:2] <- c("구분", "회사명")
          tmp <- dcast(tmp, 구분 + 회사명 + 당기_전기 ~ col_var, fun.aggregate = sum)
          # category를 다시 만들어서 table 재정의_row
          tmp <- tmp[!str_detect(tmp[,2], paste(c("소계", "합계", "기타"),collapse = '|')),]
          rownames(tmp) <- NULL
        } else if (tmp_case == "case3"){
          
          ### 해당 테이블이 case3일때 수행하는 함수
          colnames(tmp) <- text_clean(ifelse(grepl("당", tmp[1,]), "당기", ifelse(grepl("전", tmp[1,]), "전기",
                                                                               ifelse(grepl("2018", tmp[1,]), "당기", ifelse(grepl("2017", tmp[1,]), "전기", "구분")))))
          if(dim(tmp)[2] == 6){
            tmp[,3] <- paste0(tmp[,3], "@", tmp[,4]); tmp <- tmp[,-4]
          }
          # 테이블 틀 만들기
          tmp <- tmp[list_start_fun(tmp)[1]:nrow(tmp),]
          colnames(tmp)[1:3] <- c("구분", "회사명", "계정과목")
          tmp <- melt(tmp, id=1:3)
          # 숫자만 있는 value ()는 음수처리
          unit <- list_unit_fun(list_df, i, get_startend(path, 2))
          tmp$value <- as.numeric(lapply(tmp$value, function(x) gsub("\\)","",gsub("\\(","-",gsub(",","",gsub("-",0,x)))) )) * unit
          tmp <- dcast(tmp, 구분 + 회사명 + variable ~ 계정과목, fun.aggregate = sum)
          colnames(tmp)[3] <- "당기_전기"
          tmp$당기_전기 <- as.character(tmp$당기_전기)
          # category를 다시 만들어서 table 재정의_row
          tmp <- tmp[!str_detect(tmp[,2], paste(c("소계", "합계", "기타"),collapse = '|')),]
          rownames(tmp) <- NULL
        } else if (tmp_case == "case4"){
          
          ### 해당 테이블이 case4일때 수행하는 함수
            # row 이름 명명하기
            row_name1 <- text_clean(tmp[,1])
            row_name2 <- text_clean(tmp[,2])
            row_name <- paste0(row_name1, "@", row_name2); row_name <- row_name[start_r_after:length(row_name)]
            # 숫자만 있는 테이블로 만들고 ()는 음수처리
            unit <- list_unit_fun(list_df, i, get_startend(path, 2))
            tmp <- tmp[start_r_after:nrow(tmp), start_c:ncol(tmp)]
            tmp <- as.data.frame(lapply(tmp, function(x) ifelse(x =="-", 0, x)), stringsAsFactors = F)
            tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(gsub(",", "", gsub("\\s", "", gsub("\\)","",
                                                   gsub("\\(","-",x))))) * unit), stringsAsFactors = F)
            colnames(tmp) <- col_name[start_c:length(col_name)]
            # category1를 다시 만들어서 table 재정의_col
            category1 <- t(as.data.frame(str_split(row_name, "@")))
            tmp <- cbind(category1[,1], category1[,2], "값없음", tmp)
            colnames(tmp)[1:3] <- c("구분", "회사명", "당기_전기")
            tmp[,1:start_c] <- as.data.frame(lapply(tmp[,1:start_c], as.character), stringsAsFactors = F)
            rownames(tmp) <- NULL
          } else {print(paste("error:: no case in dart_get case3!", i, "에서"))}
        
        
        ### table 합치기
        # if(is.na(tmp)==TRUE){print("error is.na true")}   # 최종점검용
        tmp <- as.data.frame(lapply(tmp, function(x) ifelse(is.na(x)==TRUE, 0, x)), stringsAsFactors = F)
        final_table <- merge(final_table, tmp, all = TRUE)
        if(nrow(final_table)==0){
          final_table <- merge(final_table, tmp, all = TRUE)}
        }
  }
  ### final table 수정
  final_table[,5:ncol(final_table)] <- as.data.frame(lapply(final_table[,5:ncol(final_table)], function(x) as.numeric(gsub("<NA>", NA, x))), stringsAsFactors = F)
  final_table <- aggregate(final_table[,4:ncol(final_table)], by=list(final_table$구분, final_table$회사명, final_table$당기_전기), sum, na.rm=TRUE, na.action=NULL)
  final_table <- cbind(as.character(kospi200[index,1]), final_table)
  colnames(final_table)[1:4] <- c("대상회사명", "구분", "회사명", "당기_전기")
  
  ### except table 저장
  list_except = data.frame()
  for (i in 1:length(list_TF)){
    if(str_detect(list_TF[i], "except") == TRUE){
      tmp_target <- cbind(as.character(kospi200[index,1]), list_df[[i]])
      list_except <- rbind(list_except, tmp_target)
    }
  }
  result_final <- list(final_table, list_except)
  return(result_final)
}


############################## step 3. error_check function ##################################
error_check_fun <- function(t_ind, output_TF = FALSE){
  result_path <- as.character(kospi200$링크[t_ind])
  result_table <- dart_get(result_path, t_ind)
  except_tf <- ifelse(length(result_table[[2]]) == 0, F, T)
  check_list <- list(kospi200[t_ind,], dim(result_table[[1]]), except_tf)
  if(output_TF == TRUE){
    return(result_table)
  } else {
    return(check_list)
  }
}

############################## step 3-1. error_check 완료 ##################################

error_check_fun(1)   ### '1_(주)BNK금융지주' -> 확인완료(16,22)
error_check_fun(2)   ### '2_(주)DB하이텍' -> 확인완료(16,8)
error_check_fun(3)   ### '3_(주)KB금융지주'-> 확인완료(24,19)
error_check_fun(4)   ### '4_(주)강원랜드'-> 확인완료(6,6)
error_check_fun(5)   ### '5_(주)녹십자' -> 확인완료(19,22)
error_check_fun(6)   ### '6_(주)녹십자홀딩스' -> 확인완료(37,13)
error_check_fun(7)   ### '7_(주)농심' -> 확인완료(26,12)
error_check_fun(8)   ### '8_(주)대교' -> 확인완료(60,15)
error_check_fun(9)   ### '9_(주)대우건설' -> 확인완료(89,20)
error_check_fun(10)   ### '10_(주)대웅제약' -> 확인완료(23,19)
error_check_fun(11)   ### '11_(주)대한항공' -> 확인완료(22,8)
error_check_fun(12)   ### '12_(주)동서' -> 확인완료(14,13)
error_check_fun(13)   ### '13_(주)동양'-> 확인완료(28,18)
error_check_fun(14)   ### '14_(주)동원에프앤비' -> 확인완료(18,15)
error_check_fun(15)   ### '15_(주)두산' -> 확인완료(16,14)
error_check_fun(16)   ### '16_(주)락앤락' -> 확인완료(33,17)
error_check_fun(17)   ### '17_(주)만도' -> 확인완료(47,23)
error_check_fun(18)   ### '18_(주)무학'-> 확인완료(24,13)
error_check_fun(19)   ### '19_(주)비지에프' -> 확인완료(17,10)
error_check_fun(20)   ### '20_(주)비지에프리테일' -> 확인완료(7,10)
error_check_fun(21)   ### '21_(주)빙그레' -> 확인완료(10,11)
error_check_fun(22)   ### '22_(주)삼양사' -> 확인완료(15,15)
error_check_fun(23)   ### '23_(주)삼양홀딩스' -> 확인완료(32,11)
error_check_fun(24)   ### '24_(주)세아베스틸' -> 확인완료(46,24)
error_check_fun(25)   ### '25_(주)셀트리온‘ -> 확인완료(9,18)
error_check_fun(26)   ### '26_(주)신세계‘ -> 확인완료(40,16)
error_check_fun(27)   ### '27_(주)신한금융지주회사‘ -> 확인완료(26,11)
error_check_fun(28)   ### '28_(주)아모레퍼시픽‘ -> 확인완료(64,15)
error_check_fun(29)   ### '29_(주)아모레퍼시픽그룹‘ -> 확인완료(24,14)
error_check_fun(30)   ### '30_(주)에스비에스‘ -> 확인완료(24,23)
error_check_fun(31)   ### '31_(주)에스원‘ -> 확인완료(15,14)
error_check_fun(32)   ### '32_(주)에스피씨삼립‘ -> 확인완료(84,14)
error_check_fun(33)   ### '33_(주)엔씨소프트‘ -> 확인완료(25,21)
error_check_fun(34)   ### '34_(주)엘에스‘ -> 확인완료(58,8)
error_check_fun(35)   ### '35_(주)엘에프‘ -> 확인완료(47,18)
error_check_fun(36)   ### '36_(주)엘지‘ -> 확인완료(51,16)
error_check_fun(37)   ### '37_(주)엘지상사‘ -> 확인완료(81,11)
error_check_fun(38)   ### '38_(주)엘지생활건강‘ -> 확인완료(97,12)
error_check_fun(39)   ### '39_(주)엘지유플러스‘ -> 확인완료(76,9)
error_check_fun(40)   ### '40_(주)엘지하우시스‘ -> 확인완료(45,10)
error_check_fun(41)   ### '41_(주)엘지화학‘ -> 확인완료(52,28)
error_check_fun(42)   ### '42_(주)영원무역‘ -> list_TF에 채권, 채무 필수여야해서 나오지 않음(15,7)
error_check_fun(43)   ### '43_(주)영풍‘ -> 확인완료(27,23)
error_check_fun(44)   ### '44_(주)오뚜기‘ -> 확인완료(44,13)
error_check_fun(45)   ### '45_(주)오리온‘ -> 확인완료(33,19)
error_check_fun(46)   ### '46_(주)오리온홀딩스‘ -> 확인완료(37,18)
error_check_fun(47)   ### '47_(주)우리은행‘ -> 확인완료(20,12)
error_check_fun(48)   ### '48_(주)유니드‘ -> 확인완료(19,13)
error_check_fun(49)   ### '49_(주)유한양행‘ -> 확인완료(33,19)
error_check_fun(51)   ### '51_(주)이마트‘ -> 확인완료(48,12)
error_check_fun(52)   ### '52_(주)제일기획‘ -> 확인완료(45,19)
error_check_fun(53)   ### '53_(주)종근당‘ -> 확인완료(22,35)
error_check_fun(54)   ### '54_(주)지에스‘ -> 확인완료(52,8)
error_check_fun(55)   ### '55_(주)지에스리테일‘ -> 확인완료(13,17)
error_check_fun(56)   ### '56_(주)카카오‘ -> 확인완료(122,13)
error_check_fun(57)   ### '57_(주)케이씨씨‘ -> 확인완료(40,22)
error_check_fun(58)   ### '58_(주)케이티‘ -> 확인완료(62,13)
error_check_fun(59)   ### '59_(주)케이티앤지‘ -> 확인완료(62,8)
error_check_fun(61)   ### '61_(주)포스코' -> 확인완료(43,17)
error_check_fun(126)   ### '126_(주)삼성전자'-> 확인완료(42,10)

error_check_fun(50)   ### '50_(주)이노션‘ -> 확인완료(48,11)
error_check_fun(60)   ### '60_(주)팜스코‘ -> 확인완료(68,33)
error_check_fun(62)   ### '62_(주)포스코대우‘ -> 확인완료(125,16)
error_check_fun(63)   ### '63_(주)풍산‘ -> 확인완료(36,17)
error_check_fun(64)   ### '64_(주)하나금융지주‘ -> 확인완료(26,14)
error_check_fun(65)   ### '65_(주)한라홀딩스‘ -> 확인완료(76,29)
error_check_fun(66)   ### '66_(주)한샘‘ -> 확인완료(34,10)
error_check_fun(67)   ### '67_(주)한섬‘ -> 확인완료(26,18)
error_check_fun(68)   ### '68_(주)한솔케미칼‘ -> 확인완료(28,16)
error_check_fun(69)   ### '69_(주)한진중공업‘ -> 확인완료(24,20)
error_check_fun(70)   ### '70_(주)한화‘ -> 확인완료(75,16)
error_check_fun(71)   ### '71_(주)현대그린푸드‘ -> 확인완료(43,13)
error_check_fun(72)   ### '72_(주)현대리바트‘ -> 확인완료(61,27)
error_check_fun(73)   ### '73_(주)현대미포조선‘ -> 확인완료(29,17)
error_check_fun(74)   ### '74_(주)현대백화점‘ -> 확인완료(58,16)
error_check_fun(75)   ### '75_(주)현대홈쇼핑‘ -> 확인완료(40,14)
error_check_fun(76)   ### '76_(주)호텔신라‘ -> 확인완료(42,22)
error_check_fun(77)   ### '77_(주)효성‘ -> 확인완료(74,25)
error_check_fun(78)   ### '78_(주)후성‘ -> 확인완료(27,15)
error_check_fun(79)   ### '79_DB손해보험(주)‘ -> 확인완료(108,28)
error_check_fun(80)   ### '80_LG이노텍(주)‘ -> 확인완료(111,46)
error_check_fun(81)   ### '81_LS산전(주)‘ -> 확인완료(40,12)
error_check_fun(82)   ### '82_OCI(주)‘ -> 확인완료(59,28)
error_check_fun(83)   ### '83_S&T모티브(주)‘ -> 확인완료(26,13)
error_check_fun(84)   ### '84_SK이노베이션(주)‘ -> 확인완료(54,10)
error_check_fun(85)   ### '85_고려아연(주)‘ -> 확인완료(34,14)
error_check_fun(86)   ### '86_고려제강(주)‘ -> 확인완료(73,15)
error_check_fun(88)   ### '88_금호석유화학(주)‘ -> 확인완료(26,13)
error_check_fun(89)   ### '89_금호타이어(주)‘ -> 확인완료(34,13)
error_check_fun(90)   ### '90_기아자동차(주)‘ -> 확인완료(42,16)
error_check_fun(91)   ### '91_남양유업(주)‘ -> 확인완료(7,10)
error_check_fun(92)   ### '92_남해화학(주)‘ -> 확인완료(44,10)
error_check_fun(93)   ### '93_네이버(주)‘ -> 확인완료(109,16)
error_check_fun(94)   ### '94_넥센타이어(주)‘ -> 확인완료(39,16)
error_check_fun(95)   ### '95_넷마블(주)‘ -> 확인완료(94,23)
error_check_fun(96)   ### '96_대덕전자(주)' -> 확인완료(10,27)
error_check_fun(97)   ### '97_대림산업(주)‘ -> 확인완료(84,12)
error_check_fun(98)   ### '98_대상(주)‘ -> 확인완료(70,18)
error_check_fun(99)   ### '99_대우조선해양(주)‘ -> 확인완료(28,22)
error_check_fun(100)   ### '100_대한유화(주)‘ -> 확인완료(10,10)

error_check_fun(101)   ### '101_동국제강(주)‘ -> 확인완료(42,24)
error_check_fun(102)   ### '102_동아쏘시오홀딩스(주)‘ -> 확인완료(27,18)
error_check_fun(103)   ### '103_동아에스티(주)‘ -> 확인완료(29,21)
error_check_fun(104)   ### '104_동원시스템즈(주)‘ -> 확인완료(44,13)
error_check_fun(105)   ### '105_두산밥캣(주)‘ -> 확인완료(17,12)
error_check_fun(106)   ### '106_두산인프라코어(주)‘ -> 확인완료(20,24)
error_check_fun(107)   ### '107_두산중공업(주)‘ -> 확인완료(88,25)
error_check_fun(108)   ### '108_롯데쇼핑(주)‘ -> 확인완료(87,15)
error_check_fun(109)   ### '109_롯데정밀화학(주)‘ -> 확인완료(23,12)
error_check_fun(110)   ### '110_롯데지주(주)‘ -> 확인완료(46,11)
error_check_fun(111)   ### '111_롯데칠성음료(주)‘ -> 확인완료(53,20)
error_check_fun(112)   ### '112_롯데케미칼(주)‘ -> 확인완료(63,12)
error_check_fun(113)   ### '113_롯데푸드(주)‘ -> 확인완료(24,12)
error_check_fun(114)   ### '114_롯데하이마트(주)‘ -> 확인완료(34,13)
error_check_fun(115)   ### '115_메리츠종합금융증권(주)‘ -> 확인완료(456,8)
error_check_fun(116)   ### '116_미래에셋대우(주)‘ -> 확인완료(26,15)
error_check_fun(117)   ### '117_보령제약(주)‘ -> 확인완료(26,16)
error_check_fun(118)   ### '118_부광약품(주)‘ -> 확인완료(10,22)
error_check_fun(119)   ### '119_삼성SDI(주)‘ -> 확인완료(49,14)
error_check_fun(120)   ### '120_삼성물산(주)‘ -> 확인완료(34,12)
error_check_fun(121)   ### '121_삼성바이오로직스(주)‘ -> 확인완료(35,15)
error_check_fun(122)   ### '122_삼성생명보험(주)‘ -> 확인완료(164,21)
error_check_fun(123)   ### '123_삼성에스디에스(주)‘ -> 확인완료(74,11)
error_check_fun(124)   ### '124_삼성엔지니어링(주)‘ -> 확인완료(78,22)
error_check_fun(125)   ### '125_삼성전기(주)‘ -> 확인완료(38,21)
error_check_fun(126)   ### '126_삼성전자(주)‘ -> 확인완료(45,10)
error_check_fun(127)   ### '127_삼성중공업(주)‘ -> 확인완료(17,8)
error_check_fun(128)   ### '128_삼성증권(주)‘ -> 확인완료(54,17)
error_check_fun(129)   ### '129_삼성카드(주)‘ -> 확인완료(36,15)
error_check_fun(130)   ### '130_삼성화재해상보험(주)‘ -> 확인완료(50,20)
error_check_fun(131)   ### '131_세방전지(주)‘ -> 확인완료(25,15)
error_check_fun(132)   ### '132_쌍용양회공업(주)‘ -> 확인완료(20,13)
error_check_fun(133)   ### '133_쌍용자동차(주)‘ -> 확인완료(16,11)
error_check_fun(134)   ### '134_씨제이(주)‘ -> 확인완료(24,10)
error_check_fun(135)   ### '135_씨제이대한통운(주)‘ -> 확인완료(282,15)
error_check_fun(136)   ### '136_씨제이씨지브이(주)‘ -> 확인완료(127,20)
error_check_fun(137)   ### '137_씨제이제일제당(주)‘ -> 확인완료(134,12)
error_check_fun(138)   ### '138_아이에스동서(주)‘ -> 확인완료(90,16)
error_check_fun(139)   ### '139_에스엘(주)‘ -> 확인완료(38,18)
error_check_fun(140)   ### '140_에스케이(주)‘ -> 확인완료(62,14)


############################## step 3-2. table 생성 ##################################

### 테이블생성
BNK_table <- error_check_fun(1, TRUE)   # '1_(주)BNK금융지주'
dbhitech_table <- error_check_fun(2, TRUE)   # '2_(주)DB하이텍'
kbfinan_table <- error_check_fun(3, TRUE)   # '3_(주)KB금융지주’
gangwon_table <- error_check_fun(4, TRUE)   # '4_(주)강원랜드’
green_table <- error_check_fun(5, TRUE)   # '5_(주)녹십자'
greenhol_table <- error_check_fun(6, TRUE)   # '6_(주)녹십자홀딩스'
nongsim_table <- error_check_fun(7, TRUE)   # '7_(주)농심’
daekyo_table <- error_check_fun(8, TRUE)   # '8_(주)대교'
daewoo_table <- error_check_fun(9, TRUE)   # '9_(주)대우건설’
daewoong_table <- error_check_fun(10, TRUE)   # '10_(주)대웅제약’
daehan_table <- error_check_fun(11, TRUE)   # '11_(주)대한항공’
dongseo_table <- error_check_fun(12, TRUE)   # '12_(주)동서'
dongyang_table <- error_check_fun(13, TRUE)   # '13_(주)동양'
dongwon_table <- error_check_fun(14, TRUE)   # '14_(주)동원에프앤비'
doosan_table <- error_check_fun(15, TRUE)   # '15_(주)두산’
rockn_table <- error_check_fun(16, TRUE)   # '16_(주)락앤락'
mando_table <- error_check_fun(17, TRUE)   # '17_(주)만도'
moohak_table <- error_check_fun(18, TRUE)   # '18_(주)무학'
bgf_table <- error_check_fun(19, TRUE)   # '19_(주)비지에프'
bgfretail_table <- error_check_fun(20, TRUE)   # '20_(주)비지에프리테일'
bing_table <- error_check_fun(21, TRUE)   # '21_(주)빙그레'
samyang_table <- error_check_fun(22, TRUE)   # '22_(주)삼양사'
samyanghol_table <- error_check_fun(23, TRUE)   # '23_(주)삼양홀딩스'
sae_table <- error_check_fun(24, TRUE)   # '24_(주)세아베스틸'
celltrion_table <- error_check_fun(25, TRUE)   # '25_(주)셀트리온‘
sinsegye_table <- error_check_fun(26, TRUE)   # '26_(주)신세계‘
sinhanfinan_table <- error_check_fun(27, TRUE)   # '27_(주)신한금융지주회사‘
amgroup_table <- error_check_fun(28, TRUE)   # '28_(주)아모레퍼시픽‘
amore_table <- error_check_fun(29, TRUE)   # '29_(주)아모레퍼시픽그룹‘
sbs_table <- error_check_fun(30, TRUE)   # '30_(주)에스비에스‘
sone_table <- error_check_fun(31, TRUE)   # '31_(주)에스원‘
spc_table <- error_check_fun(32, TRUE)   # '32_(주)에스피씨삼립‘
ncsoft_table <- error_check_fun(33, TRUE)   # '33_(주)엔씨소프트‘
ls_table <- error_check_fun(34, TRUE)   # '34_(주)엘에스‘
lf_table <- error_check_fun(35, TRUE)   # '35_(주)엘에프‘
lg_table <- error_check_fun(36, TRUE)   # '36_(주)엘지‘
lgicorp_table <- error_check_fun(37, TRUE)   # '37_(주)엘지상사‘
lglife_table <- error_check_fun(38, TRUE)   # '38_(주)엘지생활건강‘
lguplus_table <- error_check_fun(39, TRUE)   # '39_(주)엘지유플러스‘
lghousis_table <- error_check_fun(40, TRUE)   # '40_(주)엘지하우시스‘
lgche_table <- error_check_fun(41, TRUE)   # '41_(주)엘지화학‘
youngwon_table <- error_check_fun(42, TRUE)   # '42_(주)영원무역‘
yp_table <- error_check_fun(43, TRUE)   # '43_(주)영풍‘
ottogi_table <- error_check_fun(44, TRUE)   # '44_(주)오뚜기‘
orion_table <- error_check_fun(45, TRUE)   # '45_(주)오리온‘
orionhol_table <- error_check_fun(46, TRUE)    #'46_(주)오리온홀딩스'
woori_table <- error_check_fun(47, TRUE)   # '47_(주)우리은행'
unid_table <- error_check_fun(48, TRUE)   # '48_(주)유니드‘
yoohan_table <- error_check_fun(49, TRUE)   # '49_(주)유한양행‘
emart_table <- error_check_fun(51, TRUE)   # '51_(주)이마트‘

cheil_table <- error_check_fun(52, TRUE)   # '52_(주)제일기획‘
ckd_table <- error_check_fun(53, TRUE)   # '53_(주)종근당‘
gs_table <- error_check_fun(54, TRUE)   # '54_(주)지에스‘
gsretail_table <- error_check_fun(55, TRUE)   # '55_(주)지에스리테일‘
kakao_table <- error_check_fun(56, TRUE)   # '56_(주)카카오‘
kcc_table <- error_check_fun(57, TRUE)   # '57_(주)케이씨씨‘
kt_table <- error_check_fun(58, TRUE)   # '58_(주)케이티‘
ktng_table <- error_check_fun(59, TRUE)   # '59_(주)케이티앤지‘
posco_table <- error_check_fun(61, TRUE)   # '61_(주)포스코'
samsung_table <- error_check_fun(126, TRUE)   # '126_(주)삼성전자'

innotion_table <- error_check_fun(50, TRUE)   # '50_(주)이노션‘
farmsco_table <- error_check_fun(60, TRUE)   # '60_(주)팜스코‘
poscodaewoo_table <- error_check_fun(62, TRUE)   # '62_(주)포스코대우‘
poongsan_table <- error_check_fun(63, TRUE)   # '63_(주)풍산‘ -
hanafinan_table <- error_check_fun(64, TRUE)   # '64_(주)하나금융지주‘
halla_table <- error_check_fun(65, TRUE)   # '65_(주)한라홀딩스‘
hansaem_table <- error_check_fun(66, TRUE)   # '66_(주)한샘‘
hansum_table <- error_check_fun(67, TRUE)   # '67_(주)한섬‘
hansol_table <- error_check_fun(68, TRUE)   # '68_(주)한솔케미칼‘ 
hanjin_table <- error_check_fun(69, TRUE)   # '69_(주)한진중공업‘ 
hanhwa_table <- error_check_fun(70, TRUE)   # '70_(주)한화‘ 
hgf_table <- error_check_fun(71, TRUE)   # '71_(주)현대그린푸드
hlv_table <- error_check_fun(72, TRUE)   # '72_(주)현대리바트‘
hmd_table <- error_check_fun(73, TRUE)   # '73_(주)현대미포조선‘ 
hyundaedepart_table <- error_check_fun(74, TRUE)   # '74_(주)현대백화점‘  
hyundaehome_table <- error_check_fun(75, TRUE)   # '75_(주)현대홈쇼핑‘ 
silla_table <- error_check_fun(76, TRUE)   # '76_(주)호텔신라‘
hyosung_table <- error_check_fun(77, TRUE)   # '77_(주)효성‘
hoosung_table <- error_check_fun(78, TRUE)   # '78_(주)후성‘
dbinsu_table <- error_check_fun(79, TRUE)   # '79_DB손해보험(주)‘
lginnotech_table <- error_check_fun(80, TRUE)   # '80_LG이노텍(주)‘
lssanjeon_table <- error_check_fun(81, TRUE)   # '81_LS산전(주)‘
oci_table <- error_check_fun(82, TRUE)   # '82_OCI(주)‘
stmotive_table <- error_check_fun(83, TRUE)   # '83_S&T모티브(주)‘
skino_table <- error_check_fun(84, TRUE)   # '84_SK이노베이션(주)‘ 
koreazinc_table <- error_check_fun(85, TRUE)   # '85_고려아연(주)‘
kiswire_table <- error_check_fun(86, TRUE)   # '86_고려제강(주)‘
kumhoche_table <- error_check_fun(88, TRUE)   # '88_금호석유화학(주)‘ 
kumho_table <- error_check_fun(89, TRUE)   # '89_금호타이어(주)‘ 
kia_table <- error_check_fun(90, TRUE)   # '90_기아자동차(주)‘
namyang_table <- error_check_fun(91, TRUE)   # '91_남양유업(주)‘
namhae_table <- error_check_fun(92, TRUE)   # '92_남해화학(주)‘
naver_table <- error_check_fun(93, TRUE)   # '93_네이버(주)‘
nexentire_table <- error_check_fun(94, TRUE)   # '94_넥센타이어(주)‘
netmable_table <- error_check_fun(95, TRUE)   # '95_넷마블(주)‘
daedug_table <- error_check_fun(96, TRUE)   # '96_대덕전자(주)'
daerim_table <- error_check_fun(97, TRUE)   # '97_대림산업(주)‘ 
daesang_table <- error_check_fun(98, TRUE)   # '98_대상(주)‘ 
daewoojoseon_table <- error_check_fun(99, TRUE)   # '99_대우조선해양(주)‘
daehanu_table <- error_check_fun(100, TRUE)   ### '100_대한유화(주)‘

dongkuk_table <- error_check_fun(101, TRUE)   # '101_동국제강(주)‘
donga_table <- error_check_fun(102, TRUE)   # '102_동아쏘시오홀딩스(주)‘
dongast_table <- error_check_fun(103, TRUE)   # '103_동아에스티(주)‘
dongwon_table <- error_check_fun(104, TRUE)   # '104_동원시스템즈(주)‘
doosan_table <- error_check_fun(105, TRUE)   # '105_두산밥캣(주)‘
doosaninfra_table <- error_check_fun(106, TRUE)   # '106_두산인프라코어(주)‘
doosanheavy_table <- error_check_fun(107, TRUE)   # '107_두산중공업(주)‘
lotteshop_table <- error_check_fun(108, TRUE)   # '108_롯데쇼핑(주)‘
lottefineche_table <- error_check_fun(109, TRUE)   # '109_롯데정밀화학(주)‘
lotte_table <- error_check_fun(110, TRUE)   # '110_롯데지주(주)‘
lottedrink_table <- error_check_fun(111, TRUE)   # '111_롯데칠성음료(주)‘
lotteche_table <- error_check_fun(112, TRUE)   # '112_롯데케미칼(주)‘
lottefood_table <- error_check_fun(113, TRUE)   # '113_롯데푸드(주)‘
lottehi_table <- error_check_fun(114, TRUE)   # '114_롯데하이마트(주)‘
merits_table <- error_check_fun(115, TRUE)   # '115_메리츠종합금융증권(주)‘
miraeasset_table <- error_check_fun(116, TRUE)   # '116_미래에셋대우(주)‘
boryung_table <- error_check_fun(117, TRUE)   # '117_보령제약(주)‘
bugwang_table <- error_check_fun(118, TRUE)   # '118_부광약품(주)‘
samsungsdi_table <- error_check_fun(119, TRUE)   # '119_삼성SDI(주)‘
samsungcnt_table <- error_check_fun(120, TRUE)   # '120_삼성물산(주)‘
samsungbio_table <- error_check_fun(121, TRUE)   # '121_삼성바이오로직스(주)‘
samsunginsur_table <- error_check_fun(122, TRUE)   # '122_삼성생명보험(주)‘
samsungsds_table <- error_check_fun(123, TRUE)   # '123_삼성에스디에스(주)‘
samsungengin_table <- error_check_fun(124, TRUE)   # '124_삼성엔지니어링(주)‘
samsungelec_table <- error_check_fun(125, TRUE)   # '125_삼성전기(주)‘
samsung_table <- error_check_fun(126, TRUE)   # '126_삼성전자(주)‘
samsungheavy_table <- error_check_fun(127, TRUE)   # '127_삼성중공업(주)‘
sasmsungsec_table <- error_check_fun(128, TRUE)   # '128_삼성증권(주)‘
samsungcard_table <- error_check_fun(129, TRUE)   # '129_삼성카드(주)‘
samsunginsu_table <- error_check_fun(130, TRUE)   # '130_삼성화재해상보험(주)‘
sebang_table <- error_check_fun(131, TRUE)   # '131_세방전지(주)‘
ssangy_table <- error_check_fun(132, TRUE)   # '132_쌍용양회공업(주)‘
ssangycar_table <- error_check_fun(133, TRUE)   # '133_쌍용자동차(주)‘
cj_table <- error_check_fun(134, TRUE)   # '134_씨제이(주)‘
cjdaehan_table <- error_check_fun(135, TRUE)   # '135_씨제이대한통운(주)‘
cjcgv_table <- error_check_fun(136, TRUE)   # '136_씨제이씨지브이(주)‘
cjcheil_table <- error_check_fun(137, TRUE)   # '137_씨제이제일제당(주)‘
isdongsoe_table <- error_check_fun(138, TRUE)   # '138_아이에스동서(주)‘
sl_table <- error_check_fun(139, TRUE)   # '139_에스엘(주)‘
sk_table <- error_check_fun(140, TRUE)   # '140_에스케이(주)‘


############################## step 3-3. new company ##################################
### 오류확인
error_check_fun(141)   ### '141_에스케이네트웍스(주)‘ -> 확인완료(69,15)
error_check_fun(142)   ### '142_에스케이디스커버리(주)‘ -> 확인완료(38,14)
error_check_fun(143)   ### '143_에스케이씨(주)‘ -> 확인완료(74,11)
error_check_fun(144)   ### '144_에스케이케미칼(주)‘ -> 확인완료(51,12)
error_check_fun(145)   ### '145_에스케이텔레콤(주)‘ -> 확인완료(53,11)
error_check_fun(146)   ### '146_에스케이하이닉스(주)‘ -> 확인완료(36,12)
error_check_fun(147)   ### '147_에쓰-오일(주)‘ -> 확인완료(18,15)
error_check_fun(148)   ### '148_에이치디씨(주)‘ -> 확인완료(34,16)
error_check_fun(149)   ### '149_에이치디씨현대산업개발(주)‘ -> 확인완료(28,12)
error_check_fun(150)   ### '150_에이케이홀딩스(주)‘ -> 확인완료(56,9)
error_check_fun(151)   ### '151_엔에이치투자증권(주)‘ -> 확인완료(214,22)
error_check_fun(152)   ### '152_엘아이지넥스원(주)‘ -> 확인완료(18,13)
error_check_fun(153)   ### '153_엘지디스플레이(주)‘ -> 확인완료(114,25)
error_check_fun(154)   ### '154_엘지전자(주)‘ -> 확인완료(128,46)
error_check_fun(155)   ### '155_영진약품(주)‘ -> 확인완료(15,10)
error_check_fun(156)   ### '156_일양약품(주)‘ -> 확인완료(6,7)
error_check_fun(157)   ### '157_일진머티리얼즈(주)‘ -> 확인완료(22,12)
error_check_fun(158)   ### '158_제이더블유중외제약(주)‘ -> 확인완료(34,22)
error_check_fun(159)   ### '159_제이더블유홀딩스(주)‘ -> 확인완료(65,12)
error_check_fun(160)   ### '160_중소기업은행‘ -> 확인완료(90,19)
error_check_fun(161)   ### '161_지에스건설(주)‘ -> 확인완료(151,18)
error_check_fun(162)   ### '162_코스맥스(주)‘ -> 확인완료(34,12)
error_check_fun(163)   ### '163_코오롱인더스트리(주)‘ -> 확인완료(61,13)
error_check_fun(164)   ### '164_코웨이(주)‘ -> 확인완료(12,15)
error_check_fun(165)   ### '165_쿠쿠홀딩스(주)‘ -> 확인완료(14,16)
error_check_fun(166)   ### '166_태광산업(주)‘ -> 확인완료(74,17)
error_check_fun(167)   ### '167_팬오션(주)‘ -> 확인완료(37,16)
error_check_fun(168)   ### '168_하이트진로(주)‘ -> 확인완료(4,7)
# error_check_fun(169)   ### '169_한국가스공사(주)‘ -> 확인완료(,)
error_check_fun(170)   ### '170_한국단자공업(주)‘ -> 확인완료(28,31)
error_check_fun(171)   ### '171_한국쉘석유(주)‘ -> 확인완료(34,15)


sknet_table <- error_check_fun(141, TRUE)   # '141_에스케이네트웍스(주)‘
skdiscovery_table <- error_check_fun(142, TRUE)   # '142_에스케이디스커버리(주)‘
skc_table <- error_check_fun(143, TRUE)   # '143_에스케이씨(주)‘
skche_table <- error_check_fun(144, TRUE)   # '144_에스케이케미칼(주)‘
sktel_table <- error_check_fun(145, TRUE)   # '145_에스케이텔레콤(주)‘
skhin_table <- error_check_fun(146, TRUE)   # '146_에스케이하이닉스(주)‘
soil_table <- error_check_fun(147, TRUE)   # '147_에쓰-오일(주)‘
hdc_table <- error_check_fun(148, TRUE)   # '148_에이치디씨(주)‘
hdchyun_table <- error_check_fun(149, TRUE)   # '149_에이치디씨현대산업개발(주)‘
akhol_table <- error_check_fun(150, TRUE)   # '150_에이케이홀딩스(주)‘
nhstock_table <- error_check_fun(151, TRUE)   # '151_엔에이치투자증권(주)‘
lignexone_table <- error_check_fun(152, TRUE)   # '152_엘아이지넥스원(주)‘
lgdisplay_table <- error_check_fun(153, TRUE)   # '153_엘지디스플레이(주)‘
lg_table <- error_check_fun(154, TRUE)   # '154_엘지전자(주)‘
youngjin_table <- error_check_fun(155, TRUE)   # '155_영진약품(주)‘
ilyang_table <- error_check_fun(156, TRUE)   # '156_일양약품(주)‘
iljin_table <- error_check_fun(157, TRUE)   # '157_일진머티리얼즈(주)‘
jwche_table <- error_check_fun(158, TRUE)   # '158_제이더블유중외제약(주)‘
jwhol_table <- error_check_fun(159, TRUE)   # '159_제이더블유홀딩스(주)‘
kbbank_table <- error_check_fun(160, TRUE)   # '160_중소기업은행‘
gscons_table <- error_check_fun(161, TRUE)   # '161_지에스건설(주)‘
cosmax_table <- error_check_fun(162, TRUE)   # '162_코스맥스(주)‘
kolon_table <- error_check_fun(163, TRUE)   # '163_코오롱인더스트리(주)‘
koway_table <- error_check_fun(164, TRUE)   # '164_코웨이(주)‘
kuku_table <- error_check_fun(165, TRUE)   # '165_쿠쿠홀딩스(주)‘
taegwang_table <- error_check_fun(166, TRUE)   # '166_태광산업(주)‘
panocean_table <- error_check_fun(167, TRUE)   # '167_팬오션(주)‘
hite_table <- error_check_fun(168, TRUE)   # '168_하이트진로(주)‘
koreaterminal_table <- error_check_fun(170, TRUE)   # '170_한국단자공업(주)‘
koreashell_table <- error_check_fun(171, TRUE)   # '171_한국쉘석유(주)‘

############################## 해결하기 어려운 케이스 ##################################

### '87_그랜드코리아레저(주)‘ -> 전기에 대한 데이터 없음!!!! 큰문제
# error_check_fun(87)
# grandkorea_table <- error_check_fun(87, TRUE)


############################## step 4. 불러들인 자료 엑셀로 저장하기 ##################################
n_list <- list(ls(pattern = "table"))
n <- length(n_list[[1]])
# 결과저장
write.xlsx(get(n_list[[1]][1])[1], file = "C:/Users/Uos/OneDrive - 서울시립대학교/key/홍익대/크롤링 관련/13. example_1018(30).xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:n)
for (j in 2:n){
  write.xlsx(get(n_list[[1]][j])[1], file = "C:/Users/Uos/OneDrive - 서울시립대학교/key/홍익대/크롤링 관련/13. example_1018(30).xlsx", sheetName = name_tmp[j-1], append=TRUE)
}
# 결과 except 저장
except_list_tf <- rep(FALSE, n)
for(i in 1:n){
  if(length(get(n_list[[1]][i])[[2]]) != 0){
    except_list_tf[i] <- TRUE}
}
list_number <- which(except_list_tf)
write.xlsx(get(n_list[[1]][list_number[1]])[[2]], file = "13. example_1018(30)_except.xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:length(list_number))
for (j in 2:length(list_number)){
  write.xlsx(get(n_list[[1]][list_number[j]])[[2]], file = "13. example_1018(30)_except.xlsx", sheetName = name_tmp[j-1], append=TRUE)
}

############################## step 5. column name 정제 & 리스트 저장##################################
n_list <- list(ls(pattern = "table"))
col_namelist <- c()
for(i in 1:length(n_list[[1]])){
  col_name_table <- colnames(get(n_list[[1]][i]))
  col_namelist <- c(col_namelist, col_name_table)
}
#　최초 칼럼
col_namelist

#　1차정제 칼럼
delete_list1 <- c("구분", "회사명", "당기전기", "값없음", "대상회사명")
for(ii in 1:length(delete_list1)){
  col_namelist <- gsub("\\.","",gsub("[0-9]","", gsub("[A-z]","",col_namelist)))
  col_namelist <- col_namelist[-which(col_namelist == delete_list1[ii])]
}
delete_list2<- c("합계", "소계")
col_namelist <- col_namelist[!str_detect(col_namelist, paste(delete_list2, collapse ="|"))]
col_namelist_update1 <- unique(col_namelist)
col_namelist_update1 <- matrix(col_namelist_update1, ncol=6)
write.csv(col_namelist_update1, file="12. col_namelist.csv")




