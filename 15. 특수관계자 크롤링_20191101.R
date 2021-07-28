######################################## 0. 환경설정 #######################################
if(!require(rvest)){install.packages('rvest')}; library(rvest)
if(!require(stringr)){install.packages('stringr')}; library(stringr)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(plyr)){install.packages('plyr')}; library(plyr)
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
  text <- subset(text, (nchar(text) <= 24) & (as.numeric(str_extract(text, "[0-9]*")) > 5) )
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
  stand_n <- ifelse(nrow(tmp_table)>= 6, 6, nrow(tmp_table))
  tmp_table <- tmp_table[1:stand_n,]
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
# k=3
# str_detect(c(colnames(list_df[[k]]), as.character(list_df[[k]])), paste0(tmp_list_f, collapse = "|"))
# str_extract_all(c(colnames(list_df[[k]]), as.character(list_df[[k]]))[1], paste0(tmp_list_f, collapse = "|"))

list_TF_fun <- function(list_data, case_TF=F){
  # 제외 단어(이중적단어): 현금출자, 자금회수, 대여금, 예금가입, 상각후원가, 계약부채, ..U.00A0., 
  # 꼭 포함되어야 할 단어: 매출 등, 제상품매출, (미지급금, 채권, 채무), 외화환산
  tmp_list_t <- c("지급수수료", "비용", "예치금","기타채권", "매출채권", "제상품매출", "상품매출", "수익",
                  "미사용약정", "기타자산", "손실충당금", "대출채권", "매입채무", "매출등", "매입등", "유가증권취득",
                  "채권등", "채무등", "재고매입", "광고비", "일반관리비", "기타영업손익", "기타자산", "유형자산", 
                  "재화의매입", "재화의판매", "사업수익", "지분매각", "지분매입", "매출거래", "매입거래", "무형자산",
                  "유무형자산취득", "배당금수령", "고정자산매입", "기타일반관리비", "기타수익", "미지급금", "채권", "채무",
                  "자금대여", "장부금액", "현금출자", "자금거래", "계약자산", "금융리스", "주식배당", "매출", "매입",
                  "전력거래등", "대여", "배당금", "배당지급")
  
  tmp_list_f <- c("급여", "주석", "매도가능금융자산의평가", "채권자", "대출채권유동화", "담보권자", "여신금액",
                  "외화단위", "보증기간", "조정사항", "무상할당", "이행보증", "채권최고액", "출자대상회사",
                  "USD", "담보설정액", "공동기업주식매각", "대손상각비", "관계기업현금출자", "지분증권",
                  "잔여재산분배", "수익인식시점", "보고된금액", "자동연장", "임직원대여금",
                  "매입확약", "자문수수료비용", "투자일임계약잔고금액", "변경되었습니다", "계약기간", "채무잔액이있는",
                  "어음배서보증", "사채매입", "책임준공보증", "환율변동효과", "현금및현물출자", "총보증한도", "담보제공자산",
                  "금융리스부채", "보안시스템", "지분추가매입", "였습니다", "피보증인", "환입", "외화환산", "산업플랜트건설업",
                  "차입금상환재원", "기준서제", "합병으로인한변동", "종업원", "지분율", "주식배당")
  
  list_TF <- c()
  for(i in 1:length(list_data)){
    if(length(list_data[[i]]) <= 2){list_TF[i] = FALSE} else {list_TF[i] = TRUE}
    #회사명이 없는 경우 FALSE 분류
    if( nrow(list_data[[i]]) >= 2){
      if(any(str_detect(list_data[[i]][,1], paste0(tmp_list_t, collapse = "|")), na.rm=T) & 
         !(str_extract(list_data[[i]][nrow(list_data[[i]]),2], "[0-9]*") %in% c("", NA)) ){
        list_TF[i] = FALSE
      }
    }
    if(!any(str_detect(colnames(list_data[[i]]), "단위"))){list_data[[i]] <- rbind(colnames(list_data[[i]]), list_data[[i]])}
    list_data[[i]] <- as.data.frame(lapply(list_data[[i]], function(x) text_clean(gsub("UA","", gsub("\\s", "", 
                                                                                                     gsub("[0-9]","",gsub("\\.","",x)))))), stringsAsFactors = F)
    if( (nrow(list_data[[i]]) >= 3) & (i==1) ){
      if((i==1) & any(str_detect(list_data[[i]][3:nrow(list_data[[i]]),ncol(list_data[[i]])], "회사"), na.rm=T)){list_TF[i] = FALSE}
    }
    #colname1번째 단어 길이가 너무 길면 오류로 분류
    if( (nchar(gsub("U.00A0", "", colnames(list_data[[i]])[1])) >= 25) %in% c(TRUE)) {
      list_TF[i] = FALSE
    }
  }

  list_TF <- list_TF & str_detect(list_data, paste(tmp_list_t, collapse = "|")) & 
    (!str_detect(list_data, paste(tmp_list_f, collapse = "|")))
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
  
  list_class <- ifelse(list_case_vec %in% c("case1", "case1_3", "case1_5"), list_class, NA)
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
list_case_fun <- function(tmp_table, col_name, start_r, start_c, tmp_list_t){
  tmp_list_t_new <- tmp_list_t[!(tmp_list_t %in% c("채권", "대출채권"))]
  tmp_list_t_new <- c(tmp_list_t_new, "현금출자", "임대료")
  
  if(!(all(str_detect(paste(col_name, collapse = ""), c("당기", "전기")))
       | all(str_detect(paste(col_name, collapse = " "), c("\\(당\\)기", "\\(전\\)기")))
       | all(str_detect(paste(col_name, collapse = ""), c("2018", "2017"))) 
       | all(str_detect(paste(col_name, collapse = ""), c("당.기", "전.기"))) )){tmp_case <- "case1"
  } else if(any(str_detect(col_name, paste(tmp_list_t_new, collapse = '|'))) == TRUE){tmp_case <- "case2"
  } else if(any(str_detect(tmp_table[,(start_c-2):(start_c-1)], paste(tmp_list_t_new, collapse = '|'))) == TRUE) {tmp_case <- "case3"
  } else {print(paste("error:: no case in list_case_fun function in ", i))}
  
  # 한번더 정제작업 필요
  tmp_list_t_new <- tmp_list_t_new[!(tmp_list_t_new %in% c("수익"))]
  # case1 내에 case3인 경우 case1_3, case1_5으로 재검사
  if(tmp_case == "case1"){
    if( (any(str_detect(tmp_table[start_r:nrow(tmp_table),start_c-1], paste(tmp_list_t_new, collapse = '|')), na.rm = T) == TRUE) &
       (any(str_detect(tmp_table[1:(start_r-1),start_c:ncol(tmp_table)], paste(tmp_list_t_new, collapse = '|')), na.rm = T) == TRUE) ){
      tmp_case <- "case1_5"} else if(any(str_detect(tmp_table[start_r:nrow(tmp_table),3], paste(tmp_list_t_new, collapse = '|')), na.rm=T) == TRUE){
        tmp_case <- "case1_3"}
  }
  # case2 결과 중 case5인 경우 재검사
  if(tmp_case == "case2"){
    if(any(str_detect(tmp_table[start_r:nrow(tmp_table),3], paste(tmp_list_t_new, collapse = '|')), na.rm=T) == TRUE){
      tmp_case <- "case5"
    }
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
  ### 첫행 첫열 값이 당기/전기 라고 표기되어 있으면 값없음 처리
  tmp_colname_table[1,1] <- gsub("당기", "값없음", gsub("전기", "값없음", tmp_colname_table[1,1]))
  colnames(tmp_colname_table)[1] <- gsub("당기", "값없음", gsub("전기", "값없음", colnames(tmp_colname_table)[1]))
  ### 예외처리
  tmp_colname_table[1,] <- gsub("당기말금액", "기말금액", tmp_colname_table[1,])
  colnames(tmp_colname_table) <- gsub("당기말금액", "기말금액", colnames(tmp_colname_table))
  ### tmp_colname_table 당기/전기 한 종류만 있으면 해당하는 행 삭제
  if(sum(c("당기", "전기") %in% tmp_colname_table[1,]) == 1){
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
                                                  NA, "NA", "상대회사명", "특수관계자명칭"))]
  }
  tmp_colname_table <- as.data.frame(tmp_colname_list, stringsAsFactors = F)   # 여기서 오류나면 list 길이가 맞지 않는 것!
  # 최종적으로 tmp_colname 생성
  tmp_colname <- c()
  for(jj in 1:length(tmp_colname_table)){
    tmp_colname <- paste0(tmp_colname, tmp_colname_table[,jj], "@")
  }
  return(tmp_colname)
}

### (사전정의7) 외화 통화 코드 있는 테이블은 list_TF를 except로 변경
except_fun <- function(alpha_code, list_TF_vec, list_data){
  for(j in 1:length(list_TF_vec)){
      tmp_table <- list_data[[j]]
      start_c <- list_start_fun(tmp_table)[2]
      start_r <- list_start_fun(tmp_table)[1]

      if(length(tmp_table) != 0){
        if(start_c %in% NA){
          if( (sum(str_detect(as.character(tmp_table[1:nrow(tmp_table),1:ncol(tmp_table)]), alpha_code), 
                   na.rm=T) >= 3)){
            list_TF_vec[j] <- "except"}
        } else{
        if( (sum(str_detect(as.character(tmp_table[start_r:nrow(tmp_table),start_c:ncol(tmp_table)]), alpha_code), 
                 na.rm=T) >= 3)){
          list_TF_vec[j] <- "except"}
        }
      }
      if( (j==1) & (start_c %in% NA) ){
        list_TF_vec[j] <- FALSE
      }
  }
  return(list_TF_vec)
}


############################## step 2. 최종함수 dart_get 함수 정의 ##################################
# k=56
# path <- as.character(kospi200$링크[k]); index = k
# a=error_check_fun(k, TRUE)[[1]];a
# 최종확인: 케이씨시, 한일홀딩스, 

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
      ### 특이케이스 수정 !!!
      if(colnames(tmp)[3] == "거래내용" & colnames(tmp)[4] == "거래내용.1"){
        tmp[,3] <- paste0(tmp[,3], "@", tmp[,4]); tmp <- tmp[,-4] 
      }
      ### 2017, 2018 -> 전기, 당기
      tmp[1,] <- gsub("2018", "당기", gsub("2017", "전기", tmp[1,]))
      if(nrow(tmp) >= 2){ tmp[2,] <- gsub("2018", "당기", gsub("2017", "전기", tmp[2,]))}
      ### 2열에 거래내역, 3열에 회사명 있으면 2,3열 바꾸기
      if(colnames(tmp)[2:3] == c("거래내역", "회사명")){
        tmp <- tmp[c(1,3,2,4:ncol(tmp))]
      }
      ### 첫열에 계, 합계 텍스트 있는 행 제거 
      tmp <- tmp[!ifelse(tmp[,1] %in% c("계", "합계", "소계", "채권", "채무", "채무합계", "채권합계", "수익", "비용",
                                        "매출등합계", "매입등합계", "당기", "전기", "당기말", "전기말", "매출등", "매입등",
                                        "채권등", "채무등"), TRUE, FALSE),]
      ### 두번째열에 계, 합계 텍스트 있는 행 제거 
      start_r_tmp <- list_start_fun(tmp)[1]
      tmp <- tmp[!ifelse(tmp[start_r_tmp:nrow(tmp),2] %in% c("계", "합계", "소계", "채권", "채무", "대규모기업집단"), TRUE, FALSE),]
      
      ### colnames가 단위에 대한 내용이 아니라면 첫행으로 추가하기
      if(!any(str_detect(colnames(tmp), "단위"))){
        tmp <- rbind(colnames(tmp), tmp)}
      
      ### 구분에 대한 첫번째 케이스 - 구분 항목이 행으로 들어가 있는 경우 NA 없애기
      start_r_tmp <- list_start_fun(tmp)[1]
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
                        "종속,관계기업", "대규모기업집단", "신규설립으로인한현금출자및유상증자", "유의적인영향력을행사하는기업",
                        "연결대상종속기업", "유의적인영향력을미치는회사", "최상위지배기업", "회사에유의적인영향력을행사하는기업")
      if( ((start_r_tmp != nrow(tmp)) & (list_start_fun(tmp[-start_r_tmp,])[2] %in% c(start_c_tmp))) |
          ((start_r_tmp == nrow(tmp)) & (list_start_fun(tmp[-start_r_tmp,])[2] %in% c(NA))) ){
          if( ((start_c_tmp == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
              ((start_c_tmp == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
              ((start_c_tmp == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) | 
              ((start_c_tmp == 3) & (any(sub_name_tmp %in% tmp_col3))) ){
            narow_which <- which(tmp_col1 %in% sub_name_tmp)
            for(ii in 1:length(narow_which)){
              tmp[narow_which[ii],is.na(tmp[narow_which[ii],])] <- "값없음"}
          }
        } 
      
      if(!is.na(tmp[start_r_tmp,ncol(tmp)])){
        tmp[start_r_tmp, which(is.na(tmp[start_r_tmp,]))] <- 0
      }

      ### 구분항목이 행 열 모두 있는 경우 정제(중복데이터삭제)
      if(!all(tmp[,1] %in% sub_name_tmp == tmp[,2] %in% sub_name_tmp, na.rm=T)){
        if(sum(tmp[,1] %in% sub_name_tmp, na.rm=T) >= 3 
                   & sum(as.matrix(tmp[start_r_tmp:nrow(tmp),start_c_tmp:ncol(tmp)]) %in% sub_name_tmp, na.rm=T) >= 2){
          tmp <- tmp[,-1]
        }
      }
      
      ### 정리된 테이블에 밀림현상있는지 체크하고 수정(순서바꾸지말것!!!)
      if(( any(is.na(tmp[1:nrow(tmp),ncol(tmp)])) & 
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
      
      if(nrow(tmp) >= 2){ tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2])))) } else{
        tmp_col2 <- FALSE }
      if(nrow(tmp) >= 3){  tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3])))) } else{
        tmp_col3 <- FALSE }
      
      if( any(c(sub_name_tmp, "특수관계자", "값없음") %in% c(tmp_col2)) & any(c(sub_name_tmp, "특수관계자", "값없음") %in% c(tmp_col3)) ){
        tmp <- tmp[!(tmp_col2 %in% c(sub_name_tmp, "특수관계자")),]
      }
      
      # 두번째 케이스 : 구분이 따로 없고 앞선테이블에서 정의되지 않은 케이스 - '알수없음'처리
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
      list_case[i] <- list_case_fun(tmp, col_name_tmp, start_r_tmp2, start_c_tmp, list_TF_fun(list_df, T))
    }
  }

  # case1과 case1_3 합계 갯수가 짝수가 아니라면 오류 출력
  if(length(which(list_case %in% c("case1", "case1_3", "case1_5"))) %% 2 == 1){
    which_nonna <- which(!is.na(list_case)); which_nonna <- which_nonna[length(which_nonna)]   # 뒤에서 처리해주기 위해 for문 앞에서 생성
    for(jj in 1:length(list_case)){
      if(sum(c("기초", "대여", "회수", "기말", "증가", "감소") %in% as.matrix(rbind(colnames(list_df[[jj]]), list_df[[jj]])), 
             na.rm=T)>=3 ){
        list_case[jj] <- "case4"}
      # case1 내에 "자금대여" 있고 '당기/전기' 구분 없을 경우 case4
      if( (list_case[jj]  %in% c("case1", "case1_3", "case1_5")) & (any(str_detect(rbind(colnames(list_df[[jj]]),list_df[[jj]]), 
          paste(c("자금거래", "자금대여", "자금.대여", "당기말금액", "장기대여금"), collapse = "|")))) ){ #기준 배당금 제외해야함
          list_case[jj] <- "case4"}
      if( (jj == which_nonna) & (list_case[jj]  %in% c("case1", "case1_3", "case1_5")) & 
          (any(str_detect(rbind(colnames(list_df[[jj]]),list_df[[jj]]), "배당금"))) ){
        list_case[jj] <- "case4"}
    }
    if(length(which(list_case %in% c("case1", "case1_3", "case1_5"))) %% 2 == 1){
      print("error:: wrong case number in list_case_fun function!")}
  }
  
  ### except table을 위한 수정
  list_TF <- except_fun(alpha_code, list_TF, list_df)
  
  final_table <- data.frame()
  tmp <- data.frame()
  for(i in 1:length(list_df)){
    if(list_TF[i] == "TRUE"){
      ### list_df의 테이블을 tmp에 저장
      tmp <- list_df[[i]]
      
      ### 전처리과정
      {
      ### tmp text_clean 수행
      tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)), stringsAsFactors = F)
      colnames(tmp) <- text_clean(colnames(tmp))
      ### 특이케이스 수정 !!!
      if(colnames(tmp)[3] == "거래내용" & colnames(tmp)[4] == "거래내용.1"){
        tmp[,3] <- paste0(tmp[,3], "@", tmp[,4]); tmp <- tmp[,-4] 
      }
      ### 2017, 2018 -> 전기, 당기
      tmp[1,] <- gsub("2018", "당기", gsub("2017", "전기", tmp[1,]))
      if(nrow(tmp) >= 2){ tmp[2,] <- gsub("2018", "당기", gsub("2017", "전기", tmp[2,])) }
      ### 2열에 거래내역, 3열에 회사명 있으면 2,3열 바꾸기
      if(colnames(tmp)[2:3] == c("거래내역", "회사명")){
        tmp <- tmp[c(1,3,2,4:ncol(tmp))]
      }
      ### 첫열에 계, 합계 텍스트 있는 행 제거 
      tmp <- tmp[!ifelse(tmp[,1] %in% c("계", "합계", "소계", "채권", "채무", "채무합계", "채권합계", "수익", "비용",
                                        "매출등합계", "매입등합계", "당기", "전기", "당기말", "전기말", "매출등", "매입등",
                                        "채권등", "채무등"), TRUE, FALSE),]
      ### 두번째열에 계, 합계 텍스트 있는 행 제거 
      start_r <- list_start_fun(tmp)[1]
      tmp <- tmp[!ifelse(tmp[start_r:nrow(tmp),2] %in% c("계", "합계", "소계", "채권", "채무", "대규모기업집단"), TRUE, FALSE),]
      
      ### colnames가 단위에 대한 내용이 아니라면 첫행으로 추가하기
      if(!any(str_detect(colnames(tmp), "단위"))){
        tmp <- rbind(colnames(tmp), tmp)}
      
      ### 특이케이스 제거 - colname중복되고 NA있으면 아래꺼 제거
      start_r <- list_start_fun(tmp)[1]
      if(any(is.na(tmp[1:start_r-1,])) & start_r == 3){
        if(sum(tmp[1,] == tmp[2,], na.rm = T) >=3){
          tmp <- tmp[-2,]
        }
      }
      
      ### 구분에 대한 첫번째 케이스 - 구분 항목이 행으로 들어가 있는 경우 NA 없애기
      start_r <- list_start_fun(tmp)[1]
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
                        "종속,관계기업","대규모기업집단", "신규설립으로인한현금출자및유상증자", "유의적인영향력을행사하는기업",
                        "연결대상종속기업", "유의적인영향력을미치는회사", "최상위지배기업","회사에유의적인영향력을행사하는기업")
      
      # if(start_r != nrow(tmp)){
      #   if(list_start_fun(tmp[-start_r,])[2] %in% c(start_c)){
      #     if( ((start_c == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
      #         ((start_c == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
      #         ((start_c == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
      #       narow_which <- which(tmp_col1 %in% sub_name_tmp)
      #       for(ii in 1:length(narow_which)){
      #         tmp[narow_which[ii],is.na(tmp[narow_which[ii],])] <- "값없음"}
      #     }
      #   }
      # }
      if( ((start_r != nrow(tmp)) & (list_start_fun(tmp[-start_r,])[2] %in% c(start_c))) |
          ((start_r == nrow(tmp)) & (list_start_fun(tmp[-start_r,])[2] %in% c(NA))) ){
        if( ((start_c == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
            ((start_c == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
            ((start_c == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) | 
            ((start_c == 3) & (any(sub_name_tmp %in% tmp_col3))) ){
          narow_which <- which(tmp_col1 %in% sub_name_tmp)
          for(ii in 1:length(narow_which)){
            tmp[narow_which[ii],is.na(tmp[narow_which[ii],])] <- "값없음"}
        }
      } 
      if(!is.na(tmp[start_r,ncol(tmp)])){
        tmp[start_r, which(is.na(tmp[start_r,]))] <- 0
      }
      
      ### 구분항목이 행 열 모두 있는 경우 정제(중복데이터삭제)
      if(!all(tmp[,1] %in% sub_name_tmp == tmp[,2] %in% sub_name_tmp, na.rm=T)){
        if(sum(tmp[,1] %in% sub_name_tmp, na.rm=T) >= 3 
           & sum(as.matrix(tmp[start_r:nrow(tmp),start_c:ncol(tmp)]) %in% sub_name_tmp, na.rm=T) >= 2){
          tmp <- tmp[,-1]
        }
      }
      
      ### 정리된 테이블에 밀림현상있는지 체크하고 수정(순서바꾸지말것!!!)
      if(( any(is.na(tmp[1:nrow(tmp),ncol(tmp)])) & 
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
          ((start_c == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) | 
          ((start_c == 3) & (any(sub_name_tmp %in% tmp_col3))) ){
        insert_col <- na.locf(ifelse(str_detect(tmp_col1, paste(sub_name_tmp, collapse = '|')) == TRUE, tmp_col1, "값없음"),
                              na.rm=FALSE, fromLast = FALSE)
        insert_col[is.na(insert_col)] <- "값없음"
        tmp <- cbind(insert_col, tmp)
        tmp <- tmp[!(tmp_col1 %in% sub_name_tmp),]
        colnames(tmp)[1:2] <- c("구분", "회사명")
        tmp[,1] <- as.character(tmp[,1])
      }
      
      if(nrow(tmp) >= 2){ tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2])))) } else{
        tmp_col2 <- FALSE }
      if(nrow(tmp) >= 3){ tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3])))) } else{
        tmp_col3 <- FALSE }
      
      if( any(c(sub_name_tmp, "특수관계자", "값없음") %in% c(tmp_col2)) & any(c(sub_name_tmp, "특수관계자", "값없음") %in% c(tmp_col3)) ){
        tmp <- tmp[!(tmp_col2 %in% c(sub_name_tmp, "특수관계자")),]
      }
      
      #두번째 케이스 : 구분이 따로 없고 앞선테이블에서 정의되지 않은 케이스 - '알수없음'처리
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
      }
      
      ### 케이스별로 table정리 
      {
      if(tmp_case %in% c("case1", "case1_3", "case1_5")){
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
          if(nrow(tmp) - start_r_after == 0){
            tmp <- tmp[start_r_after:nrow(tmp), start_c:ncol(tmp)]
            tmp <- as.data.frame(lapply(tmp, function(x) ifelse(x =="-", 0, x)), stringsAsFactors = F)
            tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(gsub(",", "", gsub("\\s", "", gsub("\\)","",
                                                                                                       gsub("\\(","-",x))))) * unit), stringsAsFactors = F)
            colnames(tmp) <- col_name[start_c:length(col_name)]
            # category1을 다시 만들어서 table 재정의_col
            category1 <- t(as.data.frame(str_split(row_name, "@")))
            tmp <- cbind(category1[,1], category1[,2], list_class[i], tmp)
            colnames(tmp)[1:3] <- c("구분", "회사명", "당기_전기")
            tmp[,1:start_c] <- as.data.frame(lapply(tmp[,1:start_c], as.character), stringsAsFactors = F)
            tmp <- tmp[, c((ncol(tmp)-2):ncol(tmp), 1:(ncol(tmp)-3))]
            } else if(ncol(tmp) - start_c == 0){
            tmp <- tmp[start_r_after:nrow(tmp),]
            tmp[,start_c] <- ifelse(tmp[,start_c] =="-", 0, tmp[,start_c])
            tmp[,start_c] <- as.numeric(gsub(",", "", gsub("\\s", "", gsub("\\)","", gsub("\\(","-",tmp[,start_c]))))) * unit
            # category1를 다시 만들어서 table 재정의_col
            tmp$당기_전기 <- list_class[i]
            tmp <- tmp[, c(1:(start_c-1), ncol(tmp), start_c)]
            colnames(tmp) <- c("구분", "회사명", "당기_전기",col_name[length(col_name)])
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
            tmp <- tmp[, c((ncol(tmp)-2):ncol(tmp), 1:(ncol(tmp)-3))]
            }
          tmp <- tmp[!str_detect(rownames(tmp), paste(c("소계", "합계", "기타"), collapse = '|')),]
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
          tmp <- tmp[!str_detect(rownames(tmp), paste(c("소계", "합계", "기타"), collapse = '|')),]
          rownames(tmp) <- NULL
        } else if( tmp_case == "case1_5"){
          
        ### 해당 테이블이 case1_5일때 수행하는 함수
          if(dim(tmp[,1:start_c])[2] == 5){
            tmp[,3] <- paste0(tmp[,3], "@", tmp[,4]); tmp <- tmp[,-4]
            col_name <- col_name[-3]
          }
          # # col, row 이름 명명하기
          # if(start_r_after==3){
          #   col_name <- paste0(col_name, "@", tmp[2,])} else if(start_r_after == 4){
          #     col_name <- paste0(col_name, "@", tmp[2,], "@", tmp[3,])
          #   } else {print(paste("error:: col_name error in dart_get case1_5!", i, "단계에서"))}
          # tmp clean 작업
          tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)), stringsAsFactors = F)
          colnames(tmp) <- col_name
          tmp <- tmp[start_r_after:nrow(tmp), ]  
          # 테이블 틀 만들기
          tmp <- melt(tmp, id=c(colnames(tmp)[1], colnames(tmp)[2], colnames(tmp)[3]))
          # 숫자만 있는 value ()는 음수처리
          unit <- list_unit_fun(list_df, i,  get_startend(path, 2))
          tmp$value <- as.numeric(lapply(tmp$value, function(x) gsub("\\)","",gsub("\\(","-",gsub(",","",gsub("-",0,x)))) )) * unit
          tmp$당기_전기 <- list_class[i]
          tmp$계정과목 <- paste0(tmp[,3], "@", tmp[,4])
          tmp <- tmp[,c(1,2, ncol(tmp)-1, ncol(tmp), ncol(tmp)-2)]
          colnames(tmp)[1:2] <- c("구분", "회사명")
          tmp <- dcast(tmp, 구분 + 회사명 + 당기_전기 ~ 계정과목, fun.aggregate = sum)
          rownames(tmp) <- NULL
        }
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
        if(ncol(tmp) - start_c == 0){
          tmp <- tmp[start_r_after:nrow(tmp),]
          tmp[,start_c] <- ifelse(tmp[,start_c] =="-", 0, tmp[,start_c])
          tmp[,start_c] <- as.numeric(gsub(",", "", gsub("\\s", "", gsub("\\)","", gsub("\\(","-",tmp[,start_c]))))) * unit
          tmp <- cbind(tmp[,1:2], "값없음", tmp[,3])
          colnames(tmp)[4] <- col_name[start_c]
        } else{
          tmp <- tmp[start_r_after:nrow(tmp), start_c:ncol(tmp)]
          tmp <- as.data.frame(lapply(tmp, function(x) ifelse(x =="-", 0, x)), stringsAsFactors = F)
          tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(gsub(",", "", gsub("\\s", "", gsub("\\)","",
                                                                                                     gsub("\\(","-",x))))) * unit), stringsAsFactors = F)
          colnames(tmp) <- col_name[start_c:length(col_name)]
          # category1를 다시 만들어서 table 재정의_col
          category1 <- t(as.data.frame(str_split(row_name, "@")))
          tmp <- cbind(category1[,1], category1[,2], "값없음", tmp)
        }
        colnames(tmp)[1:3] <- c("구분", "회사명", "당기_전기")
        tmp[,1:start_c] <- as.data.frame(lapply(tmp[,1:start_c], as.character), stringsAsFactors = F)
        rownames(tmp) <- NULL
      } else if (tmp_case == "case5"){
      
      ### 해당 테이블이 case5일때 수행하는 함수
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
        tmp <- melt(tmp, id=c(colnames(tmp)[1], colnames(tmp)[2], colnames(tmp)[3]))
        # 숫자만 있는 value ()는 음수처리
        unit <- list_unit_fun(list_df, i,  get_startend(path, 2))
        tmp$value <- as.numeric(lapply(tmp$value, function(x) gsub("\\)","",gsub("\\(","-",gsub(",","",gsub("-",0,x)))) )) * unit
        category1 <- t(as.data.frame(str_split(tmp$variable, "@")))
        tmp$당기_전기 <- category1[,1]
        tmp$대분류 <- category1[,2]
        tmp$계정과목 <- paste0(tmp$대분류, "@", tmp[,3])
        tmp <- tmp[,c(1,2,ncol(tmp)-2, ncol(tmp), ncol(tmp)-3)]
        colnames(tmp)[1:2] <- c("구분", "회사명")
        tmp <- dcast(tmp, 구분 + 회사명 + 당기_전기 ~ 계정과목, fun.aggregate = sum)
        rownames(tmp) <- NULL
      } else {print(paste("error:: no case in dart_get case3!", i, "에서"))}
      }
      
      ### table 합치기
      # if(is.na(tmp)==TRUE){print("error is.na true")}   # 최종점검용
      tmp[,1:3] <- as.data.frame(lapply(tmp[,1:3], function(x) ifelse(is.na(x)==TRUE, "값없음", x)), stringsAsFactors = F)
      tmp[,4:ncol(tmp)] <- as.data.frame(lapply(tmp[,4:ncol(tmp)], function(x) ifelse(is.na(x)==TRUE, 0, x)), stringsAsFactors = F)
      colnames(tmp)[duplicated(colnames(tmp))] <- paste0(colnames(tmp)[duplicated(colnames(tmp))],2)
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
  
  ### final table 다듬기
  del_r <- which( (rowSums(final_table[,5:ncol(final_table)], na.rm=T) %in% 0) | (final_table[,3] %in% "값없음") |
                    final_table[,3]=="NA" )
  del_c <- which(colSums(final_table[,5:ncol(final_table)], na.rm=T)==0) + 4
  if( length(del_r) != 0){
    final_table <- final_table[-del_r,]    
  } else if( length(del_c) != 0){
    final_table <- final_table[, -del_c]    
  }
  if("값없음" %in% final_table$구분){
    unique_cl <- final_table[,2:3] %>% filter(구분 != "값없음") %>% filter(구분 != "알수없음") %>% unique()
    # 차원이 다르면 문제 발생!
    if(length(unique(unique_cl[,2])) == length(unique(unique_cl)) ){
      final_table[,2] <- left_join(final_table[,2:3], by="회사명", unique_cl)[,3]      
    }
  }
  final_table[is.na(final_table[,2]),2] <- "값없음"
  
  ### except table 저장
  list_except = list()
  for (i in 1:length(list_TF)){
    if(list_TF[i] == "except"){
      tmp_target <- cbind(as.character(kospi200[index,1]), list_df[[i]])
      if(length(list_except) == 0){
        list_except <- tmp_target
      } else{
        list_except <- rbind.fill(list_except, tmp_target)        
      }
    }
  }
  
  ### 최종결과
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

error_check_fun(1)   ### '1_(주)BNK금융지주' -> 확인완료(16,21)
error_check_fun(2)   ### '2_(주)DB하이텍' -> 확인완료(18,14)
error_check_fun(3)   ### '3_(주)KB금융지주'-> 확인완료(24,23)
error_check_fun(4)   ### '4_(주)강원랜드'-> 확인완료(6,6)
error_check_fun(5)   ### '5_(주)녹십자' -> 확인완료(43,22)
error_check_fun(6)   ### '6_(주)녹십자홀딩스' -> 확인완료(51,13)
error_check_fun(7)   ### '7_(주)농심' -> 확인완료(51,12)
error_check_fun(8)   ### '8_(주)대교' -> 확인완료(80,18)
error_check_fun(9)   ### '9_(주)대우건설' -> 확인완료(96,152)
error_check_fun(10)   ### '10_(주)대웅제약' -> 확인완료(56,25)
error_check_fun(11)   ### '11_(주)대한항공' -> 확인완료(32,8)
error_check_fun(12)   ### '12_(주)동서' -> 확인완료(30,18)
error_check_fun(13)   ### '13_(주)동양'-> 확인완료(39,27)
error_check_fun(14)   ### '14_(주)동원에프앤비' -> 확인완료(41,15)
error_check_fun(15)   ### '15_(주)두산' -> 확인완료(25,22)
error_check_fun(16)   ### '16_(주)락앤락' -> 확인완료(36,25)
error_check_fun(17)   ### '17_(주)만도' -> 확인완료(56,31)
error_check_fun(18)   ### '18_(주)무학'-> 확인완료(26,17)
error_check_fun(19)   ### '19_(주)비지에프' -> 확인완료(23,10)
error_check_fun(20)   ### '20_(주)비지에프리테일' -> 확인완료(26,10)
error_check_fun(21)   ### '21_(주)빙그레' -> 확인완료(10,11)
error_check_fun(22)   ### '22_(주)삼양사' -> 확인완료(34,19)
error_check_fun(23)   ### '23_(주)삼양홀딩스' -> 확인완료(34,11)
error_check_fun(24)   ### '24_(주)세아베스틸' -> 확인완료(50,24)
error_check_fun(25)   ### '25_(주)셀트리온‘ -> 확인완료(15,15)
error_check_fun(26)   ### '26_(주)신세계‘ -> 확인완료(40,16)
error_check_fun(27)   ### '27_(주)신한금융지주회사‘ -> 확인완료(26,13)
error_check_fun(28)   ### '28_(주)아모레퍼시픽‘ -> 확인완료(69,17)
error_check_fun(29)   ### '29_(주)아모레퍼시픽그룹‘ -> 확인완료(29,15)
error_check_fun(30)   ### '30_(주)에스비에스‘ -> 확인완료(45,27)
error_check_fun(31)   ### '31_(주)에스원‘ -> 확인완료(21,14)
error_check_fun(32)   ### '32_(주)에스피씨삼립‘ -> 확인완료(84,14)
error_check_fun(33)   ### '33_(주)엔씨소프트‘ -> 확인완료(27,24)
error_check_fun(34)   ### '34_(주)엘에스‘ -> 확인완료(57,11)
error_check_fun(35)   ### '35_(주)엘에프‘ -> 확인완료(45,20)
error_check_fun(36)   ### '36_(주)엘지‘ -> 확인완료(50,16)
error_check_fun(37)   ### '37_(주)엘지상사‘ -> 확인완료(89,17)
error_check_fun(38)   ### '38_(주)엘지생활건강‘ -> 확인완료(111,12)
error_check_fun(39)   ### '39_(주)엘지유플러스‘ -> 확인완료(63,11)
error_check_fun(40)   ### '40_(주)엘지하우시스‘ -> 확인완료(47,15)
error_check_fun(41)   ### '41_(주)엘지화학‘ -> 확인완료(86,23)
error_check_fun(42)   ### '42_(주)영원무역‘ -> 확인완료(134,15)
error_check_fun(43)   ### '43_(주)영풍‘ -> 확인완료(36,22)
error_check_fun(44)   ### '44_(주)오뚜기‘ -> 확인완료(44,15)
error_check_fun(45)   ### '45_(주)오리온‘ -> 확인완료(36,20)
error_check_fun(46)   ### '46_(주)오리온홀딩스‘ -> 확인완료(45,22)
error_check_fun(47)   ### '47_(주)우리은행‘ -> 확인완료(89,19)
error_check_fun(48)   ### '48_(주)유니드‘ -> 확인완료(23,17)
error_check_fun(49)   ### '49_(주)유한양행‘ -> 확인완료(48,22)
error_check_fun(50)   ### '50_(주)이노션‘ -> 확인완료(23,10)

error_check_fun(51)   ### '51_(주)이마트‘ -> 확인완료(57,12)
error_check_fun(52)   ### '52_(주)제일기획‘ -> 확인완료(52,21)
error_check_fun(53)   ### '53_(주)종근당‘ -> 확인완료(38,35)
error_check_fun(54)   ### '54_(주)지에스‘ -> 확인완료(52,8)
error_check_fun(55)   ### '55_(주)지에스리테일‘ -> 확인완료(39,22)
error_check_fun(56)   ### '56_(주)카카오‘ -> 확인완료(128,17)
error_check_fun(57)   ### '57_(주)케이씨씨‘ -> 확인완료(43,26)
error_check_fun(58)   ### '58_(주)케이티‘ -> 확인완료(93,19)
error_check_fun(59)   ### '59_(주)케이티앤지‘ -> 확인완료(76,13)
error_check_fun(60)   ### '60_(주)팜스코‘ -> 확인완료(68,33)
error_check_fun(61)   ### '61_(주)포스코' -> 확인완료(41,17)
error_check_fun(62)   ### '62_(주)포스코대우‘ -> 확인완료(126,16)
error_check_fun(63)   ### '63_(주)풍산‘ -> 확인완료(36,17)
error_check_fun(64)   ### '64_(주)하나금융지주‘ -> 확인완료(26,12)
error_check_fun(65)   ### '65_(주)한라홀딩스‘ -> 확인완료(74,29)
error_check_fun(66)   ### '66_(주)한샘‘ -> 확인완료(34,10)
error_check_fun(67)   ### '67_(주)한섬‘ -> 확인완료(26,23)
error_check_fun(68)   ### '68_(주)한솔케미칼‘ -> 확인완료(28,16)
error_check_fun(69)   ### '69_(주)한진중공업‘ -> 확인완료(22,20)
error_check_fun(70)   ### '70_(주)한화‘ -> 확인완료(75,16)
error_check_fun(71)   ### '71_(주)현대그린푸드‘ -> 확인완료(49,14)
error_check_fun(72)   ### '72_(주)현대리바트‘ -> 확인완료(60,27)
error_check_fun(73)   ### '73_(주)현대미포조선‘ -> 확인완료(27,17)
error_check_fun(74)   ### '74_(주)현대백화점‘ -> 확인완료(58,16)
error_check_fun(75)   ### '75_(주)현대홈쇼핑‘ -> 확인완료(39,17)
error_check_fun(76)   ### '76_(주)호텔신라‘ -> 확인완료(40,22)
error_check_fun(77)   ### '77_(주)효성‘ -> 확인완료(74,25)
error_check_fun(78)   ### '78_(주)후성‘ -> 확인완료(31,19)
error_check_fun(79)   ### '79_DB손해보험(주)‘ -> 확인완료(108,28)
error_check_fun(80)   ### '80_LG이노텍(주)‘ -> 확인완료(111,40)
error_check_fun(81)   ### '81_LS산전(주)‘ -> 확인완료(35,12)
error_check_fun(82)   ### '82_OCI(주)‘ -> 확인완료(75,27)
error_check_fun(83)   ### '83_S&T모티브(주)‘ -> 확인완료(26,13)
error_check_fun(84)   ### '84_SK이노베이션(주)‘ -> 확인완료(60,14)
error_check_fun(85)   ### '85_고려아연(주)‘ -> 확인완료(34,14)
error_check_fun(86)   ### '86_고려제강(주)‘ -> 확인완료(75,20)
error_check_fun(88)   ### '88_금호석유화학(주)‘ -> 확인완료(26,12)
error_check_fun(89)   ### '89_금호타이어(주)‘ -> 확인완료(34,13)
error_check_fun(90)   ### '90_기아자동차(주)‘ -> 확인완료(42,16)
error_check_fun(91)   ### '91_남양유업(주)‘ -> 확인완료(7,13)
error_check_fun(92)   ### '92_남해화학(주)‘ -> 확인완료(44,11)
error_check_fun(93)   ### '93_네이버(주)‘ -> 확인완료(109,16)
error_check_fun(94)   ### '94_넥센타이어(주)‘ -> 확인완료(39,16)
error_check_fun(95)   ### '95_넷마블(주)‘ -> 확인완료(94,23)
error_check_fun(96)   ### '96_대덕전자(주)' -> 확인완료(10,27)
error_check_fun(97)   ### '97_대림산업(주)‘ -> 확인완료(95,19)
error_check_fun(98)   ### '98_대상(주)‘ -> 확인완료(70,18)
error_check_fun(99)   ### '99_대우조선해양(주)‘ -> 확인완료(22,22)
error_check_fun(100)   ### '100_대한유화(주)‘ -> 확인완료(10,10)

error_check_fun(101)   ### '101_동국제강(주)‘ -> 확인완료(39,17)
error_check_fun(102)   ### '102_동아쏘시오홀딩스(주)‘ -> 확인완료(27,18)
error_check_fun(103)   ### '103_동아에스티(주)‘ -> 확인완료(29,21)
error_check_fun(104)   ### '104_동원시스템즈(주)‘ -> 확인완료(44,13)
error_check_fun(105)   ### '105_두산밥캣(주)‘ -> 확인완료(17,12)
error_check_fun(106)   ### '106_두산인프라코어(주)‘ -> 확인완료(20,24)
error_check_fun(107)   ### '107_두산중공업(주)‘ -> 확인완료(88,25)
error_check_fun(108)   ### '108_롯데쇼핑(주)‘ -> 확인완료(92,20)
error_check_fun(109)   ### '109_롯데정밀화학(주)‘ -> 확인완료(23,12)
error_check_fun(110)   ### '110_롯데지주(주)‘ -> 확인완료(69,16)
error_check_fun(111)   ### '111_롯데칠성음료(주)‘ -> 확인완료(50,20)
error_check_fun(112)   ### '112_롯데케미칼(주)‘ -> 확인완료(63,12)
error_check_fun(113)   ### '113_롯데푸드(주)‘ -> 확인완료(24,12)
error_check_fun(114)   ### '114_롯데하이마트(주)‘ -> 확인완료(34,13)
error_check_fun(115)   ### '115_메리츠종합금융증권(주)‘ -> 확인완료(501,10)
error_check_fun(116)   ### '116_미래에셋대우(주)‘ -> 확인완료(26,15)
error_check_fun(117)   ### '117_보령제약(주)‘ -> 확인완료(32,17)
error_check_fun(118)   ### '118_부광약품(주)‘ -> 확인완료(10,22)
error_check_fun(119)   ### '119_삼성SDI(주)‘ -> 확인완료(49,14)
error_check_fun(120)   ### '120_삼성물산(주)‘ -> 확인완료(41,14)
error_check_fun(121)   ### '121_삼성바이오로직스(주)‘ -> 확인완료(35,15)
error_check_fun(122)   ### '122_삼성생명보험(주)‘ -> 확인완료(166,21)
error_check_fun(123)   ### '123_삼성에스디에스(주)‘ -> 확인완료(74,11)
error_check_fun(124)   ### '124_삼성엔지니어링(주)‘ -> 확인완료(74,22)
error_check_fun(125)   ### '125_삼성전기(주)‘ -> 확인완료(39,21)
error_check_fun(126)   ### '126_(주)삼성전자'-> 확인완료(45,10)
error_check_fun(127)   ### '127_삼성중공업(주)‘ -> 확인완료(17,8)
error_check_fun(128)   ### '128_삼성증권(주)‘ -> 확인완료(54,17)
error_check_fun(129)   ### '129_삼성카드(주)‘ -> 확인완료(36,15)
error_check_fun(130)   ### '130_삼성화재해상보험(주)‘ -> 확인완료(50,20)
error_check_fun(131)   ### '131_세방전지(주)‘ -> 확인완료(25,15)
error_check_fun(132)   ### '132_쌍용양회공업(주)‘ -> 확인완료(23,17)
error_check_fun(133)   ### '133_쌍용자동차(주)‘ -> 확인완료(16,11)
error_check_fun(134)   ### '134_씨제이(주)‘ -> 확인완료(24,10)
error_check_fun(135)   ### '135_씨제이대한통운(주)‘ -> 확인완료(282,15)
error_check_fun(136)   ### '136_씨제이씨지브이(주)‘ -> 확인완료(127,20)
error_check_fun(137)   ### '137_씨제이제일제당(주)‘ -> 확인완료(138,16)
error_check_fun(138)   ### '138_아이에스동서(주)‘ -> 확인완료(90,16)
error_check_fun(139)   ### '139_에스엘(주)‘ -> 확인완료(38,18)
error_check_fun(140)   ### '140_에스케이(주)‘ -> 확인완료(62,14)
error_check_fun(141)   ### '141_에스케이네트웍스(주)‘ -> 확인완료(69,15)
error_check_fun(142)   ### '142_에스케이디스커버리(주)‘ -> 확인완료(38,14)
error_check_fun(143)   ### '143_에스케이씨(주)‘ -> 확인완료(74,11)
error_check_fun(144)   ### '144_에스케이케미칼(주)‘ -> 확인완료(51,12)
error_check_fun(145)   ### '145_에스케이텔레콤(주)‘ -> 확인완료(53,11)
error_check_fun(146)   ### '146_에스케이하이닉스(주)‘ -> 확인완료(36,12)
error_check_fun(147)   ### '147_에쓰-오일(주)‘ -> 확인완료(19,15)
error_check_fun(148)   ### '148_에이치디씨(주)‘ -> 확인완료(34,17)
error_check_fun(149)   ### '149_에이치디씨현대산업개발(주)‘ -> 확인완료(28,12)

error_check_fun(150)   ### '150_에이케이홀딩스(주)‘ -> 확인완료(56,9)
error_check_fun(151)   ### '151_엔에이치투자증권(주)‘ -> 확인완료(214,22)
error_check_fun(152)   ### '152_엘아이지넥스원(주)‘ -> 확인완료(18,13)
error_check_fun(153)   ### '153_엘지디스플레이(주)‘ -> 확인완료(114,21)
error_check_fun(154)   ### '154_엘지전자(주)‘ -> 확인완료(128,38)
error_check_fun(155)   ### '155_영진약품(주)‘ -> 확인완료(15,10)
error_check_fun(156)   ### '156_일양약품(주)‘ -> 확인완료(6,7)
error_check_fun(157)   ### '157_일진머티리얼즈(주)‘ -> 확인완료(22,12)
error_check_fun(158)   ### '158_제이더블유중외제약(주)‘ -> 확인완료(38,29)
error_check_fun(159)   ### '159_제이더블유홀딩스(주)‘ -> 확인완료(76,17)
error_check_fun(160)   ### '160_중소기업은행‘ -> 확인완료(104,23)
error_check_fun(161)   ### '161_지에스건설(주)‘ -> 확인완료(151,18)
error_check_fun(162)   ### '162_코스맥스(주)‘ -> 확인완료(34,12)
error_check_fun(163)   ### '163_코오롱인더스트리(주)‘ -> 확인완료(62,13)
error_check_fun(164)   ### '164_코웨이(주)‘ -> 확인완료(12,15)
error_check_fun(165)   ### '165_쿠쿠홀딩스(주)‘ -> 확인완료(14,16)
error_check_fun(166)   ### '166_태광산업(주)‘ -> 확인완료(72,17)
error_check_fun(167)   ### '167_팬오션(주)‘ -> 확인완료(37,16)
error_check_fun(168)   ### '168_하이트진로(주)‘ -> 확인완료(38,33)
error_check_fun(169)   ### '169_한국가스공사(주)‘ -> 확인완료(119,79)  #실제항목이 NA
error_check_fun(170)   ### '170_한국단자공업(주)‘ -> 확인완료(28,31)
error_check_fun(171)   ### '171_한국쉘석유(주)‘ -> 확인완료(34,15)
error_check_fun(172)   ### '172_한국전력공사(주)‘ -> 확인완료(194,22)
error_check_fun(173)   ### '173_한국전력기술(주)‘ -> 확인완료(33,13)
error_check_fun(174)   ### '174_한국콜마(주)‘ -> 확인완료(48,18)
error_check_fun(175)   ### '175_한국타이어(주)‘ -> 확인완료(138,14)
error_check_fun(176)   ### '176_한국타이어월드와이드(주)‘ -> 확인완료(18,14)
error_check_fun(177)   ### '177_한국투자금융지주(주)‘ -> 확인완료(28,20)
error_check_fun(178)   ### '178_한국항공우주산업(주)‘ -> 확인완료(12,10)
error_check_fun(179)   ### '179_한미사이언스(주)‘ -> 확인완료(17,13)
error_check_fun(180)   ### '180_한미약품(주)‘ -> 확인완료(26,18)
error_check_fun(181)   ### '181_한세실업(주)‘ -> 확인완료(53,20)
error_check_fun(182)   ### '182_한온시스템(주)‘ -> 확인완료(10,14)
error_check_fun(183)   ### '183_한올바이오파마(주)‘ -> 확인완료(41,16)
error_check_fun(184)   ### '184_한일홀딩스(주)‘ -> 확인완료(30,25)
error_check_fun(185)   ### '185_한전케이피에스(주)‘ -> 확인완료(46,20)
error_check_fun(186)   ### '186_한화생명보험(주)‘ -> 확인완료(80,43)
error_check_fun(187)   ### '187_한화에어로스페이스(주)‘ -> 확인완료(79,25)
error_check_fun(188)   ### '188_한화케미칼(주)‘ -> 확인완료(107,15)
error_check_fun(189)   ### '189_현대건설(주)‘ -> 확인완료(49,22) 
error_check_fun(190)   ### '190_현대글로비스(주)‘ -> 확인완료(26,12)
error_check_fun(191)   ### '191_현대로템(주)‘ -> 확인완료(44,23)
error_check_fun(192)   ### '192_현대모비스(주)‘ -> 확인완료(41,13)
error_check_fun(193)   ### '193_현대엘리베이터(주)‘ -> 확인완료(34,17)
error_check_fun(194)   ### '194_현대위아(주)‘ -> 확인완료(31,12)
error_check_fun(195)   ### '195_현대자동차(주)‘ -> 확인완료(30,12)
error_check_fun(196)   ### '196_현대제철(주)‘ -> 확인완료(54,14)
error_check_fun(197)   ### '197_현대중공업(주)‘ -> 확인완료(44,13)
error_check_fun(198)   ### '198_현대중공업지주(주)‘ -> 확인완료(21,14)
error_check_fun(199)   ### '199_현대해상화재보험(주)‘ -> 확인완료(12,26)
error_check_fun(200)   ### '200_휴성중공업(주)‘ -> 확인완료(86,15)
error_check_fun(201)   ### '201_휴켐스(주)‘ -> 확인완료(19,17)


############################## step 3-2. table 생성 ##################################

### 테이블생성
table_1 <- error_check_fun(1, TRUE)   # '1_(주)BNK금융지주'
table_2 <- error_check_fun(2, TRUE)   # '2_(주)DB하이텍'
table_3 <- error_check_fun(3, TRUE)   # '3_(주)KB금융지주’
table_4 <- error_check_fun(4, TRUE)   # '4_(주)강원랜드’
table_5 <- error_check_fun(5, TRUE)   # '5_(주)녹십자'
table_6 <- error_check_fun(6, TRUE)   # '6_(주)녹십자홀딩스'
table_7 <- error_check_fun(7, TRUE)   # '7_(주)농심’
table_8 <- error_check_fun(8, TRUE)   # '8_(주)대교'
table_9 <- error_check_fun(9, TRUE)   # '9_(주)대우건설’
table_10 <- error_check_fun(10, TRUE)   # '10_(주)대웅제약’
table_11 <- error_check_fun(11, TRUE)   # '11_(주)대한항공’
table_12 <- error_check_fun(12, TRUE)   # '12_(주)동서'
table_13 <- error_check_fun(13, TRUE)   # '13_(주)동양'
table_14 <- error_check_fun(14, TRUE)   # '14_(주)동원에프앤비'
table_15 <- error_check_fun(15, TRUE)   # '15_(주)두산’
table_16 <- error_check_fun(16, TRUE)   # '16_(주)락앤락'
table_17 <- error_check_fun(17, TRUE)   # '17_(주)만도'
table_18 <- error_check_fun(18, TRUE)   # '18_(주)무학'
table_19 <- error_check_fun(19, TRUE)   # '19_(주)비지에프'
table_20 <- error_check_fun(20, TRUE)   # '20_(주)비지에프리테일'
table_21 <- error_check_fun(21, TRUE)   # '21_(주)빙그레'
table_22 <- error_check_fun(22, TRUE)   # '22_(주)삼양사'
table_23 <- error_check_fun(23, TRUE)   # '23_(주)삼양홀딩스'
table_24 <- error_check_fun(24, TRUE)   # '24_(주)세아베스틸'
table_25 <- error_check_fun(25, TRUE)   # '25_(주)셀트리온‘
table_26 <- error_check_fun(26, TRUE)   # '26_(주)신세계‘
table_27 <- error_check_fun(27, TRUE)   # '27_(주)신한금융지주회사‘
table_28 <- error_check_fun(28, TRUE)   # '28_(주)아모레퍼시픽‘
table_29 <- error_check_fun(29, TRUE)   # '29_(주)아모레퍼시픽그룹‘
table_30 <- error_check_fun(30, TRUE)   # '30_(주)에스비에스‘
table_31 <- error_check_fun(31, TRUE)   # '31_(주)에스원‘
table_32 <- error_check_fun(32, TRUE)   # '32_(주)에스피씨삼립‘
table_33 <- error_check_fun(33, TRUE)   # '33_(주)엔씨소프트‘
table_34 <- error_check_fun(34, TRUE)   # '34_(주)엘에스‘
table_35 <- error_check_fun(35, TRUE)   # '35_(주)엘에프‘
table_36 <- error_check_fun(36, TRUE)   # '36_(주)엘지‘
table_37 <- error_check_fun(37, TRUE)   # '37_(주)엘지상사‘
table_38 <- error_check_fun(38, TRUE)   # '38_(주)엘지생활건강‘
table_39 <- error_check_fun(39, TRUE)   # '39_(주)엘지유플러스‘
table_40 <- error_check_fun(40, TRUE)   # '40_(주)엘지하우시스‘
table_41 <- error_check_fun(41, TRUE)   # '41_(주)엘지화학‘
table_42 <- error_check_fun(42, TRUE)   # '42_(주)영원무역‘
table_43 <- error_check_fun(43, TRUE)   # '43_(주)영풍‘
table_44 <- error_check_fun(44, TRUE)   # '44_(주)오뚜기‘
table_45 <- error_check_fun(45, TRUE)   # '45_(주)오리온‘
table_46 <- error_check_fun(46, TRUE)    #'46_(주)오리온홀딩스'
table_47 <- error_check_fun(47, TRUE)   # '47_(주)우리은행'
table_48 <- error_check_fun(48, TRUE)   # '48_(주)유니드‘
table_49 <- error_check_fun(49, TRUE)   # '49_(주)유한양행‘
table_50 <- error_check_fun(50, TRUE)   # '50_(주)이노션‘

table_51 <- error_check_fun(51, TRUE)   # '51_(주)이마트‘
table_52 <- error_check_fun(52, TRUE)   # '52_(주)제일기획‘
table_53 <- error_check_fun(53, TRUE)   # '53_(주)종근당‘
table_54 <- error_check_fun(54, TRUE)   # '54_(주)지에스‘
table_55 <- error_check_fun(55, TRUE)   # '55_(주)지에스리테일‘
table_56 <- error_check_fun(56, TRUE)   # '56_(주)카카오‘
table_57 <- error_check_fun(57, TRUE)   # '57_(주)케이씨씨‘
table_58 <- error_check_fun(58, TRUE)   # '58_(주)케이티‘
table_59 <- error_check_fun(59, TRUE)   # '59_(주)케이티앤지‘
table_60 <- error_check_fun(60, TRUE)   # '60_(주)팜스코‘
table_61 <- error_check_fun(61, TRUE)   # '61_(주)포스코'
table_62 <- error_check_fun(62, TRUE)   # '62_(주)포스코대우‘
table_63 <- error_check_fun(63, TRUE)   # '63_(주)풍산‘ -
table_64 <- error_check_fun(64, TRUE)   # '64_(주)하나금융지주‘
table_65 <- error_check_fun(65, TRUE)   # '65_(주)한라홀딩스‘
table_66 <- error_check_fun(66, TRUE)   # '66_(주)한샘‘
table_67 <- error_check_fun(67, TRUE)   # '67_(주)한섬‘
table_68 <- error_check_fun(68, TRUE)   # '68_(주)한솔케미칼‘ 
table_69 <- error_check_fun(69, TRUE)   # '69_(주)한진중공업‘ 
table_70 <- error_check_fun(70, TRUE)   # '70_(주)한화‘ 
table_71 <- error_check_fun(71, TRUE)   # '71_(주)현대그린푸드
table_72 <- error_check_fun(72, TRUE)   # '72_(주)현대리바트‘
table_73 <- error_check_fun(73, TRUE)   # '73_(주)현대미포조선‘ 
table_74 <- error_check_fun(74, TRUE)   # '74_(주)현대백화점‘  
table_75 <- error_check_fun(75, TRUE)   # '75_(주)현대홈쇼핑‘ 
table_76 <- error_check_fun(76, TRUE)   # '76_(주)호텔신라‘
table_77 <- error_check_fun(77, TRUE)   # '77_(주)효성‘
table_78 <- error_check_fun(78, TRUE)   # '78_(주)후성‘
table_79 <- error_check_fun(79, TRUE)   # '79_DB손해보험(주)‘
table_80 <- error_check_fun(80, TRUE)   # '80_LG이노텍(주)‘
table_81 <- error_check_fun(81, TRUE)   # '81_LS산전(주)‘
table_82 <- error_check_fun(82, TRUE)   # '82_OCI(주)‘
table_83 <- error_check_fun(83, TRUE)   # '83_S&T모티브(주)‘
table_84 <- error_check_fun(84, TRUE)   # '84_SK이노베이션(주)‘ 
table_85 <- error_check_fun(85, TRUE)   # '85_고려아연(주)‘
table_86 <- error_check_fun(86, TRUE)   # '86_고려제강(주)‘

table_88 <- error_check_fun(88, TRUE)   # '88_금호석유화학(주)‘ 
table_89 <- error_check_fun(89, TRUE)   # '89_금호타이어(주)‘ 
table_90 <- error_check_fun(90, TRUE)   # '90_기아자동차(주)‘
table_91 <- error_check_fun(91, TRUE)   # '91_남양유업(주)‘
table_92 <- error_check_fun(92, TRUE)   # '92_남해화학(주)‘
table_93 <- error_check_fun(93, TRUE)   # '93_네이버(주)‘
table_94 <- error_check_fun(94, TRUE)   # '94_넥센타이어(주)‘
table_95 <- error_check_fun(95, TRUE)   # '95_넷마블(주)‘
table_96 <- error_check_fun(96, TRUE)   # '96_대덕전자(주)'
table_97 <- error_check_fun(97, TRUE)   # '97_대림산업(주)‘ 
table_98 <- error_check_fun(98, TRUE)   # '98_대상(주)‘ 
table_99 <- error_check_fun(99, TRUE)   # '99_대우조선해양(주)‘
table_100 <- error_check_fun(100, TRUE)   ### '100_대한유화(주)‘

table_101 <- error_check_fun(101, TRUE)   # '101_동국제강(주)‘
table_102 <- error_check_fun(102, TRUE)   # '102_동아쏘시오홀딩스(주)‘
table_103 <- error_check_fun(103, TRUE)   # '103_동아에스티(주)‘
table_104 <- error_check_fun(104, TRUE)   # '104_동원시스템즈(주)‘
table_105 <- error_check_fun(105, TRUE)   # '105_두산밥캣(주)‘
table_106 <- error_check_fun(106, TRUE)   # '106_두산인프라코어(주)‘
table_107 <- error_check_fun(107, TRUE)   # '107_두산중공업(주)‘
table_108 <- error_check_fun(108, TRUE)   # '108_롯데쇼핑(주)‘
table_109 <- error_check_fun(109, TRUE)   # '109_롯데정밀화학(주)‘
table_110 <- error_check_fun(110, TRUE)   # '110_롯데지주(주)‘
table_111 <- error_check_fun(111, TRUE)   # '111_롯데칠성음료(주)‘
table_112 <- error_check_fun(112, TRUE)   # '112_롯데케미칼(주)‘
table_113 <- error_check_fun(113, TRUE)   # '113_롯데푸드(주)‘
table_114 <- error_check_fun(114, TRUE)   # '114_롯데하이마트(주)‘
table_115 <- error_check_fun(115, TRUE)   # '115_메리츠종합금융증권(주)‘
table_116 <- error_check_fun(116, TRUE)   # '116_미래에셋대우(주)‘
table_117 <- error_check_fun(117, TRUE)   # '117_보령제약(주)‘
table_118 <- error_check_fun(118, TRUE)   # '118_부광약품(주)‘
table_119 <- error_check_fun(119, TRUE)   # '119_삼성SDI(주)‘
table_120 <- error_check_fun(120, TRUE)   # '120_삼성물산(주)‘
table_121 <- error_check_fun(121, TRUE)   # '121_삼성바이오로직스(주)‘
table_122 <- error_check_fun(122, TRUE)   # '122_삼성생명보험(주)‘
table_123 <- error_check_fun(123, TRUE)   # '123_삼성에스디에스(주)‘
table_124 <- error_check_fun(124, TRUE)   # '124_삼성엔지니어링(주)‘
table_125 <- error_check_fun(125, TRUE)   # '125_삼성전기(주)‘
table_126 <- error_check_fun(126, TRUE)   # '126_삼성전자(주)‘
table_127 <- error_check_fun(127, TRUE)   # '127_삼성중공업(주)‘
table_128 <- error_check_fun(128, TRUE)   # '128_삼성증권(주)‘
table_129 <- error_check_fun(129, TRUE)   # '129_삼성카드(주)‘
table_130 <- error_check_fun(130, TRUE)   # '130_삼성화재해상보험(주)‘
table_131 <- error_check_fun(131, TRUE)   # '131_세방전지(주)‘
table_132 <- error_check_fun(132, TRUE)   # '132_쌍용양회공업(주)‘
table_133 <- error_check_fun(133, TRUE)   # '133_쌍용자동차(주)‘
table_134 <- error_check_fun(134, TRUE)   # '134_씨제이(주)‘
table_135 <- error_check_fun(135, TRUE)   # '135_씨제이대한통운(주)‘
table_136 <- error_check_fun(136, TRUE)   # '136_씨제이씨지브이(주)‘
table_137 <- error_check_fun(137, TRUE)   # '137_씨제이제일제당(주)‘
table_138 <- error_check_fun(138, TRUE)   # '138_아이에스동서(주)‘
table_139 <- error_check_fun(139, TRUE)   # '139_에스엘(주)‘
table_140 <- error_check_fun(140, TRUE)   # '140_에스케이(주)‘
table_141 <- error_check_fun(141, TRUE)   # '141_에스케이네트웍스(주)‘
table_142 <- error_check_fun(142, TRUE)   # '142_에스케이디스커버리(주)‘
table_143 <- error_check_fun(143, TRUE)   # '143_에스케이씨(주)‘
table_144 <- error_check_fun(144, TRUE)   # '144_에스케이케미칼(주)‘
table_145 <- error_check_fun(145, TRUE)   # '145_에스케이텔레콤(주)‘
table_146 <- error_check_fun(146, TRUE)   # '146_에스케이하이닉스(주)‘
table_147 <- error_check_fun(147, TRUE)   # '147_에쓰-오일(주)‘
table_148 <- error_check_fun(148, TRUE)   # '148_에이치디씨(주)‘
table_149 <- error_check_fun(149, TRUE)   # '149_에이치디씨현대산업개발(주)‘
table_150 <- error_check_fun(150, TRUE)   # '150_에이케이홀딩스(주)‘

table_151 <- error_check_fun(151, TRUE)   # '151_엔에이치투자증권(주)‘
table_152 <- error_check_fun(152, TRUE)   # '152_엘아이지넥스원(주)‘
table_153 <- error_check_fun(153, TRUE)   # '153_엘지디스플레이(주)‘
table_154 <- error_check_fun(154, TRUE)   # '154_엘지전자(주)‘
table_155 <- error_check_fun(155, TRUE)   # '155_영진약품(주)‘
table_156 <- error_check_fun(156, TRUE)   # '156_일양약품(주)‘
table_157 <- error_check_fun(157, TRUE)   # '157_일진머티리얼즈(주)‘
table_158 <- error_check_fun(158, TRUE)   # '158_제이더블유중외제약(주)‘
table_159 <- error_check_fun(159, TRUE)   # '159_제이더블유홀딩스(주)‘
table_160 <- error_check_fun(160, TRUE)   # '160_중소기업은행‘
table_161 <- error_check_fun(161, TRUE)   # '161_지에스건설(주)‘
table_162 <- error_check_fun(162, TRUE)   # '162_코스맥스(주)‘
table_163 <- error_check_fun(163, TRUE)   # '163_코오롱인더스트리(주)‘
table_164 <- error_check_fun(164, TRUE)   # '164_코웨이(주)‘
table_165 <- error_check_fun(165, TRUE)   # '165_쿠쿠홀딩스(주)‘
table_166 <- error_check_fun(166, TRUE)   # '166_태광산업(주)‘
table_167 <- error_check_fun(167, TRUE)   # '167_팬오션(주)‘
table_168 <- error_check_fun(168, TRUE)   # '168_하이트진로(주)‘
table_169 <- error_check_fun(170, TRUE)   # '170_한국단자공업(주)‘
table_170 <- error_check_fun(171, TRUE)   # '171_한국쉘석유(주)‘
table_171 <- error_check_fun(169, TRUE)   # '169_한국가스공사(주)‘
table_172 <- error_check_fun(172, TRUE)   # '172_한국전력공사(주)‘
table_173 <- error_check_fun(173, TRUE)   # '173_한국전력기술(주)‘
table_174 <- error_check_fun(174, TRUE)   # '174_한국콜마(주)‘
table_175 <- error_check_fun(175, TRUE)   # '175_한국타이어(주)‘
table_176 <- error_check_fun(176, TRUE)   # '176_한국타이어월드와이드(주)‘
table_177 <- error_check_fun(177, TRUE)   # '177_한국투자금융지주(주)‘
table_178 <- error_check_fun(178, TRUE)   # '178_한국항공우주산업(주)‘
table_179 <- error_check_fun(179, TRUE)   # '179_한미사이언스(주)‘
table_180 <- error_check_fun(180, TRUE)   # '180_한미약품(주)‘
table_181 <- error_check_fun(181, TRUE)   # '181_한세실업(주)‘
table_182 <- error_check_fun(182, TRUE)   # '182_한온시스템(주)‘
table_183 <- error_check_fun(183, TRUE)   # '183_한올바이오파마(주)‘
table_184 <- error_check_fun(184, TRUE)   # '184_한일홀딩스(주)‘
table_185 <- error_check_fun(185, TRUE)   # '185_한전케이피에스(주)‘
table_186 <- error_check_fun(186, TRUE)   # '186_한화생명보험(주)‘
table_187 <- error_check_fun(187, TRUE)   # '187_한화에어로스페이스(주)‘
table_188 <- error_check_fun(188, TRUE)   # '188_한화케미칼(주)‘
table_189 <- error_check_fun(189, TRUE)   # '189_현대건설(주)‘
table_190 <- error_check_fun(190, TRUE)   # '190_현대글로비스(주)‘
table_191 <- error_check_fun(191, TRUE)   # '191_현대로템(주)‘
table_192 <- error_check_fun(192, TRUE)   # '192_현대모비스(주)‘
table_193 <- error_check_fun(193, TRUE)   # '193_현대엘리베이터(주)‘
table_194 <- error_check_fun(194, TRUE)   # '194_현대위아(주)‘
table_195 <- error_check_fun(195, TRUE)   # '195_현대자동차(주)‘
table_196 <- error_check_fun(196, TRUE)   # '196_현대제철(주)‘
table_197 <- error_check_fun(197, TRUE)   # '197_현대중공업(주)‘
table_198 <- error_check_fun(198, TRUE)   # '198_현대중공업지주(주)‘
table_199 <- error_check_fun(199, TRUE)   # '199_현대해상화재보험(주)‘
table_200 <- error_check_fun(200, TRUE)   # '200_휴성중공업(주)‘
table_201 <- error_check_fun(201, TRUE)   # '201_휴켐스(주)‘



############################## 해결하기 어려운 케이스 ##################################

# ### '87_그랜드코리아레저(주)‘ -> 전기에 대한 데이터 없음!!!! 큰문제
# error_check_fun(87)
# grandkorea_table <- error_check_fun(87, TRUE)


############################## step 4. 불러들인 자료 엑셀로 저장하기 ##################################
n_list <- list(ls(pattern = "table"))
n <- length(n_list[[1]])
# 결과저장
write.xlsx(get(n_list[[1]][161])[1], file = "15. example_1115(200)_5.xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:n)
for (j in 162:200){
  write.xlsx(get(n_list[[1]][j])[1], file = "15. example_1115(200)_5.xlsx", sheetName = name_tmp[j-1], append=TRUE)
}
# 결과 except 저장
except_list_tf <- rep(FALSE, n)
for(i in 1:n){
  if(length(get(n_list[[1]][i])[[2]]) != 0){
    except_list_tf[i] <- TRUE}
}
list_number <- which(except_list_tf)
write.xlsx(get(n_list[[1]][list_number[1]])[[2]], file = "15. example_1115(200)_except_1.xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:length(list_number))
for (j in 2:length(list_number)){
  write.xlsx(get(n_list[[1]][list_number[j]])[[2]], file = "15. example_1115(200)_except_1.xlsx", sheetName = name_tmp[j-1], append=TRUE)
}

############################## step 5. column name 정제 & 리스트 저장 ##################################
n_list <- list(ls(pattern = "table"))
col_namelist <- c()
for(i in 1:length(n_list[[1]])){
  col_name_table <- colnames(get(n_list[[1]][i])[[1]])
  col_namelist <- c(col_namelist, col_name_table)
}
#　최초 칼럼
length(col_namelist)

#　1차정제 칼럼
col_namelist <- gsub("and","@", gsub("[0-9]", "", gsub("\\ㆍ", "", gsub("[[:punct:]]", "", 
                                                                       gsub("(주,)", "", gsub("@", "and", col_namelist))))))

col_namelist[which(str_extract(col_namelist, "당기") == "당기")]
col_namelist <- gsub("당기말","", gsub("당기년", "", gsub("제당기", "", gsub("당기~당기", "", col_namelist))))

col_namelist[which(str_extract(col_namelist, "전기") == "전기")]
col_namelist <- gsub("전기말","", gsub("전기년", "", gsub("제전기", "", gsub("전기~전기", "", col_namelist))))

col_namelist[which(str_extract(col_namelist, "등") == "등")]
before_w <- c("매출등", "매입등", "채무등", "채권등", "수익등", "비용등", "매출거래등", "매입거래등")
after_w <- c("매출", "매입", "채무", "채권", "수익", "비용", "매출거래", "매입거래")
for (i in 1:length(before_w)){
  col_namelist <- gsub(before_w[i], after_w[i], col_namelist)  
}

length(col_namelist)

#　2차정제 칼럼
delete_list1 <- c("구분", "회사명", "당기전기", "대상회사명")
for(ii in 1:length(delete_list1)){
  col_namelist <- col_namelist[-which(col_namelist == delete_list1[ii])]
}
length(col_namelist)

#　3차정제 칼럼
delete_list2<- c("합계", "소계", "@계@")
col_namelist <- col_namelist[!str_detect(col_namelist, paste(delete_list2, collapse ="|"))]
col_namelist <- gsub("NA", "", gsub("@$", "", gsub("^@", "", gsub("@@","@", col_namelist))))
col_namelist <- unique(col_namelist)
length(col_namelist)

#　정제 칼럼 내보내기
col_namelist_update1 <- matrix(col_namelist, ncol=6)
write.csv(col_namelist_update1, file="15. col_namelist.csv")

