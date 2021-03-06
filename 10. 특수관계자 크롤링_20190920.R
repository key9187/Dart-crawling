######################################## 0. 환경설정 #######################################
if(!require(rvest)){install.packages('rvest')}; library(rvest)
if(!require(stringr)){install.packages('stringr')}; library(stringr)
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(reshape2)){install.packages('reshape2')}; library(reshape2)
if(!require(zoo)){install.packages('zoo')}; library(zoo)
if(!require(xlsx)){install.package('xlsx')}; library(xlsx)
if(!require(data.table)){install.package('data.table')}; library(data.table)

kospi200 <- read.csv(file='KOSPI200_web.csv')

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
  end <- ifelse(end==0, length(str_split(document, '<table')[[1]]) - 1, end)
  
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

### (사전정의1) 각 테이블 필요성 결정해주는 함수 정의
list_TF_fun <- function(list_data, case_TF=F){
  # 제외 단어: 장부가액, 설정최조액, 담보제공처, 지급처, 차입, 종업원대여금, 기초, 부채, 미지급금, 배당, 매출, 채권, 매입, 수익, 
  #            단기대여금, 자금대여, 현금출자, 자산유동화차입금
  # 꼭 포함되어야 할 단어: 매출 등, 제상품매출,
  tmp_list_t <- c("지급수수료", "비용", "예치금","기타채권", "매출채권", "제상품매출", "상품매출",
                  "미사용약정", "기타자산", "손실충당금", "대출채권", "매입채무", "매출등", "매입등",
                  "채권등", "채무등", "재고매입", "광고비", "일반관리비", "기타영업손익", "기타자산", "유형자산", 
                  "재화의매입", "재화의판매", "사업수익", "지분매각", "지분매입", "매출거래", "매입거래", 
                  "유무형자산취득", "배당금수령액", "고정자산매입", "기타일반관리비", "기타수익")
  tmp_list_f <- c("급여", "주석", "매도가능금융자산의평가")
  
  list_TF <- c()
  for(i in 1:length(list_data)){
    if(length(list_data[[i]]) <= 2){list_TF[i] = FALSE} else {list_TF[i] = TRUE}
    if(!any(str_detect(colnames(list_data[[i]]), "단위"))){list_data[[i]] <- rbind(colnames(list_data[[i]]), list_data[[i]])}
    list_data[[i]] <- as.data.frame(lapply(list_data[[i]], function(x) text_clean(gsub("\\s", "", 
                                              gsub("[0-9]","",gsub("\\.","",x))))), stringsAsFactors = F)
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
  
  list_class <- ifelse(list_case_vec %in% c("case1", "case1_3"), list_class, NA)
  tmp_class <- grep("당기/전기", list_class)
  
  if(length(tmp_class) %% 2 == 1){print("error:: class error in list_class_fun")} else{
    for(idx in 1:length(tmp_class)){
      list_class[tmp_class[idx]] <- ifelse(idx %% 2 == 1, "당기", "전기")
    }
  }
  
  return(list_class)
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
  col_name_new <- gsub(" ", "", col_name)
  
  if(!(all(str_detect(paste(col_name_new, collapse = ""), c("당기", "전기")))
       | all(str_detect(paste(col_name_new, collapse = " "), c("\\(당\\)기", "\\(전\\)기")))
       | all(str_detect(paste(col_name_new, collapse = ""), c("2018", "2017"))) 
       | all(str_detect(paste(col_name_new, collapse = ""), c("당.기", "전.기"))) )){tmp_case <- "case1"
  } else if(any(str_detect(col_name_new, paste(tmp_list_t, collapse = '|'))) == TRUE){tmp_case <- "case2"
  } else if(any(str_detect(tmp_table[,(start_c-2):(start_c-1)], paste(tmp_list_t, collapse = '|'))) == TRUE) {tmp_case <- "case3"
  } else {print("error:: no case in list_case_fun function!")}
  
  # case1 내에 case3인 경우 case1_3으로 재검사
  if(tmp_case == "case1"){
    if(any(str_detect(tmp_table[,(start_c-1)], paste(tmp_list_t, collapse = '|'))) == TRUE) {tmp_case <- "case1_3"}
  }
  
  return(tmp_case)
}

### (사전정의6) col_name 정제하는 함수
colname_clean <- function(tmp_table, tmp_r){
  #col_name에 해당하는 table 생성하고 text_clean 수행
  tmp_colname_table <- tmp_table[1:(tmp_r-1),]
  ### tmp_colname_table '.', 숫자 없애기
  tmp_colname_table <- as.data.frame(lapply(tmp_colname_table, function(x) gsub("\\.","",x)), stringsAsFactors = F)
  # 각 table 행마다 ("구분", "회사명", "기업")을 제외한 table 재생성
  tmp_colname_list <- list()
  for(j in 1:nrow(tmp_colname_table)){
    tmp_j <- as.character(tmp_colname_table[j,])
    tmp_colname_list[[j]] <- tmp_j[!(str_detect(tmp_j, paste(c("구분", "회사명", "기업", "특수관계자", "계정과목명",
                                                               "회사의명칭", "값없음", "계정과목", "알수없음", "회사"),
                                                             collapse = '|')) %in% c(TRUE, NA))]
  }
  tmp_colname_table <- as.data.frame(tmp_colname_list, stringsAsFactors = F)   # 여기서 오류나면 list 길이가 맞지 않는 것!
  # 최종적으로 tmp_colname 생성
  tmp_colname <- c()
  for(jj in 1:length(tmp_colname_table)){
    tmp_colname <- paste0(tmp_colname, tmp_colname_table[,jj], "@")
  }
  return(tmp_colname)
}

### 종속기업 구분이 다른 테이블에 있는 경우 contents를 출력해주는 함수
# contents_name <- function(list_data){
#   number_contents <- if(ncol(list_data[[1]]) == 2){1
#   } else if(ncol(list_data[[2]]) == 2){2
#   } else if(ncol(list_data[[3]]) == 2){3
#   } else{ print("error:: number_contents undefined in contents_name!")
#   }
#   target_tmp <- list_data[[number_contents]]
#   contents_result <- data.frame()
#   for(i in 1:nrow(target_tmp)){
#     tmp_result <- cbind(target_tmp[i,1], as.data.frame(str_split(gsub("\\s", "", 
#                   gsub("\\s등", "", target_tmp[i,2])), ","), stringsAsFactors = FALSE))
#     colnames(tmp_result) <- c("구분", "기업명")
#     contents_result <- rbind(contents_result, tmp_result)
#   }
#   return(contents_result)
# }
 

############################## step 2. 최종함수 dart_get 함수 정의 ##################################
path <- as.character(kospi200$링크[2]); index = 2
# dart_get(as.character(kospi200$링크[3]), 3)
# 확인: 
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
      tmp <- tmp[,!str_detect(colnames(tmp), "거래내용")]
      ### 첫열에 계, 합계 텍스트 있는 행 제거 
      tmp <- tmp[!ifelse(tmp[,1] %in% c("계", "합계", "소계", "채권", "채무", "채무합계", "채권합계", "수익", "비용",
                                        "매출등합계", "매입등합계"), TRUE, FALSE),]
      
      ### colnames가 단위에 대한 내용이 아니라면 첫행으로 추가하기
      if(!any(str_detect(colnames(tmp), "단위"))){
        tmp <- rbind(colnames(tmp), tmp)}
      
      ### 구분에 대한 첫번째 케이스 - 구분 항목이 행으로 들어가 있는 경우 NA 없애기
      start_c_tmp <- list_start_fun(tmp)[2]
      tmp_col1 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,1]))))
      tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2]))))
      tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3]))))
      sub_name_tmp <- c("종속기업", "관계기업", "지배기업",
                        "기타특수관계자", "공동기업", "관계기업및공동기업","대규모기업집단계열회사","대규모기업집단계열회사등",
                        "당사에유의적영향력을행사하는기업", "유의적인영향력을행사할수있는투자자","대기업기업집단계열회사등",
                        "당해기업에중대한영향력을미치는회사", "당사에중요한영향력을행사하는회사", "당사의종속기업", 
                        "유의적영향력을행사하는회사", "기타의특수관계자")
      
      if( ((start_c_tmp == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
          ((start_c_tmp == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
          ((start_c_tmp == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
        narow_which <- which(tmp_col1 %in% sub_name_tmp)
        for(ii in 1:length(narow_which)){
          tmp[narow_which[ii],is.na(tmp[narow_which[ii],])] <- "값없음"
        }
      }
      
      ### 정리된 테이블에 밀림현상있는지 체크하고 수정(순서바꾸지말것!!!)
      start_r_tmp <- list_start_fun(tmp)[1]
      if(any(is.na(tmp[1:(ifelse(nrow(tmp)<5, nrow(tmp),5)),ncol(tmp)])) == TRUE){
        print(paste("error:: 표 밀린 상태! 확인필요함!", i,"에서 문제"))
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
            standard_na <- which(str_detect(tmp[c(1:3,number_na[ind]),], paste(c("구분", "회사명","회사의명칭","기업명",
                                                                                 "값없음","알수없음"), collapse = '|')) == FALSE)[1] - 1
          }
          tmp[number_na[ind],(standard_na+1):ncol(tmp)] <- tmp[number_na[ind],standard_na:(ncol(tmp)-rowsum_na)]
          tmp[number_na[ind],standard_na:(standard_na+rowsum_na-1)] <- "값없음"
        }
        if(any(is.na(tmp[1:(ifelse(nrow(tmp)<4, nrow(tmp),4)),])) == FALSE){
          print(paste(i,"에서 밀림 현상 해결 완료!"))
        }
      } 
      
      # 첫번째 케이스 : 구분 항목이 행으로 들어가 있는 경우 행렬변환
      start_c_tmp <- list_start_fun(tmp)[2]
      tmp_col1 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,1]))))
      tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2]))))
      tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3]))))
      
      if( ((start_c_tmp == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
          ((start_c_tmp == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
          ((start_c_tmp == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
        insert_col <- na.locf(ifelse(str_detect(tmp_col1, paste(sub_name_tmp, collapse = '|')) == TRUE, tmp_col1, NA),
                              na.rm=FALSE, fromLast = FALSE)
        insert_col[is.na(insert_col)] <- "값없음"
        tmp <- cbind(insert_col, tmp)
        tmp <- tmp[!(tmp_col1 %in% sub_name_tmp),]
        colnames(tmp)[1:2] <- c("구분", "회사명")
        tmp[,1] <- as.character(tmp[,1])
      }
      
      # 세번째 케이스 : 구분이 따로 없고 앞선테이블에서 정의되지 않은 케이스 - '알수없음'처리
      if((start_c_tmp == 2) & (!any(sub_name_tmp %in% tmp_col1))|
         ((start_c_tmp == 3) & any(list_TF_fun(list_df,T) %in% tmp_col2) & (!any(sub_name_tmp %in% tmp_col2))) ){
        tmp$구분 <- "알수없음"
        tmp <- tmp[c(ncol(tmp), 1:(ncol(tmp)-1))]
        colnames(tmp)[1:2] <- c("구분", "회사명")
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
    print("error:: wrong case number in list_case_fun function!")
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
        tmp <- tmp[,!str_detect(colnames(tmp), "거래내용")]
        ### 첫열에 계, 합계 텍스트 있는 행 제거 
        tmp <- tmp[!ifelse(tmp[,1] %in% c("계", "합계", "소계", "채권", "채무", "채무합계", "채권합계", "수익", "비용",
                                          "매출등합계", "매입등합계"), TRUE, FALSE),]
        
        ### colnames가 단위에 대한 내용이 아니라면 첫행으로 추가하기
        if(!any(str_detect(colnames(tmp), "단위"))){
          tmp <- rbind(colnames(tmp), tmp)}
        
        ### 구분에 대한 첫번째 케이스 - 구분 항목이 행으로 들어가 있는 경우 NA 없애기
        start_c <- list_start_fun(tmp)[2]
        
        tmp_col1 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,1]))))
        tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2]))))
        tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3]))))
        sub_name_tmp <- c("종속기업", "관계기업", "지배기업",
                          "기타특수관계자", "공동기업", "관계기업및공동기업","대규모기업집단계열회사","대규모기업집단계열회사등",
                          "당사에유의적영향력을행사하는기업", "유의적인영향력을행사할수있는투자자","대기업기업집단계열회사등",
                          "당해기업에중대한영향력을미치는회사", "당사에중요한영향력을행사하는회사", "당사의종속기업", 
                          "유의적영향력을행사하는회사", "기타의특수관계자")
        
        if( ((start_c == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
            ((start_c == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
            ((start_c == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
          narow_which <- which(tmp_col1 %in% sub_name_tmp)
          for(ii in 1:length(narow_which)){
            tmp[narow_which[ii],is.na(tmp[narow_which[ii],])] <- "값없음"
          }
        }
        
        ### 정리된 테이블에 밀림현상있는지 체크하고 수정(순서바꾸지말것!!!)
        start_r <- list_start_fun(tmp)[1]
        if(any(is.na(tmp[1:(ifelse(nrow(tmp)<5, nrow(tmp),5)),ncol(tmp)])) == TRUE){
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
              standard_na <- which(str_detect(tmp[c(1:3,number_na[ind]),], paste(c("구분", "회사명","회사의명칭","기업명",
                                  "값없음","알수없음"), collapse = '|')) == FALSE)[1] - 1
              }
            tmp[number_na[ind],(standard_na+1):ncol(tmp)] <- tmp[number_na[ind],standard_na:(ncol(tmp)-rowsum_na)]
            tmp[number_na[ind],standard_na:(standard_na+rowsum_na-1)] <- "값없음"
          }
        }
        
        # 첫번째 케이스 : 구분 항목이 행으로 들어가 있는 경우 행렬변환
        start_c <- list_start_fun(tmp)[2]
        tmp_col1 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,1]))))
        tmp_col2 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,2]))))
        tmp_col3 <- gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", gsub("[0-9]","", tmp[,3]))))
        
        if( ((start_c == 2) & (any(sub_name_tmp %in% tmp_col1))) | 
            ((start_c == 3) & (any(list_TF_fun(list_df,T) %in% tmp_col2)) & (any(sub_name_tmp %in% tmp_col2))) | 
            ((start_c == 4) & (any(list_TF_fun(list_df,T) %in% tmp_col3)) & (any(sub_name_tmp %in% tmp_col3))) ){
          insert_col <- na.locf(ifelse(str_detect(tmp_col1, paste(sub_name_tmp, collapse = '|')) == TRUE, tmp_col1, NA),
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
          tmp$구분 <- "알수없음"
          tmp <- tmp[c(ncol(tmp), 1:(ncol(tmp)-1))]
          colnames(tmp)[1:2] <- c("구분", "회사명")
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
            tmp <- tmp[start_r_after:nrow(tmp), start_c:ncol(tmp)]
            tmp <- as.data.frame(lapply(tmp, function(x) gsub("\\)","",gsub("\\(","-",x)) ), stringsAsFactors = F)
            # 숫자만 있는 테이블로 만들기
            unit <- list_unit_fun(list_df, i, get_startend(path, 2))
            tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(gsub(",", "", gsub("\\s", "", x))) * unit))
            rownames(tmp) <- row_name; colnames(tmp) <- col_name[start_c:length(col_name)]
            # category1를 다시 만들어서 table 재정의_col
            category1 <- t(as.data.frame(str_split(row_name, "@")))
            tmp$구분 <- category1[,1]; tmp$회사명 <- category1[,2]
            tmp$당기_전기 <- list_class[i]
            tmp <- tmp[, c((ncol(tmp)-2):ncol(tmp), 1:(ncol(tmp)-3))]
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
            tmp[,4] <- as.numeric(lapply(tmp[,4], function(x) gsub("\\)","",gsub("\\(","-",gsub(",","",gsub("-","0",x)))) )) * unit
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
          tmp <- dcast(tmp, 구분 + 회사명 + 당기_전기 ~ col_var)
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
          tmp <- tmp[start_r_after:nrow(tmp),]
          colnames(tmp)[1:3] <- c("구분", "회사명", "계정과목")
          tmp <- melt(tmp, id=1:3)
          # 숫자만 있는 value ()는 음수처리
          unit <- list_unit_fun(list_df, i, get_startend(path, 2))
          tmp$value <- as.numeric(lapply(tmp$value, function(x) gsub("\\)","",gsub("\\(","-",gsub(",","",gsub("-","0",x)))) )) * unit
          tmp <- dcast(tmp, 구분 + 회사명 + variable ~ 계정과목, sum)
          colnames(tmp)[3] <- "당기_전기"
          # category를 다시 만들어서 table 재정의_row
          tmp <- tmp[!str_detect(tmp[,2], paste(c("소계", "합계", "기타"),collapse = '|')),]
          rownames(tmp) <- NULL
        } else {print(paste("error:: no case in dart_get case3!", i, "에서"))}
        
        ### table 합치기
        final_table <- merge(final_table, tmp, all = TRUE)
        if(nrow(final_table)==0){
          final_table <- merge(final_table, tmp, all = TRUE)}
        }
  }
  ### final table 수정
  final_table <- aggregate(final_table[,4:ncol(final_table)], by=list(final_table$구분, final_table$회사명, final_table$당기_전기), sum, na.rm=TRUE, na.action=NULL)
  final_table <- cbind(as.character(kospi200[index,1]), final_table)
  colnames(final_table)[1:4] <- c("대상회사명", "구분", "회사명", "당기_전기")
  return(final_table)
}


############################## step 3. error_check function ##################################
error_check_fun <- function(t_ind, output_TF = FALSE){
  result_path <- as.character(kospi200$링크[t_ind])
  result_table <- dart_get(result_path, t_ind)
  check_list <- list(kospi200[t_ind,], dim(result_table))
  if(output_TF == TRUE){
    return(result_table)
  } else {
    return(check_list)
  }
}

############################## step 3-1. error_check 완료 ##################################

### '1_(주)BNK금융지주' -> 확인완료(16,22)
error_check_fun(1)
BNK_table <- error_check_fun(1, TRUE)

### '2_(주)DB하이텍' -> 확인완료(16,8)
error_check_fun(2)
dbhitech_table <- error_check_fun(2, TRUE)

### '3_(주)KB금융지주'-> 확인완료(24,19)
error_check_fun(3)
kbfinan_table <- error_check_fun(3, TRUE)

### '4_(주)강원랜드'-> 확인완료(6,6)
error_check_fun(4)
gangwon_table <- error_check_fun(4, TRUE)

### '5_(주)녹십자' -> 확인완료(19,22)
error_check_fun(5)
green_table <- error_check_fun(5, TRUE)

### '6_(주)녹십자홀딩스' -> 확인완료(37,13)
error_check_fun(6)
greenhol_table <- error_check_fun(6, TRUE)

### '7_(주)농심' -> 확인완료(26,12)
error_check_fun(7)
nongsim_table <- error_check_fun(7, TRUE)

### '8_(주)대교' -> 확인완료(60,15)
error_check_fun(8)
daekyo_table <- error_check_fun(8, TRUE)

### '9_(주)대우건설' -> 확인완료(89,20)
error_check_fun(9)
daewoo_table <- error_check_fun(9, TRUE)

### '10_(주)대웅제약' -> 확인완료(23,19)
error_check_fun(10)
daewoong_table <- error_check_fun(10, TRUE)

### '11_(주)대한항공' -> 확인완료(22,8)
error_check_fun(11)
daehan_table <- error_check_fun(11, TRUE)

### '12_(주)동서' -> 확인완료(14,13)
error_check_fun(12)
dongseo_table <- error_check_fun(12, TRUE)

### '13_(주)동양'-> 확인완료(28,18)
error_check_fun(13)
dongyang_table <- error_check_fun(13, TRUE)

### '14_(주)동원에프앤비' -> 확인완료(18,15)
error_check_fun(14)
dongwon_table <- error_check_fun(14, TRUE)

### '15_(주)두산' -> 확인완료(16,14)
error_check_fun(15)
doosan_table <- error_check_fun(15, TRUE)

### '16_(주)락앤락'-> 확인완료(33,17)
error_check_fun(16)
rockn_table <- error_check_fun(16, TRUE)

## '17_(주)만도'-> 확인완료(47,23)
error_check_fun(17)
mando_table <- error_check_fun(17, TRUE)

### '18_(주)무학'-> 확인완료(24,13)
error_check_fun(18)
moohak_table <- error_check_fun(18, TRUE)

### '19_(주)비지에프' -> 확인완료(17,10)
error_check_fun(19)
bgf_table <- error_check_fun(19, TRUE)

### '20_(주)비지에프리테일'-> 확인완료(7,10)
error_check_fun(20)
bgfretail_table <- error_check_fun(20, TRUE)

### '21_(주)빙그레'-> 확인완료(10,11)
error_check_fun(21)
bing_table <- error_check_fun(21, TRUE)

### '22_(주)삼양사'-> 확인완료(15,15)
error_check_fun(22)
samyang_table <- error_check_fun(22, TRUE)

### '23_(주)삼양홀딩스'-> 확인완료(32,11)
error_check_fun(23)
samyanghol_table <- error_check_fun(23, TRUE)

### '24_(주)세아베스틸'-> 확인완료(46,24)
error_check_fun(24)
sae_table <- error_check_fun(24, TRUE)

### '25_(주)셀트리온‘ -> 확인완료(9,18)
error_check_fun(25)
celltrion_table <- error_check_fun(25, TRUE)

### '26_(주)신세계‘ -> 확인완료(40,16)
error_check_fun(26)
sinsegye_table <- error_check_fun(26, TRUE)

### '27_(주)신한금융지주회사‘ -> 확인완료(26,11)
error_check_fun(27)
sinhanfinan_table <- error_check_fun(27, TRUE)

### '28_(주)아모레퍼시픽‘ -> 확인완료(64,15)
error_check_fun(28)
amgroup_table <- error_check_fun(28, TRUE)

### '29_(주)아모레퍼시픽그룹‘ -> 확인완료(24,14)
error_check_fun(29)
amore_table <- error_check_fun(29, TRUE)

### '30_(주)에스비에스‘ -> 확인완료(24,23)
error_check_fun(30)
sbs_table <- error_check_fun(30, TRUE)

### '31_(주)에스원‘ -> 확인완료(15,14)
error_check_fun(31)
sone_table <- error_check_fun(31, TRUE)

### '32_(주)에스피씨삼립‘ -> 확인완료(84,14)
error_check_fun(32)
spc_table <- error_check_fun(32, TRUE)

### '33_(주)엔씨소프트‘ -> 확인완료(25,21)
error_check_fun(33)
ncsoft_table <- error_check_fun(33, TRUE)

### '34_(주)엘에스‘ -> 확인완료(58,8)
error_check_fun(34)
ls_table <- error_check_fun(34, TRUE)

### '35_(주)엘에프‘ -> 확인완료(47,18)
error_check_fun(35)
lf_table <- error_check_fun(35, TRUE)

### '36_(주)엘지‘ -> 확인완료(51,16)
error_check_fun(36)
lg_table <- error_check_fun(36, TRUE)

### '37_(주)엘지상사‘ -> 확인완료(81,11)
error_check_fun(37)
lgicorp_table <- error_check_fun(37, TRUE)

### '38_(주)엘지생활건강‘ -> 확인완료(97,12)
error_check_fun(38)
lglife_table <- error_check_fun(38, TRUE)

### '39_(주)엘지유플러스‘ -> 확인완료(76,9)
error_check_fun(39)
lguplus_table <- error_check_fun(39, TRUE)

### '40_(주)엘지하우시스‘ -> 확인완료(45,10)
error_check_fun(40)
lghousis_table <- error_check_fun(40, TRUE)

### '41_(주)엘지화학‘ -> 확인완료(52,28)
error_check_fun(41)
lgche_table <- error_check_fun(41, TRUE)

### '42_(주)영원무역‘ -> list_TF에 채권, 채무 필수여야해서 나오지 않음(15,7)
error_check_fun(42)
youngwon_table <- error_check_fun(42, TRUE)

### '43_(주)영풍‘ -> 확인완료(27,23)
error_check_fun(43)
yp_table <- error_check_fun(43, TRUE)

### '44_(주)오뚜기‘ -> 확인완료(44,13)
error_check_fun(44)
ottogi_table <- error_check_fun(44, TRUE)

### '45_(주)오리온‘ -> 확인완료(33,19)
error_check_fun(45)
orion_table <- error_check_fun(45, TRUE)

### '46_(주)오리온홀딩스‘ -> 확인완료(37,18)
error_check_fun(46)
orionhol_table <- error_check_fun(46, TRUE)

### '47_(주)우리은행‘ -> 확인완료(20,12)
error_check_fun(47)
woori_table <- error_check_fun(47, TRUE)

### '48_(주)유니드‘ -> 확인완료(19,13)
error_check_fun(48)
unid_table <- error_check_fun(48, TRUE)

### '49_(주)유한양행‘ -> 확인완료(33,19)
error_check_fun(49)
yoohan_table <- error_check_fun(49, TRUE)

### '51_(주)이마트‘ -> 확인완료(48,12)
error_check_fun(51)
emart_table <- error_check_fun(51, TRUE)

### '52_(주)제일기획‘ -> 확인완료(45,19)
error_check_fun(52)
cheil_table <- error_check_fun(52, TRUE)

### '53_(주)종근당‘ -> 확인완료(22,35)
error_check_fun(53)
ckd_table <- error_check_fun(53, TRUE)

### '54_(주)지에스‘ -> 확인완료(52,8)
error_check_fun(54)
gs_table <- error_check_fun(54, TRUE)

### '55_(주)지에스리테일‘ -> 확인완료(13,17)
error_check_fun(55)
gsretail_table <- error_check_fun(55, TRUE)

### '56_(주)카카오‘ -> 확인완료(122,13)
error_check_fun(56)
kakao_table <- error_check_fun(56, TRUE)

### '57_(주)케이씨씨‘ -> 확인완료(40,22)
error_check_fun(57)
kcc_table <- error_check_fun(57, TRUE)

### '58_(주)케이티‘ -> 확인완료(62,13)
error_check_fun(58)
kt_table <- error_check_fun(58, TRUE)

### '59_(주)케이티앤지‘ -> 확인완료(62,8)
error_check_fun(59)
ktng_table <- error_check_fun(59, TRUE)

### '61_(주)포스코' -> 확인완료(43,17)
error_check_fun(61)
posco_table <- error_check_fun(61, TRUE)

### '126_(주)삼성전자'-> 확인완료(42,10)
error_check_fun(126)
samsung_table <- error_check_fun(126, TRUE)


############################## step 4. 불러들인 자료 엑셀로 저장하기 ##################################
n_list <- list(ls(pattern = "table"))
n <- length(n_list[[1]])
write.xlsx(get(n_list[[1]][1]), file = "example_0917.xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:n)
for (j in 2:n){
  write.xlsx(get(n_list[[1]][j]), file = "example_0917.xlsx", sheetName = name_tmp[j-1], append=TRUE)
}

