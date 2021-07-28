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
  return(tmp_clean)
}

### (사전정의1) 각 테이블 필요성 결정해주는 함수 정의
list_TF_fun <- function(list_data){
  # 제외 단어: 장부가액, 설정최조액, 담보제공처, 지급처, 차입, 종업원대여금
  tmp_list_t <- c("매출", "채권", "매입", "지급수수료", "비용", "수익", "예치금", "부채", "배당", "자금대여",
                  "미지급금", "기초", "미사용", "자산유동화차입금")
  tmp_list_f <- c("급여", "주석")
  
  list_TF <- c()
  for(i in 1:length(list_data)){
    if(length(list_data[[i]]) <= 2){list_TF[i] = FALSE} else {list_TF[i] = TRUE}
  }
  
  list_TF <- list_TF & str_detect(list_data, paste(tmp_list_t, collapse = "|")) & 
    (!str_detect(list_data, paste(tmp_list_f, collapse = "|")))
  return(list_TF)
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
  
  list_class <- ifelse(list_case_vec == "case1", list_class, NA)
  tmp_class <- grep("당기/전기", list_class)
  
  if(length(tmp_class) %% 2 == 1){print("error!!!")} else{
    for(idx in 1:length(tmp_class)){
      list_class[tmp_class[idx]] <- ifelse(idx %% 2 == 1, "당기", "전기")
    }
  }
  
  return(list_class)
}

### (사전정의3) 숫자형변수 starting point 정의 
list_start_fun <- function(tmp_table){
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
  } else{print("error!")}
  
  if(any(str_detect(substr(unit_data, 1, 10), "천원"))==TRUE){
    unit <- 1000} else if(any(str_detect(substr(unit_data, 1, 10), "백만원"))==TRUE){
      unit <- 1000000
    } else {print("error!")}
  
  return(unit)
}

### 각 테이블이 해당하는 case 출력해주는 함수 
list_case_fun <- function(tmp_table, col_name, start_c){
  
  tmp_list_t <- c("매출", "채권", "매입", "지급수수료", "비용", "수익", "예치금", "부채", "배당", "미지급금", 
                  "미사용", "자금대여", "기타")
  col_name_new <- gsub(" ", "", col_name)
  
  if(!(all(str_detect(paste(col_name_new, collapse = ""), c("당기", "전기"))) 
       | all(str_detect(paste(col_name_new, collapse = " "), c("\\(당\\)기", "\\(전\\)기")))
       | all(str_detect(paste(col_name_new, collapse = ""), c("2018", "2017"))) )){tmp_case <- "case1"
  } else if(any(str_detect(col_name_new, paste(tmp_list_t, collapse = '|'))) == TRUE){tmp_case <- "case2"
  } else if(any(str_detect(tmp_table[,(start_c-1)], paste(tmp_list_t, collapse = '|'))) == TRUE) {tmp_case <- "case3"
  } else {print("error!")}
  
  return(tmp_case)
}


############################## step 2. 최종함수 dart_get 함수 정의 ##################################

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
      ### 특이케이스 제거 !!! - 구분, 회사명 외에 필요없는 구분자가 있을 경우 제거
      tmp <- tmp[,!str_detect(colnames(tmp), "거래내용")]
      ### '합계', '계' 있는 행 제거 
      tmp <- tmp[!ifelse(tmp[,1]=="합계", TRUE, FALSE),]; tmp <- tmp[!ifelse(tmp[,1]=="계", TRUE, FALSE),]
      
      ### colnames가 단위에 대한 내용이 아니라면 첫행으로 추가하기 + start_r_tmp에 1 더해주기
      start_r_tmp <- list_start_fun(tmp)[1]
      if(!any(str_detect(colnames(tmp), "단위"))){
        tmp <- rbind(colnames(tmp), tmp); start_r_tmp2 <- start_r_tmp + 1 } else {start_r_tmp2 <- start_r_tmp}

      ### col_name_tmp 명명하기
      if(start_r_tmp2 == 1){col_name_tmp <- colnames(tmp)} else if(start_r_tmp2==2){
        col_name_tmp <- text_clean(tmp[1,])} else if(start_r_tmp2==3){
          col_name_tmp <- paste0(text_clean(tmp[1,]), "@", text_clean(tmp[2,]))} else if(start_r_tmp2 == 4){
            col_name_tmp <- paste0(text_clean(tmp[1,]), "@", text_clean(tmp[2,]), "@", text_clean(tmp[3,]))
          } else if(start_r_tmp2 == 5){
            col_name_tmp <- paste0(text_clean(tmp[1,]), "@", text_clean(tmp[2,]), "@", text_clean(tmp[3,]), "@", text_clean(tmp[4,]))
          } else {print("error!!!")}
      
      ### 테이블 케이스 벡터 정의하기
      list_case[i] <- list_case_fun(tmp, col_name_tmp, list_start_fun(tmp)[2])
    }
  }
  
  final_table <- data.frame()
  tmp <- data.frame()
  for(i in 1:length(list_df)){
    if(list_TF[i] == TRUE){
      ### list_df의 테이블을 tmp에 저장
      tmp <- list_df[[i]]
      
      ### tmp text_clean 수행
      tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)), stringsAsFactors = F)
      ### 특이케이스 제거 !!! - 구분, 회사명 외에 필요없는 구분자가 있을 경우 제거
      tmp <- tmp[,!str_detect(colnames(tmp), "거래내용")]
      ### '합계', '계' 있는 행 제거 
      tmp <- tmp[!ifelse(tmp[,1]=="합계", TRUE, FALSE),]; tmp <- tmp[!ifelse(tmp[,1]=="계", TRUE, FALSE),]
      
      ### 사전정의 변수 - start_r, start_c
      start_r <- list_start_fun(tmp)[1]
      start_c <- list_start_fun(tmp)[2]
      
      ### colnames가 단위에 대한 내용이 아니라면 첫행으로 추가하기 + start_r에 1 더해주기
      if(!any(str_detect(colnames(tmp), "단위"))){
        tmp <- rbind(colnames(tmp), tmp); start_r_after <- start_r + 1 } else {start_r_after <- start_r }
      
      ### col_name 명명하기
      if(start_r_after == 1){col_name <- colnames(tmp)} else if(start_r_after==2){col_name <- text_clean(tmp[1,])
      } else if(start_r_after==3){
        col_name <- paste0(text_clean(tmp[1,]), "@", text_clean(tmp[2,]))
        } else if(start_r_after == 4){
          col_name <- paste0(text_clean(tmp[1,]), "@", text_clean(tmp[2,]), "@", text_clean(tmp[3,]))
        } else if(start_r_after == 5){
          col_name <- paste0(text_clean(tmp[1,]), "@", text_clean(tmp[2,]), "@", text_clean(tmp[3,]), "@", text_clean(tmp[4,]))
        } else {print("error!!!")}
      
      ### 해당 테이블의 case 정하기
      tmp_case <- list_case[i]
      
      ### 해당 테이블이 case1일때 수행하는 함수
      if(tmp_case == "case1"){
        # 사전정의 함수 - 당기/전기 구분
        list_class <- list_class_fun(list_TF, list_case, list_df)
        # 구분항목이 행으로 입력되어 있는 경우, 열로 입력되어 있는 경우 나눠서 수행
        real_r <- grepl("기업", tmp[,1]) | grepl("기타", tmp[,1]) | grepl("회사", tmp[,1])
        if(!(str_detect(tmp[real_r,2], "종속기업")[1] %in% c(NA, FALSE))){
          # row 이름 명명하기
          row_name2 <- text_clean(tmp[,1])
          row_name1 <- na.locf(ifelse(str_detect(row_name2, "기업") == TRUE, row_name2, NA), na.rm=FALSE, fromLast = FALSE)
          row_name <- paste0(row_name1, "@", row_name2); row_name <- row_name[!real_r]; row_name <- row_name[(start_r_after-1):length(row_name)]
          # 숫자만 있는 테이블로 만들기
          tmp <- tmp[!real_r,]
          tmp <- tmp[(start_r_after-1):nrow(tmp), start_c:ncol(tmp)]
        } else{
          # row 이름 명명하기
          row_name1 <- text_clean(tmp[,1])
          row_name2 <- text_clean(tmp[,2])
          row_name <- paste0(row_name1, "@", row_name2); row_name <- row_name[start_r_after:length(row_name)]
          # 숫자만 있는 테이블로 만들기
          tmp <- tmp[start_r_after:nrow(tmp), start_c:ncol(tmp)]
        }
        # 숫자만 있는 테이블로 만들기
        unit <- list_unit_fun(list_df, i, get_startend(path, 2))
        tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(gsub(",", "", gsub("\\s", "", x))) * unit))
        rownames(tmp) <- row_name; colnames(tmp) <- col_name[start_c:length(col_name)]
        # category1를 다시 만들어서 table 재정의_col
        category1 <- t(as.data.frame(str_split(row_name, "@")))
        tmp$구분 <- category1[,1]; tmp$회사명 <- category1[,2]
        tmp$당기_전기 <- list_class[i]
        tmp <- tmp[, c((ncol(tmp)-2):ncol(tmp), 1:(ncol(tmp)-3))]
        # category1를 다시 만들어서 table 재정의_row
        tmp <- tmp[!str_detect(rownames(tmp), paste(c("소계", "합계"), collapse = '|')),]
        rownames(tmp) <- NULL
      } else if (tmp_case == "case2"){
        
        ### 해당 테이블이 case2일때 수행하는 함수
        # 당기/전기 구분이 2행에 되어 있는 예외적인 경우 수행
        if(any(grepl(c("당", "전"), tmp[2,])) == TRUE){tmp <- tmp[c(2,1,3:nrow(tmp)),]}     
        if(any(grepl(c("2017", "2018"), tmp[2,])) == TRUE){tmp <- tmp[c(2,1,3:nrow(tmp)),]}
        # col, row 이름 명명하기
        col_name1 <- text_clean(ifelse(grepl("당", tmp[1,]), "당기", ifelse(grepl("전", tmp[1,]), "전기",
                                ifelse(grepl("2018", tmp[1,]), "당기", ifelse(grepl("2017", tmp[1,]), "전기", "구분")))))
        if(start_r_after == 1){print("error!!!!!")} else if(start_r_after==2){col_name <- text_clean(col_name1)} else if(start_r_after==3){
          col_name <- paste0(text_clean(col_name1), "@", text_clean(tmp[2,]))} else if(start_r_after == 4){
            col_name <- paste0(text_clean(col_name1), "@", text_clean(tmp[2,]), "@", text_clean(tmp[3,]))
          } else {print("error!!!!!")}
        # tmp clean 작업
        tmp <- as.data.frame(lapply(tmp, function(x) text_clean(x)), stringsAsFactors = F)
        colnames(tmp) <- col_name
        tmp <- tmp[start_r_after:nrow(tmp), ]  
        # 테이블 틀 만들기
        tmp <- melt(tmp, id=c(colnames(tmp)[1], colnames(tmp)[2]))
        unit <- list_unit_fun(list_df, i,  get_startend(path, 2))
        tmp$value <- as.numeric(gsub(",", "", tmp$value)) * unit 
        category2 <- t(as.data.frame(str_split(tmp$variable, "@")))
        tmp$당기_전기 <- category2[,1]; tmp$col_var <- category2[,2]
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
        if(dim(tmp_table)[2] == 6){
          tmp[,3] <- paste0(tmp[,3], "@", tmp[,4]); tmp <- tmp[,-4]
        }
        # 테이블 틀 만들기
        tmp <- tmp[start_r_after:nrow(tmp),]
        tmp <- melt(tmp, id=c(colnames(tmp)[1], colnames(tmp)[2], colnames(tmp)[3]))
        unit <- list_unit_fun(list_df, i, get_startend(path, 2))
        tmp$value <- as.numeric(gsub(",", "", tmp$value)) * unit 
        colnames(tmp)[1:2] <- c("구분", "회사명")
        tmp <- dcast(tmp, 구분 + 회사명 + variable ~ tmp[,3], sum)
        colnames(tmp)[3] <- "당기_전기"
        # category를 다시 만들어서 table 재정의_row
        tmp <- tmp[!str_detect(tmp[,2], paste(c("소계", "합계", "기타"),collapse = '|')),]
        rownames(tmp) <- NULL
      } else {print("error!!!!!!!!")}
      
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


############################## step 3-1. error_check case1 ##################################

### '61_(주)포스코' -> 확인완료(43,17)
sample61 <- as.character(kospi200$링크[61]); kospi200[61,]    
posco_table <- dart_get(path=sample61, index=61); dim(posco_table)

# '1_(주)BNK금융지주' -> 확인완료(16,30)
sample1 <- as.character(kospi200$링크[1]); kospi200[1,]  
BNK_table <- dart_get(path=sample1, index=1); dim(BNK_table)

# '5_(주)녹십자' -> 확인완료(39,22)
sample5 <- as.character(kospi200$링크[5]); kospi200[5,]  
green_table <- dart_get(path=sample5, index=5); dim(green_table)

### '6_(주)녹십자홀딩스' -> 확인완료(51,13)
sample6 <- as.character(kospi200$링크[6]); kospi200[6,]   
greenhol_table <- dart_get(path=sample6, index=6); dim(greenhol_table)

### '9_(주)대우건설' -> 확인완료(96,21)
sample9 <- as.character(kospi200$링크[9]); kospi200[9,]
daewoo_table <- dart_get(path=sample9, index=9); dim(daewoo_table)

### '10_(주)대웅제약' -> 확인완료(58,25)
sample10 <- as.character(kospi200$링크[10]); kospi200[10,]
daewoong_table <- dart_get(path=sample10, index=10); dim(daewoong_table)

### '11_(주)대한항공' -> 확인완료(41,12)
sample11 <- as.character(kospi200$링크[11]); kospi200[11,]
daehanair_table <- dart_get(path=sample11, index=11); dim(daehanair_table)

### '12_(주)동서' -> 확인완료(30,20)????????????
sample12 <- as.character(kospi200$링크[12]); kospi200[12,]
dongsoe_table <- dart_get(path=sample12, index=12); dim(dongsoe_table)

### '14_(주)동원에프앤비' -> 확인완료(41,15)
sample14 <- as.character(kospi200$링크[14]); kospi200[14,]
dongwon_table <- dart_get(path=sample14, index=14); dim(dongwon_table)

### '16_(주)락앤락'-> 확인완료(36,25)
sample16 <- as.character(kospi200$링크[16]); kospi200[16,]
rocknrock_table <- dart_get(path=sample16, index=16); dim(rocknrock_table)

### '22_(주)삼양사'-> 확인완료(34,19)
sample22 <- as.character(kospi200$링크[22]); kospi200[22,]
samyangsa_table <- dart_get(path=sample22, index=22); dim(samyangsa_table)

### '23_(주)삼양홀딩스'-> 확인완료(36,15)
sample23 <- as.character(kospi200$링크[23]); kospi200[23,]
samyanghold_table <- dart_get(path=sample23, index=23); dim(samyanghold_table)

### '126_(주)삼성전자'-> 확인완료(50,10)
sample126 <- as.character(kospi200$링크[126]); kospi200[126,]
samsung_table <- dart_get(path=sample126, index=126); dim(samsung_table)


############################## step 3-2. error_check case2 ##################################

### '2_(주)DB하이텍' -> case1 + case2 -> 확인완료(18,14)
sample2 <- as.character(kospi200$링크[2]); kospi200[2,]
dbhitech_table <- dart_get(path=sample2, index=2); dim(dbhitech_table)


############################## step 3-3. error_check case3 ##################################

### '3_(주)KB금융지주'-> 확인완료(26,27)???????????
sample3 <- as.character(kospi200$링크[3]); kospi200[3,]  
kbfinan_table <- dart_get(path=sample3, index=3); dim(kbfinan_table)

### '4_(주)강원랜드'-> 확인완료(6,6)???????????
sample4 <- as.character(kospi200$링크[4]); kospi200[4,] 
gangwon_table <- dart_get(path=sample4, index=4); dim(gangwon_table)

### '24_(주)세아베스틸'-> 확인완료(54,24)???????????
sample24 <- as.character(kospi200$링크[24]); kospi200[24,]
seah_table <- dart_get(path=sample24, index=24); dim(seah_table)


############################## step 3-4. error_check 혼합일 경우 ##################################

### '7_(주)농심' -> 확인완료(94,12)
sample7 <- as.character(kospi200$링크[7]); kospi200[7,]
nongsim_table <- dart_get(path=sample7, index=7); dim(nongsim_table)

### '8_(주)대교' -> 확인완료(80,18)
sample8 <- as.character(kospi200$링크[8]); kospi200[8,]
daekyo_table <- dart_get(path=sample8, index=8); dim(daekyo_table)

### '13_(주)동양'-> new case
sample13 <- as.character(kospi200$링크[13]); kospi200[13,]
dongyang_table <- dart_get(path=sample13, index=13); dim(dongyang_table)


############################## step 4. 불러들인 자료 엑셀로 저장하기 ##################################
n_list <- list(ls(pattern = "table"))
n <- length(n_list[[1]])
write.xlsx(get(n_list[[1]][1]), file = "example_27.xlsx", sheetName = "sheet1")
name_tmp <- paste0("sheet", 2:n)
for (j in 2:n){
  write.xlsx(get(n_list[[1]][j]), file = "example_27.xlsx", sheetName = name_tmp[j-1], append=TRUE)
}

