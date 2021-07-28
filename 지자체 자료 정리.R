
######################################## 0. 환경설정 ########################################
# if(!require(readxl)){install.packages('readxl')}; library(readxl)   # 버전 바뀜
# if(!require(XLConnect)){install.packages('XLConnect')}; library(XLConnect)   # xls읽을 수 있지만, 자바설치 필요
if(!require(openxlsx)){install.packages('openxlsx')}; library(openxlsx)   # xlsx만 읽을 수 있음
if(!require(dplyr)){install.packages('dplyr')}; library(dplyr)
if(!require(stringr)){install.packages('stringr')}; library(stringr)
if(!require(zoo)){install.packages('zoo')}; library(zoo)

setwd("")

######################################## 1. 파일 정제 및 합치기 #######################################
file_list <- list.files(pattern="xls")   # 통제변수 폴더에 있는 xls파일 리스트 생성
data_set <- data.frame()

for(i in 1:length(file_list)){
  list_number <- file_list[i]   # i번째 파일 파일명을 list_number에 저장
  
  ### 연도별 다른 정제과정 필요
  if(substr(list_number,1,2) %in% c("06", "07")){
    ### 2006, 2007년 데이터 정제 과정
    tmp_data <- read_excel(list_number) %>% as.data.frame()   # readxl: 버전달라져서 오류발생
    # tmp_data <- readWorksheet(loadWorkbook(list_number), sheet=1)   # XLConnect: xls읽을 수 있지만, 자바설치 필요
    # tmp_data <- openxlsx::read.xlsx(list_number, sheet = 1)   # openxlsx: tmp_data에 파일저장
    tmp_nrow <- nrow(tmp_data); tmp_ncol <- ncol(tmp_data)   # dimension 계산
    row_name <- str_extract(gsub("\\s", "", tmp_data[,1]), "[가-힣]+")   # 1열정제(한글만 남기고 제거)하고 행제목 row_name에 저장
    col_name <- str_extract(gsub("\\s", "", tmp_data[3,]), "[가-힣]+")   # 3행정제(한글만 남기고 제거)하고 열제목 col_name에 저장
    tmp_data <- tmp_data[12:tmp_nrow,2:tmp_ncol]   # 금액 나타나있는 부분을 tmp_data에 저장
    rownames(tmp_data) <- row_name[12:tmp_nrow]   # tmp_data의 행제목 변경
    colnames(tmp_data) <- col_name[2:tmp_ncol]   # tmp_data의 열제목 변경
    tmp_data <- tmp_data[,!is.na(colnames(tmp_data))]   # tmp_data에 NA있는 열 제거
  } else if(substr(list_number,1,2) %in% c("08")){
    ### 2008년 데이터 정제 과정
    # tmp_data <- read_excel(list_number) %>% as.data.frame()   # readxl: 버전달라져서 오류발생
    # tmp_data <- readWorksheet(loadWorkbook(list_number), sheet=1)   # XLConnect: xls읽을 수 있지만, 자바설치 필요
    tmp_data <- openxlsx::read.xlsx(list_number, sheet = 1)   # openxlsx: tmp_data에 파일저장    
    tmp_data <- tmp_data[!is.na(tmp_data[,1]),]   # tmp_data의 NA인 행 제거
    tmp_data <- tmp_data[,!is.na(tmp_data[1,])]   # tmp_data의 NA인 열 제거
    tmp_nrow <- nrow(tmp_data); tmp_ncol <- ncol(tmp_data)   # dimension 계산
    row_name <- str_extract(gsub("\\s", "", tmp_data[,1]), "[가-힣]+")   # 1열정제(한글만 남기고 제거)하고 행제목 row_name에 저장
    col_name <- str_extract(gsub("\\s", "", tmp_data[1,]), "[가-힣]+")   # 1행정제(한글만 남기고 제거)하고 열제목 col_name에 저장
    tmp_data <- tmp_data[10:tmp_nrow,2:tmp_ncol]   # 금액 나타나있는 부분을 tmp_data에 저장
    rownames(tmp_data) <- row_name[10:tmp_nrow]   # tmp_data의 행제목 변경
    colnames(tmp_data) <- col_name[2:tmp_ncol]   # tmp_data의 열제목 변경
    tmp_data <- tmp_data[,!is.na(colnames(tmp_data))]   # tmp_data에 NA있는 열 제거
  } else if(as.numeric(substr(list_number,1,2)) >= 09){
      ### 2009년 이상 데이터 정제과정
    sheets <- excel_sheets(list_number)  # 버전달라져서 오류발생
    tmp_list <- lapply(sheets, function(x) read_excel(list_number, sheet = x))  # 버전달라져서 오류발생
    # sheets = openxlsx::getSheetNames(list_number)   # sheet명 알아내기
    # tmp_list <- lapply(sheets, function(x) openxlsx::read.xlsx(list_number, sheet = x))   # sheet별로 리스트 생성(sheet갯수 = list length)
    tmp_data <- do.call("cbind", tmp_list)   # list형태를 data.frame으로 cbind해서 tmp_data에 저장
    tmp_data <- tmp_data[-1,]   # tmp_data의 1열 제거
    tmp_data <- tmp_data[!is.na(tmp_data[,1]),]   # tmp_data의 NA인 행 제거
    tmp_data <- tmp_data[,!is.na(tmp_data[1,])]   # tmp_data의 NA인 열 제거
    row_name <- str_extract(gsub("\\s", "", tmp_data[,1]), "[가-힣]+")   # 1열정제(한글만 남기고 제거)하고 행제목 row_name에 저장
    col_name <- str_extract(gsub("\\s", "", tmp_data[1,]), "[가-힣]+")   # 1행정제(한글만 남기고 제거)하고 열제목 col_name에 저장
    tmp_nrow <- nrow(tmp_data)   # dimension 계산
    start_n <- which(str_detect(tmp_data[,1], "인건비"))   # 인건비부터 예비비및기타 항목까지 필요하므로 인건비가 포함된 행 알아내기
    tmp_data <- tmp_data[start_n:tmp_nrow,!col_name %>% str_detect("단체별")]   # 필요한 행, 열을 선별해서 tmp_data에 저장
    tmp_ncol <- ncol(tmp_data)   # dimension 계산
    rownames(tmp_data) <- row_name[start_n:tmp_nrow]   # tmp_data의 행제목 변경
    colnames(tmp_data) <- col_name[!col_name %>% str_detect("단체별")]   # tmp_data의 열제목 변경
    tmp_data <- tmp_data[,!is.na(colnames(tmp_data))]   # tmp_data에 NA있는 열 제거
    }
  
  ### 1차 데이터 정제
  tmp_data <- tmp_data %>% select(-one_of("합계","시계","군계","구계")) %>% t()  # 특정열 제거 후 행렬전환
  연도 <- paste0("20", substr(list_number, 1, 2))   # 연도구하기
  
  if(str_detect(list_number, "14-2")){
    ### '특별광역시'(14-2)일 경우 수행
    수준 <- "광역시"   # 수준 = 광역시
    name1 <- rownames(tmp_data)   # name1 지정
    name2 <- rownames(tmp_data)   # name2 지정
  } else if(str_detect(list_number, "19-2")){
    ### '도'(19-2)일 경우 수행
    수준 <- "도"   # 수준 = 도
    name1 <- rownames(tmp_data)   # name1 지정
    name2 <- rownames(tmp_data)   # name2 지정
  } else if(str_detect(list_number, "24-2")){
    ### '시'(24-2)일 경우 수행
    수준 <- "시"   # 수준 = 시
    name1 <- rownames(tmp_data)   # name1 지정
    col_name <- (col_name[!is.na(col_name)])[-1]
    tmp_tf <- str_detect(col_name, "시계")
    name2 <- na.locf(ifelse(tmp_tf == FALSE, NA, col_name), fromLast = FALSE)
    name2 <- substr(name2[!tmp_tf], 1, 2)   # name2 지정
    } else if(str_detect(list_number, "29-2")){
    ### '군'(29-2)일 경우 수행
    수준 <- "군"   # 수준 = 군
    name1 <- rownames(tmp_data)   # name1 지정
    col_name <- (col_name[!is.na(col_name)])[-1]
    tmp_tf <- str_detect(col_name, "군계")
    name2 <- na.locf(ifelse(tmp_tf == FALSE, NA, col_name), fromLast = FALSE)
    name2 <- substr(name2[!tmp_tf], 1, 2)   # name2 지정
    } else if(str_detect(list_number, "34-2")){
    ### '자치구'(34-2)일 경우 수행
    수준 <- "구"   # 수준 = 구
    name1 <- rownames(tmp_data)   # name1 지정
    col_name <- (col_name[!is.na(col_name)])[-1]
    tmp_tf <- str_detect(col_name, "구계")
    name2 <- na.locf(ifelse(tmp_tf == FALSE, NA, col_name), fromLast = FALSE)
    name2 <- gsub("특별", "서울", name2)
    name2 <- substr(name2[!tmp_tf], 1, 2)   # name2 지정
    }
  name3 <- paste0(name1, "_", name2)   # name3 지정
  final_data <- cbind(연도, 수준, name1, name2, name3, tmp_data)   # tmp_data를 원하는 형태로 생성해서 final_data에 저장
  colnames(final_data)[6:ncol(final_data)] <- 
    c("인건비", "물건비", "경상이전", "자본지출", "융자및출자", "보전재원", "내부거래", "예비비및기타")   # 열이름정제(다시볼것...)
  
  ### 누적으로 데이터 합치는 과정
  data_set <- rbind(data_set, final_data)
}


### 2차 데이터 정제과정 _ name2 NA 처리
data_set <- as.data.frame(lapply(data_set, function(x) as.character(x)), stringsAsFactors = F)   # factor를 character로 변경
# 광주지역 먼저 처리
# data_set%>% filter(name1=="광주") #확인용
data_set[(data_set$수준=="시") & (data_set$name1 == "광주"), 3] <- "광주.1"   # 시 수준이고 name1이 광주일때 name2에 "광주.1" 값 넣어주기 
data_set$name2[(data_set$name1 == "통합청주")] <- "충북"   # name1이 통합청주일때 name2에 "충북" 값 넣어주기 

match_list_u <- data_set %>% filter(name2 != "NA") %>% select(starts_with("name")) %>% unique()  # unique한 c(name1, name2, name3) 구해서 match_list_u에 저장
match_list_o <- data_set %>% select(starts_with('name'))   # 바꿔야할 c(name1, name2, name3) 구해서 match_list_o에 저장
match_list <- left_join(match_list_o, match_list_u, by="name1")   # name1기준으로 값채워넣기 위해 join
match_list <- match_list %>% select(name1, name2.y)   # match_list에서 필요한 변수만 선택해서 저장
colnames(match_list) <- c("name1", "name2")   # match_list 이름 바꾸기
match_list$name1 <- str_extract(match_list$name1, "[가-힣]+")   # name1 정제: 한글만 남김
match_list$name3 <- paste0(match_list$name2, "_", match_list$name1)   # name3 새로 생성(name1이 변경되었기 때문)
data_set <- cbind(data_set, match_list)   # data_set 재생성
data_set <- data_set[,c(1,2,14:16,6:13)]   # data_set 재배열
rownames(data_set) <- NULL   # data_set rowname 제거하기


######################################## 2. 최종데이터 저장하기 #######################################
write.csv(data_set, file = "data_set.csv")

