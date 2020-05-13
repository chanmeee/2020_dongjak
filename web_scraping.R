###### 오래된가게 언급량 변화 웹 스크래핑 ######

# Load library
library(httr)
library(rvest)
library(jsonlite)

# 테스트 
httr::GET(url = "https://section.blog.naver.com/ajax/SearchList.nhn",
          query = list("countPerPage" = "7",
                         "currentPage"  = 1,
                       "endDate" = "2020-04-30",
                       "keyword" = "동작구 레트로",
                       "orderBy" = "sim",
                       "startDate" = "2011-05-01",,
                         "type"         = "post"),
          add_headers("referer" = "https://section.blog.naver.com/Search/Post.nh")) %>% 
  print()

# Create an empty dataframe
Nblog <- c()

# Web Scraping - Naver blog
for (i in 1:50) {
  httr::GET(url = "https://section.blog.naver.com/ajax/SearchList.nhn",
            query = list("countPerPage" = "7",
                         "currentPage" = i, # 페이지숫자를 i로 지정
                         "endDate" = "2020-04-30",
                         "keyword" = "동작구 레트로",
                         "orderBy" = "sim",
                         "startDate" = "2011-05-01",
                         "type" = "post"),
            add_headers("referer" = "https://section.blog.naver.com/Search/Post.nh")) %>%
    httr::content(as = "text") %>% # text 형식으로 변환
    str_remove(pattern = '\\)\\]\\}\',') %>% # 특수문자 제거
    jsonlite::fromJSON() -> naverBlog # json을 R 데이터 프레임으로 변환
  
  data <- naverBlog$result$searchList # 크롤링 내용을 data에 먼저 저장
  Nblog <- dplyr::bind_rows(Nblog, data) # 원래 만들어 두었던 Nblog에 계속해서 추가
  
  cat(i, "번째 페이지 정리 완료\n")
  Sys.sleep(time = 3)
 }

# Check Nblog dataset
dplyr::glimpse(Nblog)

# Select necessary columns
# Save URL for web-crawling
# Create new column (contents)
Nblog <- Nblog %>% 
  dplyr::select(1, 2, 4, 6, 9) %>% 
  dplyr::rename(id = blogId,
                no = logNo,
                posturl = postUrl,
                title = noTagTitle,
                name = blogName) %>% 
  dplyr::mutate(url = stringr::str_glue("http://blog.naver.com/PostView.nhn?blogId={id}&logNo={no}"), # 블로그 게시글 크롤링을 위한 url
                contents   = NA) # 콘텐츠가 들어갈 열

# Delete duplicated rows 
Nblog <- unique(Nblog)

# Check Nblog dataset
dplyr::glimpse(Nblog)

####### 각 블로그 게시글 내용을 크롤링 ####### 
# 각 블로그 url을 활용하여 크롤링해오는 for문을 만들었습니다.
# for문은 에러발생시 작동을 멈추는데요, 이 때 tryCatch() 함수를 사용하면 작동을 멈추는 대신 다른 행동을 명령할 수 있습니다.
# for문을 돌려보니 몇 개 게시글에서 오류가 발생하여 tryCatch()를 추가해주었습니다.
for(i in 1:nrow(Nblog)){
  tryCatch({
    Nblog$contents[i] <- httr::GET(url = Nblog$url[i]) %>% 
      xml2::read_html() %>% 
      rvest::html_nodes(css = "div.se-main-container")%>% 
      html_text(trim = TRUE)

    cat(i, "번째 블로그 글 내용 취합 완료\n")
    Sys.sleep(time = 3)

    }, error = function(e) cat(' --> 에러\n'))
}

# 에러가 생긴 글 중 첫번째걸 들어가보니, 글 자체가 없는 것은 아니고 css가 다르더군요. 
# 그래서 다시 변경된 css로 돌려줍니다.
which(is.na(Nblog$contents)) -> naData

# 위 for문을 고대로 복사해서 i에 대입될 문자만 변경해주었습니다.
for(i in naData){
  tryCatch({
    Nblog$contents[i] <- httr::GET(url = Nblog$url[i]) %>% 
      xml2::read_html() %>% 
      rvest::html_nodes(css = "div#postViewArea")%>% 
      html_text(trim = TRUE)
    
    cat(i, "번째 블로그 글 내용 취합 완료\n")
    Sys.sleep(time = 3)
    
  }, error = function(e) cat(' --> 에러\n'))
}

# 또 에러가 떠서 얘도 css가 다른지 들어가보았더니 그냥 그사이에 비공개로 바뀌었더군요.
# 당연히 비공개 글은 크롤링할 수 없습니다.
# 그래서 이 데이터는 삭제했습니다.
na.omit(Nblog) -> Nblog

####### 각 블로그 게시글 작성일을 크롤링 ####### 
for(i in 1:nrow(Nblog)){
  tryCatch({
    Nblog$date[i] <- httr::GET(url = Nblog$url[i]) %>% 
      xml2::read_html() %>% 
      rvest::html_nodes(css = "span.se_publishDate.pcol2")%>% 
      html_text(trim = TRUE)
    
    cat(i, "번째 블로그 글 날짜 취합 완료\n")
    Sys.sleep(time = 3)
    
  }, error = function(e) cat(' --> 에러\n'))
}

table(is.na(Nblog$date))

# 날짜 전처리 
word_func <- function(word){
  data_split <- strsplit(word, ' ')
  split <- data.frame(sep=unlist(data_split))
  year <- substr(split$sep[1], 1, 4)
  month <- substr(split$sep[2], 1, 2)
  day <- substr(split$sep[3], 1, 2)
  split_df <- data.frame(year=year,
                         month=month,
                         day=day)

  return(split_df)
}
for (i in 1:nrow(Nblog)) {
  word <- Nblog$date[i]
  word_temp <- word_func(word)
  if (i == 1) {
    word_df <- word_temp
  } else {
    word_df <- rbind(word_df, word_temp)
  }
}
Nblog$year <- word_df$year
Nblog$month <- word_df$month
Nblog$day <- word_df$day

levels(Nblog$month) <- c('08', '10', '04', '05', '03','06', 
                         '11', '02', '01', '07', '09', '12')
# month 레벨 이름 바꾸기
for (i in 1:12){
  if (endsWith(levels(Nblog$month)[i], '.') == TRUE){
    levels(Nblog$month)[i] <- paste0('0', as.character(substr(levels(Nblog$month)[i], 1, 1)))
  }
}

Nblog <- transform(Nblog, 
                   ymd = paste(year, month, day, sep="")) %>% 
  mutate(ymd = as.Date(ymd, format='%Y%m%d'))

  

###### 연별 언급량 변화 그래프 그리기 ######
library(ggplot2)

Nblog %>% 
  ggplot() +
  geom_bar(aes(x=yearmonth), fill='#209DD9')+
  theme(axis.text.x = element_text(size = 7, angle = 30))

# 추가수정
Nblog %>% 
  ggplot() +
  geom_freqpoly(aes(x=ymd, y=..count..), color='#209DD9', size=1) +
  ggtitle("동작구 레트로 키워드 검색량 변화") +
  #scale_x_continuous(limits=c(as.Date('2011-05-01', origin=lubridate::origin), as.Date('2020-04-30', origin=lubridate::origin)))
  annotate("text", x=as.Date('2018-08-27', '%Y%m%d'), y=68, "2018년 8월 27일")


###### 블로그 내용 전처리 ######
Nblog_contents <- Nblog$contents
Nblog_contents <- Nblog_contents %>% 
  #str_trim(Nblog_contents) # 공백 제거
  gsub("\\\n","",Nblog_contents) #\n 글자 제거 
  gsub("\\\t","",Nblog_contents) #\t 글자 제거
