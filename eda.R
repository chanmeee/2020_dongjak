###### EDA 그래프 그리기 ######
## [참고] 동작구 휘장에 사용된 하늘색을 최대한 활용하여 시각화하였다. 해당색의 RGB 색상코드는 209DD9인 것으로 확인하였다. 

# Load library
library(tidyverse)
library(data.table)
library(ggplot2)
library(GGally)

# Load data
setwd('../2020 dongjak contest/data')
retro <- fread("retro.csv")

###### retro 데이터 EDA ######
# 소재지번 -> 동 추출 
# word_func <- function(word){
#   data_split <- strsplit(word, ' ')
#   split <- data.frame(sep=unlist(data_split))
#   
#   dong <- split$sep[3]
#   split_df <- data.frame(dong=dong)
#   
#   return(split_df)
# }
# for (i in 1:nrow(retro)) {
#   word <- retro$소재지지번[i]
#   word_temp <- word_func(word)
#   if (i == 1) {
#     word_df <- word_temp
#   } else {
#     word_df <- rbind(word_df, word_temp)
#   }
# }
# 
# retro$동 <- word_df$dong

# 행정동명  
retro %>% 
  ggplot(aes(행정동명)) + geom_bar(fill='#209DD9') +
  theme(axis.text.x = element_text(size = 7, angle = 30))
## [결과] 상도동, 사당동, 노량진동 순으로 많이 분포되어 있으며, 상도1동에는 없다. 


# 업종 
retro %>% 
  ggplot(aes(업종명)) + geom_bar(fill='#209DD9') +
  theme(axis.text.x = element_text(size = 7, angle = 30))

# 업태명 
retro %>% 
  ggplot(aes(업태명)) + geom_bar(fill='#209DD9') +
  theme(axis.text.x = element_text(size = 7, angle = 25))

# 연도 
retro %>% 
  ggplot(aes(년도)) + 
  geom_histogram(fill='#209DD9')

# 허가신고일 
retro %>% 
  ggplot(aes(허가신고일)) + 
  geom_histogram(fill='#209DD9')

# 영업자시작일 
retro$영업자시작_year <- substr(retro$영업자시작일, 1, 4)
retro$영업자시작_month <- substr(retro$영업자시작일, 5, 6)
retro %>% 
  ggplot(aes(영업자시작_year)) + geom_bar(fill='#209DD9') 
retro %>% 
  ggplot(aes(영업자시작_month)) + geom_bar(fill='#209DD9') 
#[결과] 지금까지 영업하는 옛 가게들 중 가장 오래된 가게는 1975년에 연 '대방떡방앗간'이었다. 월별로 보면 11월에 개업한 가게가 가장 많았고 1, 2월에 개업한 가게는 가장 적었다. 

# 소재지시작일 
retro$소재지시작_year <- substr(retro$소재지시작일, 1, 4)
retro$소재지시작_month <- substr(retro$소재지시작일, 5, 6)
retro %>% 
  ggplot(aes(소재지시작_year)) + geom_bar(fill='#209DD9') +
  theme(axis.text.x = element_text(size = 7, angle = 25))
retro %>% 
  ggplot(aes(소재지시작_month)) + geom_bar(fill='#209DD9') 
#[결과] 지금까지 터를 지키고 있는 가게들 중 1993~1999년에 현재 위치에서 영업하던 가게들이 많았다. 

# 업소위치 
retro %>% 
  ggplot(aes(업소위치)) + geom_bar(fill='#209DD9')
#[결과] 지상에 위치한 경우가 압도적으로 많았다. 

# 종업원
ggplot(data=retro) + 
  geom_bar(aes(종업원남), fill='#209DD9') 
ggplot(data=retro) + 
  geom_bar(aes(종업원여), fill='#209DD9') 

# 면적  
retro[,38:49] %>%
  ggpairs(na.omit=T) 


##################### 더 추가할 예정 ##################### 


###### cluster 데이터 EDA ######

# Load data
setwd('../2020 dongjak contest/data')
cluster <- fread("클러스터링/fianl2.csv")

# 







