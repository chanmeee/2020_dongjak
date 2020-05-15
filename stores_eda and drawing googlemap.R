# Delete All existing variables
#rm(list=ls())

# Load library
library(tidyverse)
library(data.table)
library(ggplot2)

# Set Directory
setwd('../2020 dongjak contest/data')

# Load Data (위경도 변환 파일)
food <- fread('food_lonlat.csv') 
names(food)
food <- rename(food, id=V1)
food$adress <- NULL
## V1열의 이름을 id로 변경하고, 유의미한 정보가 없어 불필요한 adresss열을 삭제했다. 


# 데이터 구조 살펴보기
dim(food)
## [결과] 29040행 60열로 이루어진 데이터셋이다. 
str(food)
## [결과] 변수명과 변수형태를 확인하였다. 이 부분은 별도의 표를 첨부하여 설명하겠다. 
summary(food)
## [결과] '년도' 열의 최소값이 994, 최대값이 3992로 데이터 입력할 때 오류가 발생했음을 알 수 있다. '허가신고일'은 1899년 12월 30일부터 2020년 4월 27일까지 있으며, '영업자시작일'은 995년 11월 1일부터 2020년 4월 27일까지 있다. 

# 결측값 확인 
colSums(is.na(food))
## [결과] 결측값을 살펴보니 '년도', '업종명', '업소명', '폐업일자', '소재지도로명' 등 분석에서 사용할 변수들에는 결측값이 없었고, 영업장 형태나 종업원 현황에 대한 변수들에 결측값이 다수 있었다. 

# 연도별 식품업소 개수 확인 및 전처리 
table(food$년도)
## 연도별 식품업소를 살펴본 결과, '994', '998', '2996', '3992'라는 오류값을 발견하여 해당 열을 살펴보았다.
food[food$년도=='994',]
food[food$년도=='998',]
food[food$년도=='2996',]
food[food$년도=='3992',]
## 994로 입력된 업소는 '종로식당'으로 영업자 시작일이 2004년 10월 21일이어서 994 대신 2004로 변경하였다. 998로 입력된 데이터는 1998년 5월 20일에 허가신고를 한 '오양임'으로 1998으로 년도를 수정하였다. 같은 방식으로 2296, 3992로 잘못 입력된 값을 각각 1996년, 1992년으로 수정하였다. 
food[food$년도=='994', '년도'] <- 2004
food[food$년도=='998', '년도'] <- 1998
food[food$년도=='2996', '년도'] <- 1996
food[food$년도=='3992', '년도'] <- 1992

# 20세기에 개업하여 현재까지 영업중인 식품업소 찾기 
old <- food %>% filter(영업자시작일 < 20000000)
retro <- old %>% filter(폐업일자 == ''|폐업일자 == "    ") # 처음에는 <폐업일자 == ''> 조건만 넣었다가 이후 소재지도로명 변수를 전처리하는 과정에서 새로운 형태의 빈칸("    ")이 나와서 추가하였다. 필터링 이후 관측값의 개수는 ''로만 필터링했을 떄와 동일하게 1392개였다. 
dim(old)
dim(retro)
## [결과] 20세기에 개업한 동작구 식품업소의 개수는 6750개이며, 이중 현재까지 영업중인 곳은 197개에 해당한다. 15% 정도만이 오랜 시간 동안 영업중이라는 것을 알 수 있다. 

# 레트로 식품업소들의 대략적인 위치 파악 
retro %>% 
  group_by(소재지도로명) %>% 
  summarise(n=n())
## 소재지도로명 변수에 "    "로 빈칸으로 아무것도 입력되지 않은 경우가 있었다. "    "으로 입력된 식품업소는 아래와 같다. 

retro %>% 
  select(id, 업종명, 업소명, 소재지도로명, 소재지지번) %>% 
  filter(소재지도로명 == "    ")
## 총 18개의 식품업소의 주소가 "    "으로 입력되어 있었다. 이 업소들의 주소는 인터넷에서 업소를 검색하여 직접 주소를 입력하려고 했지만.. 일단은 여기까지만 하고 오후에 이어서 하도록 하겠습니다. 

################ 소연이가 알려주면 이 부분 추가하기 ###################

# 중복 식당 삭제 
length(unique(retro$`허가(신고)번호`)) == nrow(retro)
## retro 데이터셋에 동일한 허가(신고)번호를 가진 경우가 있는 것을 확인할 수 있다.
overlap_permitnum <- retro[duplicated(retro$`허가(신고)번호`), 50]

retro %>% 
  filter(`허가(신고)번호` %in% unique(overlap_permitnum)) %>% 
  select(c(1,8,50))

## 인터넷에서 검색한 결과, 위 중복된 식당들 중에서 검색 결과 없음('다성분식', '청운꼬들살', '해피분식', '영떡볶이', '본동떡집'), 폐업('동해칼국수', '용원'), 단순 중복('대천분식', '영남', '파리바게뜨', '함지박떡방', '온누리건강원')인 경우가 있다는 것을 알 수 있었다.
## 따라서 id가 988, 1530, 1995, 2563, 3105, 5568, 6618, 11651, 11923, 11982, 16355,16356,16358, 21826 인 경우 삭제했다. 
retro <- retro %>% 
  filter(!id %in% c(988, 1530, 1995, 2563, 3105, 5568, 6618,
                    11651, 11923, 11982, 16355,16356,16358, 21826))
## 그 결과 retro 데이터셋의 열의 개수가 183개로 감소하였다. 

# 오래된 가게 전처리: 프랜차이즈 및 급식시설 제외하기, 업종 살펴보고 특성 파악하기

# 1) 프랜차이즈 업체 삭제 
## 프랜차이즈 업체는 레트로 문화 관광 자원으로서의 가치가 크지 않다고 판단하여 삭제하기로 하였다. 프랜차이즈 업체는 업소명이 동일할 것이라는 아이디어에서 착안하여 프랜차이즈 업체들을 필터링하였다. 
overlap_name <- retro %>% 
  filter(duplicated(업소명)) %>% 
  select(업소명)

retro %>% 
  filter(업소명 %in% unique(unlist(overlap_name[,1]))) %>% 
  select(c(1,4:9))
## [결과] 흑석동과 상도동에 위치한 김밥천국 2곳, 신대방동과 노량진동에 위치한 한솥 2곳, 그리고 같은 건물 1, 2층에 위치한 (주)태명종합식품이 있음을 확인할 수 있었다. 김밥천국과 한솥은 프랜차이즈 업체이므로 삭제하고, (주)태명종합식품은 유통/수입 업체이기 때문에 삭제하기로 하였다. 

retro <- retro %>% 
  filter(!업소명 %in% unique(unlist(overlap_name[,1]))) 
## 6개 업체가 삭제되어 177열이 남았다. 

# 2) 구내식당, 자판기, 유흥업소 삭제 
## 업종 분포 확인 
retro %>% 
  group_by(업종명) %>% 
  tally()

retro %>% 
  select(id, 업소명, 업종명) %>% 
  filter(!업종명 %in% c('일반음식점', '즉석판매제조가공업'))

## 35개 업소를 인터넷에서 검색해본 뒤, id가 12710, 14310, 15389, 15390, 19123인 업체만 적합하다고 판단하였다. 
retro <- retro %>% 
  filter(업종명 %in% c('일반음식점', '즉석판매제조가공업')
            | id %in% c(12710, 14310, 15389, 15390, 19123))

## 오타 수정 
retro$업소명 <- gsub("터방네", "터방내", retro$업소명) 

#write.csv(retro, 'retro.csv')


###### 가게를 지도에 표시하기 ###### 

#구글지도로 식품업소 좌표 찍기
devtools::install_github('dkahle/ggmap')
library(ggmap)
#register_google(key='AIzaSyCaA1p9a-iU4IjsLnmtbhPSP5jvmCa7__o') #찬미 api (인증 기다리는중)
register_google(key='AIzaSyANUdSFNm-jnJDFTcB2zexeAaRuUqbyHbo') #채아 api 

# 동작구 좌표에 맞춰 지도 불러오기
center <- c(mean(retro$lon, na.rm=T), mean(retro$lat, na.rm=T)) 
map <- ggmap(get_map(location=center, zoom=13, maptype='roadmap')) +
  # x축, y축 범위 설정 
  #scale_x_continuous(limits=c(126.90, 127.00)) +
  #scale_y_continuous(limits=c(37.475, 37.52))
  coord_cartesian(xlim=c(126.90, 127.00), ylim=c(37.475, 37.52)) 
  

# 동작구의 경계선을 그리고 싶다. 
library(raster)
korea_emd <- raster::shapefile('EMD_201905/TL_SCCO_EMD.shp')
korea_emd <- sp::spTransform(korea_emd, CRS("+proj=longlat"), region = 'EMD_CD')
korea_emd <- fortify(korea_emd)

#korea_emd_refer <- shapefile('EMD_201905/TL_SCCO_EMD.shp')
#korea_emd_refer <- fortify(korea_emd_refer)
#group <- levels(as.factor(korea_emd_refer$id))
#look <- data.frame(id=1:length(group), group=group)
#look[substr(look$group, 1, 5) == 11590,]

dongjak <- korea_emd %>% 
  filter(id %in% c(409:417)) 
#write.csv(dongjak, 'dongjak.csv')

# 상권 클러스터링 결과를 그리고 싶다.
real.final <- fread("클러스터링/real_final_new.csv")
new_cluster2 <- fread("new_cluster2.csv")
new_cluster2 <- new_cluster2[,1:4]
real.final_longlat <- left_join(real.final, new_cluster2,
                                 by = c('street.code' = 'street.code'))
real.final_longlat <- real.final_longlat %>% 
  mutate(mon_selng_amt_10 = mon_selng_amt *10,
         mon_selng_amt_20 = mon_selng_amt *20,
         mon_selng_amt_50 = mon_selng_amt *50)

###### 지도 그리기 ######
## 위도 1도가 약 133.33km, 상권 1개의 원의 지름이 500m 정도 되도록 그렸다. 
library(ggrepel) # 글자 겹치지 않도록 

###### 레트로 식당들 점 찍기 ######
map + 
  # 동작구 경계선 그리기 
  geom_polygon(data = dongjak, aes(x=long, y=lat, group=group), 
               fill='white', color='black', size=.75, alpha=.2) +
  # 동작구 내 업체 점 찍기 
  geom_point(data=retro, aes(x=lon, y=lat), 
             shape = 21, fill = '#209DD9', size=2, alpha=1) +
  labs(x=NULL, y=NULL) + scale_color_discrete(name="cluster type")

###### 클러스터링 2개일 때 ######
map + 
  # 동작구 경계선 그리기 
  geom_polygon(data = dongjak, aes(x=long, y=lat, group=group), 
               fill='white', color='black', size=.75, alpha=.5) +
  # 클러스터별로 상권 표시
  geom_point(data=real.final_longlat, aes(x=lon, y=lat, color=as.factor(result.k2.cluster)), 
             size=10, alpha=.4) +
  # 동작구 내 업체 점 찍기 
  geom_point(data=retro, aes(x=lon, y=lat), 
             shape = 21, fill = '#209DD9', size=2, alpha=1) +
  labs(x=NULL, y=NULL) + scale_color_discrete(name="cluster type")

###### 클러스터링 3개일 때 ######
map + 
  # 동작구 경계선 그리기 
  geom_polygon(data = dongjak, aes(x=long, y=lat, group=group), 
               fill='white', color='black', size=.75, alpha=.2) +
  # 클러스터별로 상권 표시
  geom_point(data=real.final_longlat, aes(x=lon, y=lat, color=as.factor(result.k3.cluster)), 
             size=10, alpha=.4) +
  # 동작구 내 업체 점 찍기 
  geom_point(data=retro, aes(x=lon, y=lat), 
             shape = 21, fill = '#209DD9', size=2, alpha=1) +
  labs(x=NULL, y=NULL) + scale_color_discrete(name="cluster type")

###### 클러스터링 6개일 때 ######
map + 
  # 동작구 경계선 그리기 
  geom_polygon(data = dongjak, aes(x=long, y=lat, group=group), 
               fill='white', color='black', size=.75, alpha=.5) +
  # 클러스터별로 상권 표시
  geom_point(data=real.final_longlat, aes(x=lon, y=lat, color=as.factor(result.k6.cluster)), 
             size=10, alpha=.4) +
  # 동작구 내 업체 점 찍기 
  geom_point(data=retro, aes(x=lon, y=lat), 
             shape = 21, fill = '#209DD9', size=2, alpha=1) +
  labs(x=NULL, y=NULL) + scale_color_discrete(name="cluster type")

###### 오래가게 표시 ######
seoul_orae <- data.table(id=1:2,
                         name=c('설화철물', '터방내'),
                         open_year=c(1980, 1983),
                         업종=c('철물점', '다방'),
                         주소=c('동작구 사당로 16길 7', '동작구 흑석로 101-7'),
                         lon=c(126.973943, 126.960766),
                         lat=c(37.482914, 37.508021))

map + 
  # 동작구 경계선 그리기 
  geom_polygon(data = dongjak, aes(x=long, y=lat, group=group), 
               fill='white', color='black', size=.75, alpha=.5) +
  # 클러스터별로 상권 표시
  geom_point(data=real.final_longlat, aes(x=lon, y=lat, color=as.factor(result.k6.cluster)), 
             size=10, alpha=.4) +
  # 동작구 내 업체 점 찍기 
  geom_point(data=retro, aes(x=lon, y=lat), 
             shape = 21, fill = '#209DD9', size=2, alpha=1) +
  # 서울시 지정 오래가게 점 찍기 
  geom_point(data=seoul_orae, aes(x=lon, y=lat), 
             shape = 21, fill = 'red', size=2, alpha=1) + 
  labs(x=NULL, y=NULL) + scale_color_discrete(name="cluster type")


###### 구역별 확대 ######
map2 <- ggmap(get_map(location=center, zoom=13, maptype='roadmap'))
  # x축, y축 범위 설정 
  #scale_x_continuous(limits=c(126.90, 127.00)) +
  #scale_y_continuous(limits=c(37.475, 37.52))

# 사당동 확대 지도 
map2 + 
  coord_cartesian(xlim=c(126.96, 126.99), ylim=c(37.475, 37.496)) + 
  # 동작구 경계선 그리기 
  geom_polygon(data = dongjak, aes(x=long, y=lat, group=group), 
               fill='white', color='black', size=.75, alpha=.5) +
  # 클러스터별로 상권 표시
  geom_point(data=real.final_longlat, aes(x=lon, y=lat, color=as.factor(result.k6.cluster)), 
             size=10, alpha=.4) +
  # 동작구 내 업체 점 찍기 
  geom_point(data=retro, aes(x=lon, y=lat), 
             shape = 21, fill = '#209DD9', size=2, alpha=1) +
  # 서울시 지정 오래가게 점 찍기 
  geom_point(data=seoul_orae, aes(x=lon, y=lat), 
             shape = 21, fill = 'red', size=2, alpha=1) + 
  # 가게 이름 주석 달기 
  geom_text_repel(data=dplyr::filter(retro, between(lon, 126.96, 126.99) & between(lat, 37.475, 37.496)), aes(label=업소명), size=2.5) +
  geom_text_repel(data=dplyr::filter(seoul_orae, between(lon, 126.96, 126.99) & between(lat, 37.475, 37.496)), aes(x=lon+.00055, y=lat-.0006, label=name), size=2.5, color='red') +
  labs(x=NULL, y=NULL) + scale_color_discrete(name="cluster type")

# 흑석동 확대 지도 
map2 + 
  coord_cartesian(xlim=c(126.9554, 126.986), ylim=c(37.500, 37.515)) + 
  # 동작구 경계선 그리기 
  geom_polygon(data = dongjak, aes(x=long, y=lat, group=group), 
               fill='white', color='black', size=.75, alpha=.5) +
  # 클러스터별로 상권 표시
  geom_point(data=real.final_longlat, aes(x=lon, y=lat, color=as.factor(result.k6.cluster)), 
             size=10, alpha=.4) +
  # 동작구 내 업체 점 찍기 
  geom_point(data=retro, aes(x=lon, y=lat), 
             shape = 21, fill = '#209DD9', size=2, alpha=1) +
  # 서울시 지정 오래가게 점 찍기 
  geom_point(data=seoul_orae, aes(x=lon, y=lat), 
             shape = 21, fill = 'red', size=2, alpha=1) + 
  # 가게 이름 주석 달기 
  geom_text_repel(data=dplyr::filter(retro, between(lon, 126.9554, 126.986) & between(lat, 37.500, 37.515) & 업소명 != '터방내'), aes(label=업소명), size=2.5) +
  geom_text_repel(data=dplyr::filter(seoul_orae, between(lon, 126.9554, 126.986) & between(lat, 37.500, 37.515)), aes(x=lon+.00001, y=lat, label=name), size=2.5, color='red') +
  labs(x=NULL, y=NULL) + scale_color_discrete(name="cluster type")

# 신대방2동 확대 지도 
map2 + 
  coord_cartesian(xlim=c(126.915, 126.9425), ylim=c(37.4899, 37.506)) + 
  # 동작구 경계선 그리기 
  geom_polygon(data = dongjak, aes(x=long, y=lat, group=group), 
               fill='white', color='black', size=.75, alpha=.5) +
  # 클러스터별로 상권 표시
  geom_point(data=real.final_longlat, aes(x=lon, y=lat, color=as.factor(result.k6.cluster)), 
             size=10, alpha=.4) +
  # 동작구 내 업체 점 찍기 
  geom_point(data=retro, aes(x=lon, y=lat), 
             shape = 21, fill = '#209DD9', size=2, alpha=1) +
  # 서울시 지정 오래가게 점 찍기 
  geom_point(data=seoul_orae, aes(x=lon, y=lat), 
             shape = 21, fill = 'red', size=2, alpha=1) + 
  # 가게 이름 주석 달기 
  geom_text_repel(data=dplyr::filter(retro, between(lon, 126.915, 126.9425) & between(lat, 37.4899, 37.506)), aes(label=업소명), size=2.5) +
  geom_text_repel(data=dplyr::filter(seoul_orae, between(lon, 126.915, 126.9425) & between(lat, 37.4899, 37.506)), aes(x=lon+.00001, y=lat, label=name), size=2.5, color='red') +
  annotate("text", x=126.919626, y=37.492519, label= "보라매공원", size=3) +
  annotate("text", x=126.937662, y=37.491003, label= "상도공원", size=3) +
  labs(x=NULL, y=NULL) + scale_color_discrete(name="cluster type")


#######################################
