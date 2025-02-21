rm(list=ls())



library(fitdistrplus)
library(pscl)  
library(VGAM)
library(readxl)  # 엑셀 파일을 읽기 위한 패키지 로드
library(dplyr)   # 데이터 조작을 위한 패키지 로드
library(ggplot2)
library(tidyr)
library(fitdistrplus)
library(igraph)
library(tidyverse)
library(fitdistrplus)
library(gamlss)
library(gridExtra)
library(grid)
library(cowplot)



################################offspring########################################
# 
# #전체데이터 불러오기기
# linelist <- read_excel("2_SSE_data_Wildtype_KimH_JIPH2025.xlsx")
# 
# 
# #source of infection, infectee 변수 선택 및 변수명 변경
# overall_data <- linelist %>% dplyr::select("source of infection", "infectee") %>%
#   rename(infector.case = "source of infection", infectee.case = "infectee")
# 
# 
# 
# #변수를 숫자 타입으로 바꾸기 
# overall_data <- overall_data %>%
#   mutate(
#     infector.case = gsub("#", "", infector.case),
#     infectee.case = gsub("#", "", infectee.case)
#   )
# 
# 
# print(overall_data)
# 
# 

overall_data <- read_excel("overall_data.xlsx")





#infector.case 별로 감염시킨 사람의 수를 계산
offspring <- overall_data %>%
  dplyr::select(infector.case) %>%
  group_by(infector.case) %>%
  count() %>%
  arrange(desc(n))

#감염된 사람들의 정보를 추출
infectee <- overall_data %>%
  dplyr::select(infector.case, infectee.case) %>%
  gather() %>%
  filter(key == 'infectee.case')

#감염시킨 사람들만 추출
infector <- overall_data %>%
  dplyr::select(infector.case, infectee.case) %>%
  gather() %>%
  filter(key == 'infector.case')

#감염시킨 사람과 감염된 사람의 중복되는 정보
duplicate <- infector %>%
  left_join(., infectee, by = 'value', relationship = "many-to-many") %>%
  filter(!is.na(key.y)) %>%
  dplyr::select(value) %>%
  distinct()

#최종적으로 감염된 사람들의 수계산
nterminal_infectees <- infectee %>%
  dplyr::select(value) %>%
  filter(!value %in% duplicate$value) %>%
  transmute(case.no = as.numeric(value)) %>%
  nrow()

complete_offspringd <- enframe(c(offspring$n, rep(0, nterminal_infectees)))






#####################################모형적합###################################
# offspring count 데이터 가져오기
offspring_counts <- complete_offspringd %>% pull(value)

#POI
pfit <- offspring_counts %>% fitdist(., distr = 'pois')

#NB
nbfit <- offspring_counts %>% fitdist(., distr = 'nbinom')

#결과확인
summary(pfit)
summary(nbfit)




################################Figure만들기######################### 

#포아송분포
ggplot() +
  geom_histogram(aes(x = complete_offspringd$value, y = ..density..), 
                 fill = "gray", colour = "black", binwidth = 1, alpha = 0.5) +
  geom_point(aes(x = 0:15, y = dpois(x = 0:15, lambda = pfit$estimate[[1]])), size = 2) +
  stat_smooth(aes(x = 0:15, y = dpois(x = 0:15, lambda = pfit$estimate[[1]])), 
              method = 'lm', formula = y ~ poly(x, 9), se = FALSE, size = 0.8, colour = 'black') +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous("Secondary Cases / Index", expand = c(0, 0), breaks = 0:15) +
  scale_y_continuous("Probability density", limits = c(0, 0.8), expand = c(0, 0)) +
  ggtitle("POI dist") + 
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.7),  
    axis.text = element_text(size = 20),  
    axis.title = element_text(size = 18),  
    aspect.ratio = 1, 
    axis.title.y = element_text(margin = margin(r = 12)), 
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 50, r = 10, b = 10, l = 10),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2),  # 제목 크기 키우고 중앙 정렬
    plot.title.position = "plot" 
  ) 



#음이항분포 
ggplot() +
  geom_histogram(aes(x = complete_offspringd$value, y = ..density..), fill = "gray", colour = "black", binwidth = 1, alpha = 0.5) +
  geom_point(aes(x = 0:15, y = dnbinom(x = 0:15, size = nbfit$estimate[[1]], mu = nbfit$estimate[[2]])), size = 2) +
  stat_smooth(aes(x = 0:15, y = dnbinom(x = 0:15, size = nbfit$estimate[[1]], mu = nbfit$estimate[[2]])), 
              method = 'lm', formula = y ~ poly(x, 9), se = FALSE, size = 0.8, colour = 'black') +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous("Secondary Cases / Index", expand = c(0, 0), breaks = 0:15) +
  scale_y_continuous("Probability density", limits = c(0, 0.8), expand = c(0, 0)) +
  ggtitle("NB dist") + 
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.7),  # 축의 두께 조절
    axis.text = element_text(size = 20),  # 축 텍스트 크기 조절
    axis.title = element_text(size = 18),  # 축 제목 크기 조절
    aspect.ratio = 1, 
    axis.title.y = element_text(margin = margin(r = 12)), 
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 50, r = 10, b = 10, l = 10),  # 상단 여백 조정
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2),  # 제목 크기 키우고 중앙 정렬
    plot.title.position = "plot"  # 제목을 플롯의 맨 위에 배치
  ) 







#그냥 히스토그램 
ggplot() +
  geom_histogram(aes(x = complete_offspringd$value, y = ..density..), fill = "gray", colour = "black", binwidth = 1, alpha = 0.5) +
  # geom_point(aes(x = 0:15, y = dnbinom(x = 0:15, size = nbfit$estimate[[1]], mu = nbfit$estimate[[2]])), size = 2) +
  # stat_smooth(aes(x = 0:15, y = dnbinom(x = 0:15, size = nbfit$estimate[[1]], mu = nbfit$estimate[[2]])), 
  #             method = 'lm', formula = y ~ poly(x, 9), se = FALSE, size = 0.8, colour = 'black') +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous("Secondary Cases / Index", expand = c(0, 0), breaks = 0:15) +
  scale_y_continuous("Probability density", limits = c(0, 0.8), expand = c(0, 0)) +
  ggtitle("NB dist") + 
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.7),  # 축의 두께 조절
    axis.text = element_text(size = 20),  # 축 텍스트 크기 조절
    axis.title = element_text(size = 18),  # 축 제목 크기 조절
    aspect.ratio = 1, 
    axis.title.y = element_text(margin = margin(r = 12)), 
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 50, r = 10, b = 10, l = 10),  # 상단 여백 조정
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2),  # 제목 크기 키우고 중앙 정렬
    plot.title.position = "plot"  # 제목을 플롯의 맨 위에 배치
  ) 


