
rm(list=ls())

###########################데이터 불러오기 및 전처리###########################
# 라이브러리 불러오기 
library(readxl)      
library(dplyr)     
library(tidyr)      
library(fitdistrplus) 
library(ggplot2)     
library(tibble) 


# #패키지 설치
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("fitdistrplus")
# install.packages("ggplot2")
# install.packages("tibble")


#전체데이터 불러오기
linelist <- read_excel("백일해데이터.xlsx")


#source of infection, infectee 변수 선택 및 변수명 변경
overall_data <- linelist %>% rename(infector.case = "SOI", infectee.case = "key")





###########################데이터를 count data로 변환 ###########################

# infector.case 별로 감염시킨 사람의 수를 계산
# infector.case가 몇번 등장하는지 
offspring <- overall_data %>%
  dplyr::select(infector.case) %>%
  drop_na() %>%
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

#아무에게도 감염시키지 않은 환자수수 계산 
nterminal_infectees <- infectee %>%
  dplyr::select(value) %>%
  filter(!value %in% duplicate$value) %>%
  transmute(case.no = as.numeric(value)) %>%
  nrow()



offspring_counts <- enframe(c(offspring$n, rep(0, nterminal_infectees))) %>% pull(value)


max(offspring_counts)


###########################음이항 분포 적합 ###########################



#NB 분포 적합 
nbfit <- offspring_counts %>% fitdist(., distr = 'nbinom')

#결과확인
summary(nbfit)




###########################적합 플롯 만들기###########################


#음이항분포 

ggplot() +
  geom_histogram(aes(x = offspring_counts, y = ..density..), 
                 fill = "gray", colour = "black", binwidth = 1, alpha = 0.5) +
  geom_point(aes(x = 0:max(offspring_counts), 
                 y = dnbinom(0:max(offspring_counts), size = nbfit$estimate[[1]], mu = nbfit$estimate[[2]])), 
             size = 2) +
  geom_line(aes(x = 0:max(offspring_counts), 
                y = dnbinom(0:max(offspring_counts), size = nbfit$estimate[[1]], mu = nbfit$estimate[[2]])), 
            size = 0.8, colour = 'black') + 
  expand_limits(x = 0, y = 0) +
  scale_x_continuous("Secondary Cases / Index", expand = c(0, 0), breaks = 0:max(offspring_counts)) +
  scale_y_continuous("Probability density", limits = c(0,1), expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.7),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 18),
    aspect.ratio = 1, 
    axis.title.y = element_text(margin = margin(r = 12)), 
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 50, r = 10, b = 10, l = 10),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2),
    plot.title.position = "plot"
  )  



