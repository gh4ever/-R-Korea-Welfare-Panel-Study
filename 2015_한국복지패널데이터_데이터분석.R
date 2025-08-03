library(foreign)
library(haven)
library(dplyr)
library(ggplot2)

data <- read_sav("C:/Users/erika/Downloads/RDataAnalysis/Koweps_hpc10_2015_beta10.sav")
class(data)
head(data)
colnames(data)

data <- rename(data, sex=h10_g3, #성별
                     birth=h10_g4, #태어난 연도
                     marriage=h10_g10, #종교
                     income=p1002_8aq1, #월급
                     code_job=h10_eco9, #직종코드
                     code_region=h10_reg7) #지역코드

# 성별 (1 - 남, 2 - 여, 이상치 없음)

data$sex <- ifelse(data$sex==9, NA)
table(is.na(data$sex)) #이상치 없음 - 16664명 
# 만일 이상치 있다면, 결측치로 처리한 후 확인절차 필요

class(data$sex)
table(data$sex)
data$sex <- factor(data$sex,
                   levels = c(1, 2),
                   labels = c("male", "female"))
qplot(data$sex)

# 월급 (월평균 임금, 만원 단위로 입력)

class(data$income)
summary(data$income)
# median 중간값이 mean 평균보다 낮음 - right skewed; 즉, 전반적으로 임금은 낮은 값에 치우쳐 있음.
# 모름, 무응답이 9999로 코딩되어있어 (무려 12000개 이상) 전처리 필요

qplot(data$income) # right skewed

#결측치 전처리

data$income <- ifelse(data$income %in% c(0, 9999), NA, data$income)

# “소득 없음”도 0으로 기입되며, 이는 의미 있는 결측으로 간주될 수 있음.
# 월급 데이터의 실질적 유의미한 값은 1 이상부터 시작되는 경우가 많음.
# 분석 대상이 실제 월급을 받은 사람들만이라면 0도 제외하는 게 좋음.

sum(data$income == 0, na.rm = TRUE)
sum(data$income == 9999, na.rm = TRUE)

# 소득 없음 (0)과 모름(9999) 등 전처리 완료 

table(data$income)

# 성별 월급 평균표 만들기

sex_income <- data %>% 
              filter(!is.na(income)) %>%
              group_by(sex) %>%
              summarise(mean_income = mean(income))

sex_income
# 평균적으로 남성의 월급이 여성보다 약 150만원 더 많다는 결과가 나옴

# 시각화
ggplot(data = sex_income, aes(x = sex, y = mean_income, fill = sex)) +
  geom_col() +
  labs(title = "성별 평균 월급 비교", x = "성별", y = "평균 월급(만원)") +
  theme_minimal()

# 연령대 및 성별 월급 평균표 만들기


#########

# 나이와 월급의 관계
# 패널데이터에는 나이변수는 없고 출생년도 변수만 있어 여기서 나이를 추출해야 함.

class(data$birth)
summary(data$birth) #최고령 - 1907년생, 최연소 - 2014년생, 모름 또는 무응답 - 9999

qplot(data$birth)

# 나이를 구하려면 조사가 시행된 "2015년 - 출생년도 + 1 = 나이"가 됨.

data$age <- 2015-data$birth + 1
summary(data$age)
qplot(data$age)
table(is.na(data$age)) #Na가 0개, 유효한 데이터(Na가 False인 데이터) 총 16664개로 결측치 없음. 

# 나이에 따른 월급 평균표 만들기

age_income <- data %>% 
              filter(!is.na(income)) %>%
              group_by(age) %>%
              summarise(mean_income = mean(income))
head(age_income)                  

# 시각화 하기

ggplot(data = age_income, aes(x = age, y = mean_income))+geom_line()

# 시각화 결과: 20대 초반 100~200만원대로 받고
# 50대가 가장 높은 평균 300만원대의 월급을 받다가
# 퇴직나이인 60대부터 급격히 떨어져 70대 이상은 20대보다 더 낮은 평균월급을 받는 것으로 나옴.

# 어떤 연령대의 월급이 가장 많을까?

# 우선 연령대 범주는 아래와 같음.
# 초년: 30세 미만 / 중년: 30-60세 미만/ 노년: 60세 이상
# 연령대 만들기

data <- data %>% mutate(ageg = ifelse(age <30, "young",
                ifelse(age < 60, "middle", "old")))
table(data$ageg)

# 연령대 분포 시각화: 초년이 가장 적고, 중년/노년의 인원은 비슷 
qplot(data$ageg)

# 연령대별 월급 평균표 만들기
ageg_income <- data %>%
              filter(!is.na(income)) %>%
              group_by(ageg) %>%
              summarise(mean_income=mean(income))
ageg_income

# 연령대별 월급 평균 시각화: 월급은 중년이 많이 받음.
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + geom_col()+scale_x_discrete(limits=c("young", "middle", "old"))

# 성별 월급 차이가 연령대에 따라 다를까?

# 연령대 및 성별 월급 평균표: 초년생 제외하고 중년, 노년층 성별 월급 차이 약 2배 혹은 2배 이상!!!
sex_income <- data %>%
            filter(!is.na(income)) %>%
            group_by(ageg, sex) %>%
            summarise(mean_income = mean(income))
sex_income

# 연령대, 성별에 따른 평균 월급 차이 시각화
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

# 결과: 앞의 연령대 별 평균 월급 비교와 달리 남성의 경우 노년과 초년의 월급 차이가 그다지 다르지 않음. 
# 반면 여성의 노년, 초년 간 월급 차이는 현저히 큼. 즉, 여성의 경우 나이가 들수록 경제 자립도가 떨어짐.

sex_age <- data %>%
    filter(!is.na(income)) %>%
    group_by(age, sex) %>%
    summarise(mean_income = mean(income))

sex_age
ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex)) + geom_line()

# 라인 그래프에서 여성은 전 연령대에서 평균 월급이 급격하게 올라가지 않음.

mean_income = mean(data$income)
t.test(mean_income ~ sex, data = data)
# not working!!!
