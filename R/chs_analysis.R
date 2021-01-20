library(devtools)
library(plyr)

install_github("Sanggeun/chs")
library(chs)

devtools::install_github("Sanggeun/map.choropleth")
devtools::install_github("Sanggeun/g.function.bsg")
library(map.choropleth)
library(g.function.bsg)

library(tidyverse)
library(survey)
library(knitr)
library(RColorBrewer)
library(scales)
library(viridis)

data_path<- "data/"

# 우선 여기에서는 주요 지표만 다룸
# 1. 분석변수 정리 및 데이터 읽기----------;;
tb2 <- tribble(
  ~a, ~b, ~c, ~d,~by_year,~va_class,~josa_year,
  #---------
  "흡연", "현재흡연율","sm_a0100", NULL,TRUE,2, NULL, 
  "흡연","남자현재흡연율","sm_a0100","sex",TRUE,2, NULL, 
  "흡연","금연시도율","sm_d0600","sm_a0100",FALSE,1, NULL, 
  "흡연","현재 비흡연자의 직장실내간접흡연노출률","sm_c0800","sm_e0200",FALSE,2,c(2011, 2013:2019),
  "음주","월간음주율","dr_a0400",NULL,TRUE,2, NULL, 
  "음주","고위험음주율","dr_a0500",NULL,TRUE,2, NULL, 
  "안전의식","운전시 안전벨트 착용률","sf_a0100","sfa_01z1",TRUE,1, NULL, 
  "안전의식","동승자 안전벨트 착용률","sf_a0400","sf_a0300",TRUE,1, NULL, 
  "안전의식", "음주운전 경험률", "sf_b0500","sf_b0400",FALSE,2, NULL, 
  "운동 및 신체활동","중등도이상 신체활동 실천율","ph_a0500",NULL,TRUE,1, NULL, 
  "운동 및 신체활동", "걷기 실천율", "ph_b0200", NULL,TRUE,1, NULL, 
  "비만 및 체중조절", "비만율", "ob_a0203", NULL,TRUE,2, NULL, 
  "비만 및 체중조절", "체중조절 시도율", "ob_b0100", NULL,TRUE,1,  NULL, 
  "비만 및 체중조절", "주관적 비만 인지율", "ob_a0300", NULL,TRUE,2,  NULL, 
  "식생활", "저염선호율(type1)", "nu_b0400", NULL,TRUE,1,  NULL, 
  "식생활", "저염선호율(type2)", "nu_b0500", NULL,TRUE,1,  NULL, 
  "식생활", "저염선호율(type3)", "nu_b0600", NULL,TRUE,1,  NULL, 
  "식생활", "영양표시독해율","nu_c0300",NULL,FALSE,1,2014:2016,
  "식생활", "5일 이상 아침식사 실천율","nu_a0204",NULL,TRUE,1, NULL, 
  "구강건강", "저작불편 호소율", "or_b0100", "age65",FALSE,2, NULL, 
  "구강건강", "구강검진 수진율", "or_e0500", NULL,TRUE,1, NULL, 
  "구강건강", "점심식사후 칫솔질 실천율", "or_d0100", "ord1",TRUE,1, NULL, 
  "정신건강", "스트레스 인지율", "mt_a0100", NULL,TRUE,2, NULL, 
  "정신건강", "우울감 경험률", "mt_b0100", NULL,TRUE,2, NULL, 
  "예방접종", "인플루엔자 예방접종률", "sc_a0100", NULL,TRUE,1, NULL, 
  "이환_고혈압", "혈압인지율","il_a1900", NULL,TRUE,1, NULL, 
  "이환_고혈압","고혈압 의사진단경험률(30세이상)","il_a0200","age30",TRUE,2, NULL, 
  "이환_고혈압","고혈압 약물치료율(30세이상)","il_a0500",c("age30","il_a0200"),FALSE,1, NULL, 
  "이환_당뇨병","혈당인지율","il_b1900",NULL,TRUE,1, NULL, 
  "이환_당뇨병","당뇨병 의사진단경험률(30세이상)","il_b0200","age30",TRUE,2, NULL, 
  "이환_당뇨병","당뇨병 치료율(30세이상)","il_b0600",c("age30","il_b0200"),FALSE,1, NULL, 
  "이환_당뇨병","당뇨병 안질환 합병증검사 수진율(30세이상)","il_b0900",c("age30","il_b0200"),FALSE,1, NULL, 
  "이환_당뇨병","당뇨병 신장질환 합병증검사 수진율(30세이상)","il_b1000",c("age30", "il_b0200"),FALSE,1, NULL, 
  "이환_이상지질혈증","이상지질혈증 의사진단경험률(30세이상)","il_r0100","age30",TRUE,2, NULL, 
  "이환_관절염","관절염 의사진단경험률(50세이상)","il_g0100","age50",FALSE,2, NULL, 
  "의료이용","필요의료서비스 미치료율","sr_a0100",NULL,TRUE,2,2012:2019,
  "활동제한 및 삶의 질","양호한 주관적건강 인지율","ql_a0100",NULL,TRUE,1, NULL, 
  "보건기관 이용","보건기관 이용률","ct_a0100",NULL,TRUE,1, NULL 
)

# 영역별 변수
com_va <- c('josa_year','CITY_CD','BOGUN_CD','dong_p','JIJUM_CD','dong_type',
            'house_type','wt_house','wt','sod_02z2')
indi_va <- c("age","age_10","sex","gender", "income_3", "educ","job","generation","town")

smoking <- c("sm_a0100","sm_a0200","sm_a0300","sm_d0600","sex_m", "sm_c0800","sm_e0200")
dringking <- c("dr_a0400","dr_a0500")
safety <- c("sf_a0100","sf_a0400","sf_b0500","sf_b0400","sfa_01z1","sf_a0300")
physical_act <- c("ph_a0500","ph_b0200", "ph_a1009", "ph_a1100", "ph_c0100", 
                  "pha_aerobic", "ph_a0500_2", "ph_a0500_3")
obesity <- c("ob_a0203","ob_b0100","ob_a0300")
diet <- c("nu_b0400","nu_b0500","nu_b0600","nu_c0300","nu_a0204")
dental_health <- c("or_b0100","or_e0500","or_d0100","ord_01d2","or_d0050","age65")
mental_health <- c("mt_a0100","mt_b0100")
vaccination <- "sc_a0100"
illness <- c("il_a1900","il_a0200","il_a0500","il_b1900","il_b0200","il_b0600","il_b0900","il_b1000",
             "il_r0100","il_g0100","age30","age50","age65")
med_use <- "sr_a0100"
qol <- "ql_a0100"
pub_use <- "ct_a0100"
socio_envir <- c("en_a0101", "en_a0102", "en_a0103", "en_a0104", "en_a0105", "en_a0106",
                 "en_b0401", "en_b0402", "en_b0403", "en_b0404")

analy_va <- c(smoking, dringking, safety, physical_act, obesity, diet, dental_health, mental_health, vaccination, 
              illness, med_use, qol, pub_use, socio_envir)

rm(list = c("smoking", "dringking", "safety", "physical_act", "obesity", "diet", "dental_health", "mental_health", "vaccination",
            "illness", "med_use", "qol", "pub_use", "socio_envir"))

# 데이터 읽기 --------------------------------------------------------
bogun_tb <- data.frame(name = c("남구","달서구","달성군","동구","북구","서구","수성구","중구"), 
                       num = 042:049)
year_tb <- data.frame(yr = 2011:2019, data3 = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE))

#
dt_2019_042 <- data_coding(dataread_chs_raw(year_tb[9, "yr"], 
                                            paste0(data_path, year_tb[9, "yr"],
                                                   "/개인_대구광역시_", 
                                                   bogun_tb[bogun_tb$num == 042, "name"], "보건소.xlsx"), 
                                            data3 = year_tb[9, "data3"]),
                           year_tb[9, "yr"])

dt_2018_042 <- data_coding(dataread_chs_raw(year_tb[8, "yr"], 
                                            paste0(data_path, year_tb[8, "yr"],
                                                   "/개인_대구광역시_", 
                                                   bogun_tb[bogun_tb$num == 042, "name"], "보건소.xlsx"), 
                                            data3 = year_tb[8, "data3"]),
                           year_tb[8, "yr"])


#dt_2015 <- bind_rows(dt_2015_070[,c(com_va, indi_va, analy_va)],
#                     dt_2015_071[,c(com_va, indi_va, analy_va)])


dt_dg_2011_2019 <- NULL
for (k in 1:9) {
  for (i in 042:049) {
    dt_i <- data_coding(dataread_chs_raw(year_tb[k, "yr"], 
                                         paste0(data_path, year_tb[k, "yr"],
                                                "/개인_대구광역시_", 
                                                bogun_tb[bogun_tb$num == i, "name"], "보건소.xlsx"), 
                                         data3 = year_tb[k, "data3"]),
                        year_tb[k, "yr"])
    
    dt_i <- dt_i[, c(com_va, indi_va, analy_va)]
    
    dt_dg_2011_2019 <- bind_rows(dt_dg_2011_2019, dt_i)
  }
}

dt_dg_2011_2019 <- code_district_daegu(dt_dg_2011_2019)

dt_dg_2011_2019$ord1 = ifelse(dt_dg_2011_2019$ord_01d2 %in% c(1,2), 1, 0)

# 2. 자료 정리
# 2.1. 지도만들기 --------------------------------######################################################################################################
#---------##################################

library(rgdal) # requires sp, will use proj.4 if installed
#install.packages("gpclib")
library(maptools)

map_path <- "data/map_2014/"

## 동별지도

map_path_dong <- paste0(map_path, "bnd_dong_00_2014")

map_dong <- readOGR(dsn=paste0(map_path_dong,"/bnd_dong_00_2014.shp"))
# 동이름 한글 encoding 변경
map_dong@data$adm_dr_nm <- iconv(map_dong@data$adm_dr_nm, "CP949", "UTF-8")
map_dong_dg <- map_dong[substr(map_dong$adm_dr_cd,1,2)=="22",] 
map_dong_dg@data$adm_dr_nm <- trim(underbar_change(map_dong_dg@data$adm_dr_nm))

## 권역별 지도
# 동별 지도자료에 권역 표기
map_dong_dg@data <- code_district_daegu(map_dong_dg@data, var_district = "district", var_dong = "adm_dr_nm")

# 권역별 지도 만들기
library(gpclib)
gpclibPermit() 
map_district_dg <- unionSpatialPolygons(map_dong_dg, IDs = map_dong_dg$district)
map_dong_dg@data$sigungu_cd <- substr(map_dong_dg@data$adm_dr_cd,3,5)
map_district_dg_df <- map_dong_dg@data[!duplicated(map_dong_dg@data$district),
                                       c("base_year","district","sigungu_cd")]
row.names(map_district_dg_df) <- map_district_dg_df$district
map_district_dg <- SpatialPolygonsDataFrame(map_district_dg, map_district_dg_df)

# 보건소 지도
map_bogun <- readRDS("data/bogun_2014.rds")
map_bogun$bogun_nm <- iconv(map_bogun$bogun_nm, "CP949", "UTF-8")
map_bogun_dg <- map_bogun[map_bogun$bogun_cd %in% unique(dt_dg_2011_2019$BOGUN_CD),]

# 2.1 인구자료 정리  

# 2.1.1 인구자료 받기--------------------------------########################################################################################


devtools::install_github("Sanggeun/pop.data")
library(pop.data)

d_po <- "./data/pop/"

p <- list()
for (i in 2010:2019){
  if (i %in% 2011:2019) {
    if (i %in% 2011:2017){
      pop  <- read_csv(paste0(d_po,'101_DT_1B04005N_Y_',i,'.csv'))
    } 
    if (i %in% 2018:2019) {
      pop  <- read_csv(paste0(d_po,'101_DT_1B04005N_Y_',i,'.csv'), locale = locale(encoding = "euc-kr")) 
    }
    
  }  else {
    pop  <- read_csv(paste0(d_po,'101_DT_1B04005_Y_',i,'.csv'))
    
  }
  colnames(pop) <- c("d_num", "d", "age_num", "age", "year", "pop_all", "pop_male", "pop_female")
  # "."을 "_"로 변환
  pop$d <- underbar_change(pop$d)
  p[[paste0("p_",i)]] <- pop
}


## 2.1.2 권역별 자료정리------------------------------------------------------------------
## 권역 변수 코딩
p[[1]] <- dg_district_code(p[[1]])
p[[2]] <- dg_district_code(p[[2]])
p[[3]] <- dg_district_code(p[[3]])
p[[4]] <- dg_district_code(p[[4]])
p[[5]] <- dg_district_code(p[[5]])
p[[6]] <- dg_district_code(p[[6]])
p[[7]] <- dg_district_code(p[[7]])
p[[8]] <- dg_district_code(p[[8]])
p[[9]] <- dg_district_code(p[[9]])
p[[10]] <- dg_district_code(p[[10]])

# 2.3 연도별 지역명 정리

# 2.3.1. 인구자료
#    동이름: 지도(2014년)의 이름에 맞추어 정리
# 지도 동이름
dong_name = map_dong_dg$adm_dr_nm
district_name = map_district_dg$district
bogun_name = map_bogun_dg$bogun_nm

# 2.3.1.1 대구 인구자료 동이름 정리
p_dg <- list()
for (i in 1:10) {
  p_dg[[i]] <- p[[i]][substr(p[[i]]$d_num,2,3) == 27, ]
}

# 2010
p_dg[[1]][p_dg[[1]][["d"]] %in% c("동인1_2_4가동", "동인3가동"), ]$d = "동인동"
p_dg[[1]][p_dg[[1]][["d"]] %in% c("대현1동", "대현2동"), ]$d = "대현동"
p_dg[[1]][p_dg[[1]][["d"]] %in% c("성당1동", "성당2동"), ]$d = "성당동"
p_dg[[1]][p_dg[[1]][["d"]] %in% c("두류1동", "두류2동"), ]$d = "두류1_2동"

# 논공읍공단출장소, 다사읍서재출장소(2012 - 2019)
for (i in 3:10) {
  p_dg[[i]][p_dg[[i]][["d"]] %in% c("논공읍공단출장소"), ]$d = "논공읍"
  p_dg[[i]][p_dg[[i]][["d"]] %in% c("다사읍서재출장소"), ]$d = "논공읍"
}

# 현풍읍, 옥포읍, 유가읍 ==> 현풍면, 옥포면, 유가면(2018 - 2019)
for (i in 9:10) {
  p_dg[[i]][p_dg[[i]][["d"]] %in% c("현풍읍"), ]$d = "현풍면"
  p_dg[[i]][p_dg[[i]][["d"]] %in% c("옥포읍"), ]$d = "옥포면"
  p_dg[[i]][p_dg[[i]][["d"]] %in% c("유가읍"), ]$d = "유가면"
}

## 2.3.1.2 동별로 합치기
for (i in 1:10) {
  p_dg[[i]] = p_dg[[i]] %>%
    group_by(d, age) %>%
    summarise(pop_all = sum(pop_all), pop_male = sum(pop_male), pop_female = sum(pop_female)) 
}

## 2.3.1.3 권역별 인구자료
p_dg_d <- list()
for (i in 1:10) {
  p_dg_d[[i]] <- p[[i]][substr(p[[i]]$d_num,2,3) == 27, ]
}

## 2.3.1.4 권역별로 합치기
for (i in 1:10) {
  p_dg_d[[i]] = p_dg_d[[i]] %>%
    group_by(district, age) %>%
    summarise(pop_all = sum(pop_all), pop_male = sum(pop_male), pop_female = sum(pop_female)) 
}


# 2.3.2 CHS 자료(자료 코딩에서 정리되어 있음)
# unique(dt_dg_2011_2019$dong_p)


# 3. 자료 분석 -------------------------------------------------------------------------------#######################################################################

## 3.1 조율구하기 
crude_tb = chs_c_rate(dt_dg_2011_2019[dt_dg_2011_2019$dong_p == "동인동",], de_var = tb2[["c"]][2] , 
                      index_subset = tb2[["d"]][[2]], josa_year = 2015:2019)


## 3.2 표준화비 구하기

## 3.2.1 표준인구기준(대구시) 지역의 구간별(인구, 성별) 특수율 구하기   --------------------------

stan_pop_rate <- chs_by_age_sex(dt_dg_2011_2019[dt_dg_2011_2019$josa_year %in% 2015:2019,], 
                                de_var= tb2[["c"]][2], var_name = tb2[["b"]][2])

## 3.2.2 대상지역의 성별 연령별 인구구하기

devtools::install_github("Sanggeun/standardization")
library(standardization)
pop_by = (fix_age_class_tochs(p_dg[[5]][p_dg[[5]]$d=="동인동",], class_var = "age", josa_year = 2014) +
            fix_age_class_tochs(p_dg[[6]][p_dg[[6]]$d=="동인동",], class_var = "age", josa_year = 2015)*2 +
            fix_age_class_tochs(p_dg[[7]][p_dg[[7]]$d=="동인동",], class_var = "age", josa_year = 2016)*2 +
            fix_age_class_tochs(p_dg[[8]][p_dg[[8]]$d=="동인동",], class_var = "age", josa_year = 2017)*2 +
            fix_age_class_tochs(p_dg[[9]][p_dg[[9]]$d=="동인동",], class_var = "age", josa_year = 2018)*2 +
            fix_age_class_tochs(p_dg[[10]][p_dg[[10]]$d=="동인동",], class_var = "age", josa_year = 2019)) /10

## 3.2.3 표준화비 구하기 (직접사망자수/간접사망자수)
## 3.2.3.1 직접 사망자수
direct_n = as.numeric(crude_tb[4]) * sum(pop_by) / 100
direct_n_uci = as.numeric(crude_tb[5]) * sum(pop_by) / 100
direct_n_lci = as.numeric(crude_tb[6]) * sum(pop_by) / 100

## 3.2.3.2 간접 사망자수
indirect_n = sum(stan_pop_rate * pop_by)

## 3.2.3.3 간접표준화비
direct_n/indirect_n 
direct_n_uci/indirect_n 
direct_n_lci/indirect_n 

## 3.3 대구시 지역별 결과
### 연도별 데이터셋
dt_1 = dt_dg_2011_2019[dt_dg_2011_2019$josa_year %in% 2011:2014, ]
dt_2 = dt_dg_2011_2019[dt_dg_2011_2019$josa_year %in% 2015:2018, ]
dt_3 = dt_dg_2011_2019[dt_dg_2011_2019$josa_year %in% 2019, ]
dt_4 = dt_dg_2011_2019[dt_dg_2011_2019$josa_year %in% 2015:2019, ]

## 3.3.1 동별 결과

### 3.3.1.1 2011-2014
dong_list1 = list()
for (j in 1:NROW(tb2)) {
  re1= chs_c_rate_by(dt_1, by_var = "dong_p", de_var = tb2[["c"]][j], 
                     index_subset = tb2[["d"]][[j]], josa_year = 2011:2014)
  # identical(sort(unique(dong_name)), sort(unique(re1[,"By"])))
  
  dong_name = re1[,"By"]
  
  stan_rate_dg <- chs_by_age_sex(dt_1[dt_1$josa_year %in% 2011:2014,], 
                                 de_var= tb2[["c"]][j], var_name = tb2[["b"]][j])
  
  re_tb = matrix(nrow = length(dong_name), ncol = 9)
  colnames(re_tb) = c(colnames(re1), "stan_ratio", "stan_ratio_lower", "stan_ratio_upper")
  re_tb[,1:6] <- re1
  for (i in 1:length(dong_name)){
    pop_by = (fix_age_class_tochs(p_dg[[1]][p_dg[[1]]$d == dong_name[i],], class_var = "age", josa_year = 2010) +
                fix_age_class_tochs(p_dg[[2]][p_dg[[2]]$d == dong_name[i],], class_var = "age", josa_year = 2011)*2 +
                fix_age_class_tochs(p_dg[[3]][p_dg[[3]]$d == dong_name[i],], class_var = "age", josa_year = 2012)*2 +
                fix_age_class_tochs(p_dg[[4]][p_dg[[4]]$d == dong_name[i],], class_var = "age", josa_year = 2013)*2 +
                fix_age_class_tochs(p_dg[[5]][p_dg[[5]]$d == dong_name[i],], class_var = "age", josa_year = 2014))/8
    
    ## 지표별 분모 지정
    if (tb2[["b"]][j] %in% c("고혈압 의사진단경험률(30세이상)", "당뇨병 의사진단경험률(30세이상)",
                             "이상지질혈증 의사진단경험률(30세이상)")) {
      pop_by = pop_by[-c(1,2),]
    } else if (tb2[["b"]][j] == "관절염 의사진단경험률(50세이상)") { 
      pop_by <- pop_by[-c(1:6),]
    } else if (tb2[["b"]][j] == "남자현재흡연율") {
      pop_by <- pop_by[c(1,3,5,7,9,11),]
    } else if (tb2[["b"]][j] == "저작불편 호소율") {
      pop_by <- pop_by[c(9,10,11,12),]
    } else if (tb2[["b"]][j] == "고혈압 약물치료율(30세이상)") {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_1, de_var= "il_a0200", var_name = "고혈압 의사진단경험률(30세이상)")
    } else if (tb2[["b"]][j] %in% c("당뇨병 치료율(30세이상)","당뇨병 안질환 합병증검사 수진율(30세이상)","당뇨병 신장질환 합병증검사 수진율(30세이상)" )) {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_1, de_var= "il_b0200", var_name = "당뇨병 의사진단경험률(30세이상)") 
    } else if (tb2[["b"]][j] == "금연시도율") {
      pop_by = pop_by * chs_by_age_sex(dt_1, de_var = "sm_a0100", var_name = "현재흡연율") 
    } 
    
    
    
    ## 표준화비 구하기 (직접사망자수/간접사망자수)
    ## 직접 사망자수
    direct_n = as.numeric(re1[i, 4]) * sum(pop_by) / 100
    direct_n_lci = as.numeric(re1[i, 5]) * sum(pop_by) / 100
    direct_n_uci = as.numeric(re1[i, 6]) * sum(pop_by) / 100
    
    ## 간접 사망자수
    indirect_n = sum(stan_rate_dg * pop_by)
    
    ## 간접표준화비
    stan_ratio = direct_n/indirect_n 
    stan_ratio_l = direct_n_lci/indirect_n 
    stan_ratio_u = direct_n_uci/indirect_n 
    
    re_tb[i, 7] = round_new(stan_ratio, 2)
    re_tb[i, 8] = round_new(stan_ratio_l, 2)
    re_tb[i, 9] = round_new(stan_ratio_u, 2)
    dong_list1[[j]] <- re_tb
  }
}

### 3.3.1.2 2015-2018
dong_list2 = list()
for (j in 1:NROW(tb2)) {
  re1= chs_c_rate_by(dt_2, by_var = "dong_p", de_var = tb2[["c"]][j], 
                     index_subset = tb2[["d"]][[j]], josa_year = 2015:2018)
  # identical(sort(unique(dong_name)), sort(unique(re1[,"By"])))
  
  dong_name = re1[,"By"]
  
  stan_rate_dg <- chs_by_age_sex(dt_2[dt_2$josa_year %in% 2015:2018,], 
                                 de_var= tb2[["c"]][j], var_name = tb2[["b"]][j])
  
  re_tb = matrix(nrow = length(dong_name), ncol = 9)
  colnames(re_tb) = c(colnames(re1), "stan_ratio", "stan_ratio_lower", "stan_ratio_upper")
  re_tb[,1:6] <- re1
  for (i in 1:length(dong_name)){
    pop_by = (fix_age_class_tochs(p_dg[[5]][p_dg[[5]]$d == dong_name[i],], class_var = "age", josa_year = 2014) +
                fix_age_class_tochs(p_dg[[6]][p_dg[[6]]$d == dong_name[i],], class_var = "age", josa_year = 2015)*2 +
                fix_age_class_tochs(p_dg[[7]][p_dg[[7]]$d == dong_name[i],], class_var = "age", josa_year = 2016)*2 +
                fix_age_class_tochs(p_dg[[8]][p_dg[[8]]$d == dong_name[i],], class_var = "age", josa_year = 2017)*2 +
                fix_age_class_tochs(p_dg[[9]][p_dg[[9]]$d == dong_name[i],], class_var = "age", josa_year = 2018))/8
    ## 지표별 분모 지정
    if (tb2[["b"]][j] %in% c("고혈압 의사진단경험률(30세이상)", "당뇨병 의사진단경험률(30세이상)",
                             "이상지질혈증 의사진단경험률(30세이상)")) {
      pop_by = pop_by[-c(1,2),]
    } else if (tb2[["b"]][j] == "관절염 의사진단경험률(50세이상)") { 
      pop_by <- pop_by[-c(1:6),]
    } else if (tb2[["b"]][j] == "남자현재흡연율") {
      pop_by <- pop_by[c(1,3,5,7,9,11),]
    } else if (tb2[["b"]][j] == "저작불편 호소율") {
      pop_by <- pop_by[c(9,10,11,12),]
    } else if (tb2[["b"]][j] == "고혈압 약물치료율(30세이상)") {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_2, de_var= "il_a0200", var_name = "고혈압 의사진단경험률(30세이상)")
    } else if (tb2[["b"]][j] %in% c("당뇨병 치료율(30세이상)","당뇨병 안질환 합병증검사 수진율(30세이상)","당뇨병 신장질환 합병증검사 수진율(30세이상)" )) {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_2, de_var= "il_b0200", var_name = "당뇨병 의사진단경험률(30세이상)") 
    } else if (tb2[["b"]][j] == "금연시도율") {
      pop_by = pop_by * chs_by_age_sex(dt_2, de_var = "sm_a0100", var_name = "현재흡연율") 
    } 
    
    ## 표준화비 구하기 (직접사망자수/간접사망자수)
    ## 직접 사망자수
    direct_n = as.numeric(re1[i, 4]) * sum(pop_by) / 100
    direct_n_lci = as.numeric(re1[i, 5]) * sum(pop_by) / 100
    direct_n_uci = as.numeric(re1[i, 6]) * sum(pop_by) / 100
    
    ## 간접 사망자수
    indirect_n = sum(stan_rate_dg * pop_by)
    
    ## 간접표준화비
    stan_ratio = direct_n/indirect_n 
    stan_ratio_l = direct_n_lci/indirect_n 
    stan_ratio_u = direct_n_uci/indirect_n 
    
    re_tb[i, 7] = round_new(stan_ratio, 2)
    re_tb[i, 8] = round_new(stan_ratio_l, 2)
    re_tb[i, 9] = round_new(stan_ratio_u, 2)
    dong_list2[[j]] <- re_tb
  }
  
}

### 3.3.1.3 

### 3.3.1.4 2015-2019
dong_list3 = list()
for (j in 1:NROW(tb2)) {
  re1= chs_c_rate_by(dt_4, by_var = "dong_p", de_var = tb2[["c"]][j], 
                     index_subset = tb2[["d"]][[j]], josa_year = 2015:2019)
  # identical(sort(unique(dong_name)), sort(unique(re1[,"By"])))
  
  dong_name = re1[,"By"]
  
  stan_rate_dg <- chs_by_age_sex(dt_4[dt_4$josa_year %in% 2015:2019,], 
                                 de_var= tb2[["c"]][j], var_name = tb2[["b"]][j])
  
  re_tb = matrix(nrow = length(dong_name), ncol = 9)
  colnames(re_tb) = c(colnames(re1), "stan_ratio", "stan_ratio_lower", "stan_ratio_upper")
  re_tb[,1:6] <- re1
  for (i in 1:length(dong_name)){
    pop_by = (fix_age_class_tochs(p_dg[[5]][p_dg[[5]]$d == dong_name[i],], class_var = "age", josa_year = 2014) +
                fix_age_class_tochs(p_dg[[6]][p_dg[[6]]$d == dong_name[i],], class_var = "age", josa_year = 2015)*2 +
                fix_age_class_tochs(p_dg[[7]][p_dg[[7]]$d == dong_name[i],], class_var = "age", josa_year = 2016)*2 +
                fix_age_class_tochs(p_dg[[8]][p_dg[[8]]$d == dong_name[i],], class_var = "age", josa_year = 2017)*2 +
                fix_age_class_tochs(p_dg[[9]][p_dg[[9]]$d == dong_name[i],], class_var = "age", josa_year = 2018)*2 +
                fix_age_class_tochs(p_dg[[10]][p_dg[[10]]$d == dong_name[i],], class_var = "age", josa_year = 2019))/10
    ## 지표별 분모 지정
    if (tb2[["b"]][j] %in% c("고혈압 의사진단경험률(30세이상)", "당뇨병 의사진단경험률(30세이상)",
                             "이상지질혈증 의사진단경험률(30세이상)")) {
      pop_by = pop_by[-c(1,2),]
    } else if (tb2[["b"]][j] == "관절염 의사진단경험률(50세이상)") { 
      pop_by <- pop_by[-c(1:6),]
    } else if (tb2[["b"]][j] == "남자현재흡연율") {
      pop_by <- pop_by[c(1,3,5,7,9,11),]
    } else if (tb2[["b"]][j] == "저작불편 호소율") {
      pop_by <- pop_by[c(9,10,11,12),]
    } else if (tb2[["b"]][j] == "고혈압 약물치료율(30세이상)") {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_4, de_var= "il_a0200", var_name = "고혈압 의사진단경험률(30세이상)")
    } else if (tb2[["b"]][j] %in% c("당뇨병 치료율(30세이상)","당뇨병 안질환 합병증검사 수진율(30세이상)","당뇨병 신장질환 합병증검사 수진율(30세이상)" )) {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_4, de_var= "il_b0200", var_name = "당뇨병 의사진단경험률(30세이상)") 
    } else if (tb2[["b"]][j] == "금연시도율") {
      pop_by = pop_by * chs_by_age_sex(dt_4, de_var = "sm_a0100", var_name = "현재흡연율") 
    } 
    
    ## 표준화비 구하기 (직접사망자수/간접사망자수)
    ## 직접 사망자수
    direct_n = as.numeric(re1[i, 4]) * sum(pop_by) / 100
    direct_n_lci = as.numeric(re1[i, 5]) * sum(pop_by) / 100
    direct_n_uci = as.numeric(re1[i, 6]) * sum(pop_by) / 100
    
    ## 간접 사망자수
    indirect_n = sum(stan_rate_dg * pop_by)
    
    ## 간접표준화비
    stan_ratio = direct_n/indirect_n 
    stan_ratio_l = direct_n_lci/indirect_n 
    stan_ratio_u = direct_n_uci/indirect_n 
    
    re_tb[i, 7] = round_new(stan_ratio, 2)
    re_tb[i, 8] = round_new(stan_ratio_l, 2)
    re_tb[i, 9] = round_new(stan_ratio_u, 2)
    dong_list3[[j]] <- re_tb
  }
  
}

# 3.3.2 권역별 결과

## 3.3.2.1 2011-2014
d_list1 = list()
for (j in 1:NROW(tb2)) {
  re1= chs_c_rate_by(dt_1, by_var = "district", de_var = tb2[["c"]][j], 
                     index_subset = tb2[["d"]][[j]], josa_year = 2011:2014)
  # identical(sort(unique(dong_name)), sort(unique(re1[,"By"])))
  
  district_name = re1[,"By"]
  
  stan_rate_dg <- chs_by_age_sex(dt_1[dt_1$josa_year %in% 2011:2014,], 
                                 de_var= tb2[["c"]][j], var_name = tb2[["b"]][j])
  
  re_tb = matrix(nrow = length(district_name), ncol = 9)
  colnames(re_tb) = c(colnames(re1), "stan_ratio", "stan_ratio_lower", "stan_ratio_upper")
  re_tb[,1:6] <- re1
  for (i in 1:length(district_name)){
    pop_by = (fix_age_class_tochs(p_dg_d[[1]][p_dg_d[[1]]$district == district_name[i],], class_var = "age", josa_year = 2010) +
                fix_age_class_tochs(p_dg_d[[2]][p_dg_d[[2]]$district == district_name[i],], class_var = "age", josa_year = 2011)*2 +
                fix_age_class_tochs(p_dg_d[[3]][p_dg_d[[3]]$district == district_name[i],], class_var = "age", josa_year = 2012)*2 +
                fix_age_class_tochs(p_dg_d[[4]][p_dg_d[[4]]$district == district_name[i],], class_var = "age", josa_year = 2013)*2 +
                fix_age_class_tochs(p_dg_d[[5]][p_dg_d[[5]]$district == district_name[i],], class_var = "age", josa_year = 2014))/8
    
    ## 지표별 분모 지정
    if (tb2[["b"]][j] %in% c("고혈압 의사진단경험률(30세이상)", "당뇨병 의사진단경험률(30세이상)",
                             "이상지질혈증 의사진단경험률(30세이상)")) {
      pop_by = pop_by[-c(1,2),]
    } else if (tb2[["b"]][j] == "관절염 의사진단경험률(50세이상)") { 
      pop_by <- pop_by[-c(1:6),]
    } else if (tb2[["b"]][j] == "남자현재흡연율") {
      pop_by <- pop_by[c(1,3,5,7,9,11),]
    } else if (tb2[["b"]][j] == "저작불편 호소율") {
      pop_by <- pop_by[c(9,10,11,12),]
    } else if (tb2[["b"]][j] == "고혈압 약물치료율(30세이상)") {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_1, de_var= "il_a0200", var_name = "고혈압 의사진단경험률(30세이상)")
    } else if (tb2[["b"]][j] %in% c("당뇨병 치료율(30세이상)","당뇨병 안질환 합병증검사 수진율(30세이상)","당뇨병 신장질환 합병증검사 수진율(30세이상)" )) {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_1, de_var= "il_b0200", var_name = "당뇨병 의사진단경험률(30세이상)") 
    } else if (tb2[["b"]][j] == "금연시도율") {
      pop_by = pop_by * chs_by_age_sex(dt_1, de_var = "sm_a0100", var_name = "현재흡연율") 
    } 
    
    
    
    ## 표준화비 구하기 (직접사망자수/간접사망자수)
    ## 직접 사망자수
    direct_n = as.numeric(re1[i, 4]) * sum(pop_by) / 100
    direct_n_lci = as.numeric(re1[i, 5]) * sum(pop_by) / 100
    direct_n_uci = as.numeric(re1[i, 6]) * sum(pop_by) / 100
    
    ## 간접 사망자수
    indirect_n = sum(stan_rate_dg * pop_by)
    
    ## 간접표준화비
    stan_ratio = direct_n/indirect_n 
    stan_ratio_l = direct_n_lci/indirect_n 
    stan_ratio_u = direct_n_uci/indirect_n 
    
    re_tb[i, 7] = round_new(stan_ratio, 2)
    re_tb[i, 8] = round_new(stan_ratio_l, 2)
    re_tb[i, 9] = round_new(stan_ratio_u, 2)
    d_list1[[j]] <- re_tb
  }
}

## 3.3.2.2 2015-2018
d_list2 = list()
for (j in 1:NROW(tb2)) {
  re1= chs_c_rate_by(dt_2, by_var = "district", de_var = tb2[["c"]][j], 
                     index_subset = tb2[["d"]][[j]], josa_year = 2015:2018)
  # identical(sort(unique(dong_name)), sort(unique(re1[,"By"])))
  
  district_name = re1[,"By"]
  
  stan_rate_dg <- chs_by_age_sex(dt_2[dt_2$josa_year %in% 2015:2018,], 
                                 de_var= tb2[["c"]][j], var_name = tb2[["b"]][j])
  
  re_tb = matrix(nrow = length(district_name), ncol = 9)
  colnames(re_tb) = c(colnames(re1), "stan_ratio", "stan_ratio_lower", "stan_ratio_upper")
  re_tb[,1:6] <- re1
  for (i in 1:length(district_name)){
    pop_by = (fix_age_class_tochs(p_dg_d[[5]][p_dg_d[[5]]$district == district_name[i],], class_var = "age", josa_year = 2014) +
                fix_age_class_tochs(p_dg_d[[6]][p_dg_d[[6]]$district == district_name[i],], class_var = "age", josa_year = 2015)*2 +
                fix_age_class_tochs(p_dg_d[[7]][p_dg_d[[7]]$district == district_name[i],], class_var = "age", josa_year = 2016)*2 +
                fix_age_class_tochs(p_dg_d[[8]][p_dg_d[[8]]$district == district_name[i],], class_var = "age", josa_year = 2017)*2 +
                fix_age_class_tochs(p_dg_d[[9]][p_dg_d[[9]]$district == district_name[i],], class_var = "age", josa_year = 2018))/8
    
    ## 지표별 분모 지정
    if (tb2[["b"]][j] %in% c("고혈압 의사진단경험률(30세이상)", "당뇨병 의사진단경험률(30세이상)",
                             "이상지질혈증 의사진단경험률(30세이상)")) {
      pop_by = pop_by[-c(1,2),]
    } else if (tb2[["b"]][j] == "관절염 의사진단경험률(50세이상)") { 
      pop_by <- pop_by[-c(1:6),]
    } else if (tb2[["b"]][j] == "남자현재흡연율") {
      pop_by <- pop_by[c(1,3,5,7,9,11),]
    } else if (tb2[["b"]][j] == "저작불편 호소율") {
      pop_by <- pop_by[c(9,10,11,12),]
    } else if (tb2[["b"]][j] == "고혈압 약물치료율(30세이상)") {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_2, de_var= "il_a0200", var_name = "고혈압 의사진단경험률(30세이상)")
    } else if (tb2[["b"]][j] %in% c("당뇨병 치료율(30세이상)","당뇨병 안질환 합병증검사 수진율(30세이상)","당뇨병 신장질환 합병증검사 수진율(30세이상)" )) {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_2, de_var= "il_b0200", var_name = "당뇨병 의사진단경험률(30세이상)") 
    } else if (tb2[["b"]][j] == "금연시도율") {
      pop_by = pop_by * chs_by_age_sex(dt_2, de_var = "sm_a0100", var_name = "현재흡연율") 
    } 
    
    
    
    ## 표준화비 구하기 (직접사망자수/간접사망자수)
    ## 직접 사망자수
    direct_n = as.numeric(re1[i, 4]) * sum(pop_by) / 100
    direct_n_lci = as.numeric(re1[i, 5]) * sum(pop_by) / 100
    direct_n_uci = as.numeric(re1[i, 6]) * sum(pop_by) / 100
    
    ## 간접 사망자수
    indirect_n = sum(stan_rate_dg * pop_by)
    
    ## 간접표준화비
    stan_ratio = direct_n/indirect_n 
    stan_ratio_l = direct_n_lci/indirect_n 
    stan_ratio_u = direct_n_uci/indirect_n 
    
    re_tb[i, 7] = round_new(stan_ratio, 2)
    re_tb[i, 8] = round_new(stan_ratio_l, 2)
    re_tb[i, 9] = round_new(stan_ratio_u, 2)
    d_list2[[j]] <- re_tb
  }
}

## 3.3.2.3 2019
d_list3 = list()
for (j in 1:NROW(tb2)) {
  re1= chs_c_rate_by(dt_3, by_var = "district", de_var = tb2[["c"]][j], 
                     index_subset = tb2[["d"]][[j]], josa_year = 2019)
  # identical(sort(unique(dong_name)), sort(unique(re1[,"By"])))
  
  district_name = re1[,"By"]
  
  stan_rate_dg <- chs_by_age_sex(dt_3[dt_3$josa_year %in% 2019,], 
                                 de_var= tb2[["c"]][j], var_name = tb2[["b"]][j])
  
  re_tb = matrix(nrow = length(district_name), ncol = 9)
  colnames(re_tb) = c(colnames(re1), "stan_ratio", "stan_ratio_lower", "stan_ratio_upper")
  re_tb[,1:6] <- re1
  for (i in 1:length(district_name)){
    pop_by = (fix_age_class_tochs(p_dg_d[[9]][p_dg_d[[9]]$district == district_name[i],], class_var = "age", josa_year = 2010) +
                fix_age_class_tochs(p_dg_d[[10]][p_dg_d[[10]]$district == district_name[i],], class_var = "age", josa_year = 2011))/2
    
    ## 지표별 분모 지정
    if (tb2[["b"]][j] %in% c("고혈압 의사진단경험률(30세이상)", "당뇨병 의사진단경험률(30세이상)",
                             "이상지질혈증 의사진단경험률(30세이상)")) {
      pop_by = pop_by[-c(1,2),]
    } else if (tb2[["b"]][j] == "관절염 의사진단경험률(50세이상)") { 
      pop_by <- pop_by[-c(1:6),]
    } else if (tb2[["b"]][j] == "남자현재흡연율") {
      pop_by <- pop_by[c(1,3,5,7,9,11),]
    } else if (tb2[["b"]][j] == "저작불편 호소율") {
      pop_by <- pop_by[c(9,10,11,12),]
    } else if (tb2[["b"]][j] == "고혈압 약물치료율(30세이상)") {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_3, de_var= "il_a0200", var_name = "고혈압 의사진단경험률(30세이상)")
    } else if (tb2[["b"]][j] %in% c("당뇨병 치료율(30세이상)","당뇨병 안질환 합병증검사 수진율(30세이상)","당뇨병 신장질환 합병증검사 수진율(30세이상)" )) {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_3, de_var= "il_b0200", var_name = "당뇨병 의사진단경험률(30세이상)") 
    } else if (tb2[["b"]][j] == "금연시도율") {
      pop_by = pop_by * chs_by_age_sex(dt_3, de_var = "sm_a0100", var_name = "현재흡연율") 
    } 
    
    
    
    ## 표준화비 구하기 (직접사망자수/간접사망자수)
    ## 직접 사망자수
    direct_n = as.numeric(re1[i, 4]) * sum(pop_by) / 100
    direct_n_lci = as.numeric(re1[i, 5]) * sum(pop_by) / 100
    direct_n_uci = as.numeric(re1[i, 6]) * sum(pop_by) / 100
    
    ## 간접 사망자수
    indirect_n = sum(stan_rate_dg * pop_by)
    
    ## 간접표준화비
    stan_ratio = direct_n/indirect_n 
    stan_ratio_l = direct_n_lci/indirect_n 
    stan_ratio_u = direct_n_uci/indirect_n 
    
    re_tb[i, 7] = round_new(stan_ratio, 2)
    re_tb[i, 8] = round_new(stan_ratio_l, 2)
    re_tb[i, 9] = round_new(stan_ratio_u, 2)
    d_list3[[j]] <- re_tb
  }
}

## 3.3.2.4 2015-2019
d_list4= list()
for (j in 1:NROW(tb2)) {
  re1= chs_c_rate_by(dt_4, by_var = "district", de_var = tb2[["c"]][j], 
                     index_subset = tb2[["d"]][[j]], josa_year = 2015:2019)
  # identical(sort(unique(dong_name)), sort(unique(re1[,"By"])))
  
  district_name = re1[,"By"]
  
  stan_rate_dg <- chs_by_age_sex(dt_4[dt_4$josa_year %in% 2015:2019,], 
                                 de_var= tb2[["c"]][j], var_name = tb2[["b"]][j])
  
  re_tb = matrix(nrow = length(district_name), ncol = 9)
  colnames(re_tb) = c(colnames(re1), "stan_ratio", "stan_ratio_lower", "stan_ratio_upper")
  re_tb[,1:6] <- re1
  for (i in 1:length(district_name)){
    pop_by = (fix_age_class_tochs(p_dg_d[[5]][p_dg_d[[5]]$district == district_name[i],], class_var = "age", josa_year = 2014) +
                fix_age_class_tochs(p_dg_d[[6]][p_dg_d[[6]]$district == district_name[i],], class_var = "age", josa_year = 2015)*2 +
                fix_age_class_tochs(p_dg_d[[7]][p_dg_d[[7]]$district == district_name[i],], class_var = "age", josa_year = 2016)*2 +
                fix_age_class_tochs(p_dg_d[[8]][p_dg_d[[8]]$district == district_name[i],], class_var = "age", josa_year = 2017)*2 +
                fix_age_class_tochs(p_dg_d[[9]][p_dg_d[[9]]$district == district_name[i],], class_var = "age", josa_year = 2018)*2 +
                fix_age_class_tochs(p_dg_d[[10]][p_dg_d[[10]]$district == district_name[i],], class_var = "age", josa_year = 2019))/10
    
    ## 지표별 분모 지정
    if (tb2[["b"]][j] %in% c("고혈압 의사진단경험률(30세이상)", "당뇨병 의사진단경험률(30세이상)",
                             "이상지질혈증 의사진단경험률(30세이상)")) {
      pop_by = pop_by[-c(1,2),]
    } else if (tb2[["b"]][j] == "관절염 의사진단경험률(50세이상)") { 
      pop_by <- pop_by[-c(1:6),]
    } else if (tb2[["b"]][j] == "남자현재흡연율") {
      pop_by <- pop_by[c(1,3,5,7,9,11),]
    } else if (tb2[["b"]][j] == "저작불편 호소율") {
      pop_by <- pop_by[c(9,10,11,12),]
    } else if (tb2[["b"]][j] == "고혈압 약물치료율(30세이상)") {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_4, de_var= "il_a0200", var_name = "고혈압 의사진단경험률(30세이상)")
    } else if (tb2[["b"]][j] %in% c("당뇨병 치료율(30세이상)","당뇨병 안질환 합병증검사 수진율(30세이상)","당뇨병 신장질환 합병증검사 수진율(30세이상)" )) {
      pop_by <- pop_by[-c(1,2),] * chs_by_age_sex(dt_4, de_var= "il_b0200", var_name = "당뇨병 의사진단경험률(30세이상)") 
    } else if (tb2[["b"]][j] == "금연시도율") {
      pop_by = pop_by * chs_by_age_sex(dt_4, de_var = "sm_a0100", var_name = "현재흡연율") 
    } 
    
    ## 표준화비 구하기 (직접사망자수/간접사망자수)
    ## 직접 사망자수
    direct_n = as.numeric(re1[i, 4]) * sum(pop_by) / 100
    direct_n_lci = as.numeric(re1[i, 5]) * sum(pop_by) / 100
    direct_n_uci = as.numeric(re1[i, 6]) * sum(pop_by) / 100
    
    ## 간접 사망자수
    indirect_n = sum(stan_rate_dg * pop_by)
    
    ## 간접표준화비
    stan_ratio = direct_n/indirect_n 
    stan_ratio_l = direct_n_lci/indirect_n 
    stan_ratio_u = direct_n_uci/indirect_n 
    
    re_tb[i, 7] = round_new(stan_ratio, 2)
    re_tb[i, 8] = round_new(stan_ratio_l, 2)
    re_tb[i, 9] = round_new(stan_ratio_u, 2)
    d_list4[[j]] <- re_tb
  }
}

# 4. 지도화
## 4.1 동별 지도
## 테마 지정
theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경
theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경 

## 지역명 넣기
region_name_district <- name_in_map(map_district_dg, name_var = "district")
region_name_bogun <- name_in_map(map_bogun_dg, name = c("남구","달서구","달성군","동구","북구","서구","수성구","중구"))

## 4.1.1 2011-2014
for (i in 1:NROW(tb2)) {
  ## 데이터 병합
  map_f = data_to_map(map_dong_dg, dong_list1[[i]], map_merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "By")
  
  ## 지도그리기
  map_f$c.rate = as.numeric(map_f$c.rate) 
  map_1 <- plot_map(map_f, re_var = "c.rate", index_type = tb2[["va_class"]][i], legend_label = "조율(%)", 
                    color_type = "viridis") +
    ggrepel::geom_label_repel(data = region_name_bogun, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  map_f$stan_ratio = as.numeric(map_f$stan_ratio)
  map_2 <- plot_map(map_f, re_var = "stan_ratio", index_type = tb2[["va_class"]][i], legend_label = "표준화비(%)", 
                    fixed_mid = 1, color_good = "blue", color_bad = "red", color_type = "blue_red") +
    ggrepel::geom_label_repel(data = region_name_bogun, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  ## 내보내기
  
  write.csv(dong_list1[[i]], paste0("./result/daegu/2011-2014/", tb2[["b"]][i], "_동별.csv"), fileEncoding = "euc-kr" )
  ggsave(paste0("./result/daegu/2011-2014/", tb2[["b"]][i], "(조율)_동별.png"), plot = map_1)
  ggsave(paste0("./result/daegu/2011-2014/", tb2[["b"]][i], "(표준화비)_동별.png"), plot = map_2)
}

## 2) 2015-2018

for (i in 1:NROW(tb2)) {
  ## 데이터 병합
  map_f = data_to_map(map_dong_dg, dong_list2[[i]], map_merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "By")
  
  ## 지도그리기
  map_f$c.rate = as.numeric(map_f$c.rate) 
  map_1 <- plot_map(map_f, re_var = "c.rate", index_type = tb2[["va_class"]][i], legend_label = "조율(%)", 
                    color_type = "viridis") +
    ggrepel::geom_label_repel(data = region_name_bogun, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  map_f$stan_ratio = as.numeric(map_f$stan_ratio)
  map_2 <- plot_map(map_f, re_var = "stan_ratio", index_type = tb2[["va_class"]][i], legend_label = "표준화비(%)", 
                    fixed_mid = 1, color_good = "blue", color_bad = "red", color_type = "blue_red") +
    ggrepel::geom_label_repel(data = region_name_bogun, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  ## 내보내기
  
  write.csv(dong_list2[[i]], paste0("./result/daegu/2015-2018/", tb2[["b"]][i], "_동별.csv"), fileEncoding = "euc-kr" )
  ggsave(paste0("./result/daegu/2015-2018/", tb2[["b"]][i], "(조율)_동별.png"), plot = map_1)
  ggsave(paste0("./result/daegu/2015-2018/", tb2[["b"]][i], "(표준화비)_동별.png"), plot = map_2)
}

## 3) 2015-2019

for (i in 1:NROW(tb2)) {
  ## 데이터 병합
  map_f = data_to_map(map_dong_dg, dong_list3[[i]], map_merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "By")
  
  ## 지도그리기
  map_f$c.rate = as.numeric(map_f$c.rate) 
  map_1 <- plot_map(map_f, re_var = "c.rate", index_type = tb2[["va_class"]][i], legend_label = "조율(%)", 
                    color_type = "viridis") +
    ggrepel::geom_label_repel(data = region_name_bogun, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  map_f$stan_ratio = as.numeric(map_f$stan_ratio)
  map_2 <- plot_map(map_f, re_var = "stan_ratio", index_type = tb2[["va_class"]][i], legend_label = "표준화비(%)", 
                    fixed_mid = 1, color_good = "blue", color_bad = "red", color_type = "blue_red") +
    ggrepel::geom_label_repel(data = region_name_bogun, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  ## 내보내기
  
  write.csv(dong_list3[[i]], paste0("./result/daegu/2015-2019/", tb2[["b"]][i], "_동별.csv"), fileEncoding = "euc-kr" )
  ggsave(paste0("./result/daegu/2015-2019/", tb2[["b"]][i], "(조율)_동별.png"), plot = map_1)
  ggsave(paste0("./result/daegu/2015-2019/", tb2[["b"]][i], "(표준화비)_동별.png"), plot = map_2)
}

## 4.2 권역별 지도 
## 1) 2011-2014
for (i in 1:NROW(tb2)) {
  
  ## 데이터 병합
  map_f = data_to_map(map_district_dg, d_list1[[i]], map_merge_key = "district", map_key = "district", data_key = "By")
  
  ## 지도그리기
  map_f$c.rate = as.numeric(map_f$c.rate) 
  map_1 <- plot_map(map_f, re_var = "c.rate", index_type = tb2[["va_class"]][i], legend_label = "조율(%)", 
                    color_type = "viridis") +
    ggrepel::geom_label_repel(data = region_name_district, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  map_f$stan_ratio = as.numeric(map_f$stan_ratio)
  map_2 <- plot_map(map_f, re_var = "stan_ratio", index_type = tb2[["va_class"]][i], legend_label = "표준화비(%)", 
                    fixed_mid = 1, color_good = "blue", color_bad = "red", color_type = "blue_red") +
    ggrepel::geom_label_repel(data = region_name_district, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  ## 내보내기
  
  write.csv(d_list1[[i]], paste0("./result/daegu/2011-2014/", tb2[["b"]][i], "_권역별.csv"), fileEncoding = "euc-kr" )
  ggsave(paste0("./result/daegu/2011-2014/", tb2[["b"]][i], "(조율)_권역별.png"), plot = map_1)
  ggsave(paste0("./result/daegu/2011-2014/", tb2[["b"]][i], "(표준화비)_권역별.png"), plot = map_2)
}

## 2) 2015-2018
for (i in 1:NROW(tb2)) {
  
  ## 데이터 병합
  map_f = data_to_map(map_district_dg, d_list2[[i]], map_merge_key = "district", map_key = "district", data_key = "By")
  
  ## 지도그리기
  map_f$c.rate = as.numeric(map_f$c.rate) 
  map_1 <- plot_map(map_f, re_var = "c.rate", index_type = tb2[["va_class"]][i], legend_label = "조율(%)", 
                    color_type = "viridis") +
    ggrepel::geom_label_repel(data = region_name_district, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  map_f$stan_ratio = as.numeric(map_f$stan_ratio)
  map_2 <- plot_map(map_f, re_var = "stan_ratio", index_type = tb2[["va_class"]][i], legend_label = "표준화비(%)", 
                    fixed_mid = 1, color_good = "blue", color_bad = "red", color_type = "blue_red") +
    ggrepel::geom_label_repel(data = region_name_district, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  ## 내보내기
  
  write.csv(d_list2[[i]], paste0("./result/daegu/2015-2018/", tb2[["b"]][i], "_권역별.csv"), fileEncoding = "euc-kr" )
  ggsave(paste0("./result/daegu/2015-2018/", tb2[["b"]][i], "(조율)_권역별.png"), plot = map_1)
  ggsave(paste0("./result/daegu/2015-2018/", tb2[["b"]][i], "(표준화비)_권역별.png"), plot = map_2)
}
## 3) 2019
## 4) 2015-2019
for (i in 1:NROW(tb2)) {
  
  ## 데이터 병합
  map_f = data_to_map(map_district_dg, d_list4[[i]], map_merge_key = "district", map_key = "district", data_key = "By")
  
  ## 지도그리기
  map_f$c.rate = as.numeric(map_f$c.rate) 
  map_1 <- plot_map(map_f, re_var = "c.rate", index_type = tb2[["va_class"]][i], legend_label = "조율(%)", 
                    color_type = "viridis") +
    ggrepel::geom_label_repel(data = region_name_district, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  map_f$stan_ratio = as.numeric(map_f$stan_ratio)
  map_2 <- plot_map(map_f, re_var = "stan_ratio", index_type = tb2[["va_class"]][i], legend_label = "표준화비(%)", 
                    fixed_mid = 1, color_good = "blue", color_bad = "red", color_type = "blue_red") +
    ggrepel::geom_label_repel(data = region_name_district, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") +
    theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete) 
  
  ## 내보내기
  
  write.csv(d_list4[[i]], paste0("./result/daegu/2015-2019/", tb2[["b"]][i], "_권역별.csv"), fileEncoding = "euc-kr" )
  ggsave(paste0("./result/daegu/2015-2019/", tb2[["b"]][i], "(조율)_권역별.png"), plot = map_1)
  ggsave(paste0("./result/daegu/2015-2019/", tb2[["b"]][i], "(표준화비)_권역별.png"), plot = map_2)
}


