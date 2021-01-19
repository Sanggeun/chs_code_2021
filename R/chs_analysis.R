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

library(g.function.bsg)

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

