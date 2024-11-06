setwd('/Users/zhoujia/Desktop/AA1030') ##设置工作路径
library(dplyr)
EC <- read.csv('worldyear.csv',header = T)  ## 读取我们的数据

####  1990 ASIR_EAPC
##获取ASIR
ASIR_1990 <- subset(EC,EC$year==1990 & 
                     EC$age_name=='Age-standardized' & 
                     EC$metric_name== 'Rate' &
                     EC$measure_name=='Incidence') ## 获取1990年EC年龄校正后发病率
ASIR_1990 <- ASIR_1990[,c(4,14)]  ###只取需要的变量
names(ASIR_1990)[2] <- 'ASR'
###获取绝对发病数
Incidence_case_1990 <- subset(EC,EC$year==1990 & 
                                EC$age_name=='All ages' & 
                                EC$metric_name== 'Number' &
                                EC$measure_name=='Incidence')
Incidence_case_1990 <- Incidence_case_1990[,c(4,14)]  ###只取需要的变量
names(Incidence_case_1990)[2] <- 'case'
#### 计算EAPC
EAPC <- subset(EC, EC$age_name=='Age-standardized' & 
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')

EAPC <- EAPC[,c(4,13,14)]

country <- ASIR_1990$location  ###获取国家名称
EAPC_cal <- data.frame(location_name=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

###合并三者数据成一个数据集
Total <- merge(Incidence_case_1990,EAPC_cal, by='location_name')
Total <- merge(Total,ASIR_1990, by='location_name')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'  ###指示变量提示该数据为发病率数据



###绘制图像
library(ggplot2)
library(ggrepel)

# 设置阈值
threshold <- 400
x_range <- range(Total$ASR, na.rm = TRUE)
y_range <- range(Total$EAPC, na.rm = TRUE)
labels_to_plot <- Total %>%
  filter(ASR > threshold) %>%
  top_n(10, case)  # 选择发病数最大的10个国家

# 绘制图像
p1 <- ggplot(Total, aes(ASR, EAPC, size = case)) +  
  geom_point(color='darkred') +
  geom_smooth(data = Total,aes(ASR, EAPC),se = .8,colour='black',span=1) +
  geom_text_repel(aes(label = location_name), data = labels_to_plot, 
                  na.rm = TRUE, 
                  box.padding = 5,  # 标签与点的距离
                  point.padding = 0.5,  # 标签与点的距离
                  segment.color = 'grey50',
                  max.overlaps = 25) +  # 标签连线颜色
  scale_size(name = 'Cases in 1990', breaks = c(100, 1000, 10000, 50000),
             labels = c("<500", "500-1,000", "10,000-50,000", ">50,000")) +
  theme_light()

p1


### 计算pearson相关系数及对应P值
cor.test(Total_incidence$EAPC,Total_incidence$ASR,method="pearson")


####  2021 ASIR_EAPC
##获取ASIR
ASIR_2021 <- subset(EC,EC$year==2021 & 
                      EC$age_name=='Age-standardized' & 
                      EC$metric_name== 'Rate' &
                      EC$measure_name=='Incidence') ## 获取2021年EC年龄校正后发病率
ASIR_2021 <- ASIR_2021[,c(4,14)]  ###只取需要的变量
names(ASIR_2021)[2] <- 'ASR'
###获取绝对发病数
Incidence_case_2021 <- subset(EC,EC$year==2021 & 
                                EC$age_name=='All ages' & 
                                EC$metric_name== 'Number' &
                                EC$measure_name=='Incidence')
Incidence_case_2021 <- Incidence_case_2021[,c(4,14)]  ###只取需要的变量
names(Incidence_case_2021)[2] <- 'case'
#### 计算EAPC
EAPC <- subset(EC, EC$age_name=='Age-standardized' & 
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')

EAPC <- EAPC[,c(4,13,14)]

country <- ASIR_2021$location_name  ###获取国家名称
EAPC_cal <- data.frame(location_name=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location_name==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

###合并三者数据成一个数据集
Total <- merge(Incidence_case_2021,EAPC_cal, by='location_name')
Total <- merge(Total,ASIR_2021, by='location_name')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'  ###指示变量提示该数据为发病率数据



###绘制图像
library(ggplot2)
library(ggrepel)



# 修改国家名称
Total$location_name[Total$location_name == "Kingdom of Thailand"] = "Thailand"
Total$location_name[Total$location_name == "Republic of Indonesia"] = "Indonesia"
Total$location_name[Total$location_name == "Republic of Azerbaijan"] = "Azerbaijan"
Total$location_name[Total$location_name == "Independent State of Samoa"] = "Samoa"
Total$location_name[Total$location_name == "Socialist Republic of Viet Nam"] = "Vietnam"
Total$location_name[Total$location_name == "Republic of Albania"] = "Albania"
Total$location_name[Total$location_name == "Republic of Kazakhstan"] = "Kazakhstan"
Total$location_name[Total$location_name == "People's Republic of China"] = "China"
Total$location_name[Total$location_name == "Republic of Bulgaria"] = "Bulgaria"
Total$location_name[Total$location_name == "Kingdom of Tonga"] = "Tonga"
Total$location_name[Total$location_name == "Republic of Fiji"] = "Fiji"
Total$location_name[Total$location_name == "Republic of the Union of Myanmar"] = "Myanmar"
Total$location_name[Total$location_name == "Taiwan (Province of China)"] = "Taiwan"
Total$location_name[Total$location_name == "Republic of the Marshall Islands"] = "Marshall Islands"
Total$location_name[Total$location_name == "Democratic Socialist Republic of Sri Lanka"] = "Sri Lanka"
Total$location_name[Total$location_name == "Republic of Armenia"] = "Armenia"
Total$location_name[Total$location_name == "Independent State of Papua New Guinea"] = "Papua New Guinea"
Total$location_name[Total$location_name == "Kingdom of Cambodia"] = "Cambodia"
Total$location_name[Total$location_name == "Democratic Republic of Timor-Leste"] = "Timor-Leste"
Total$location_name[Total$location_name == "Republic of the Philippines"] = "Philippines"
Total$location_name[Total$location_name == "Republic of Uzbekistan"] = "Uzbekistan"
Total$location_name[Total$location_name == "Federated States of Micronesia"] = "Micronesia"
Total$location_name[Total$location_name == "Lao People's Democratic Republic"] = "Laos"
Total$location_name[Total$location_name == "Kyrgyz Republic"] = "Kyrgyzstan"
Total$location_name[Total$location_name == "Republic of Vanuatu"] = "Vanuatu"
Total$location_name[Total$location_name == "United States of America"] = "United States"
Total$location_name[Total$location_name == "Hellenic Republic"] = "Greece"
Total$location_name[Total$location_name == "Republic of Tajikistan"] = "Tajikistan"
Total$location_name[Total$location_name == "Bosnia and Herzegovina"] = "Bosnia and Herzegovina"
Total$location_name[Total$location_name == "Republic of Maldives"] = "Maldives"
Total$location_name[Total$location_name == "Republic of Croatia"] = "Croatia"
Total$location_name[Total$location_name == "Republic of Kiribati"] = "Kiribati"
Total$location_name[Total$location_name == "Czech Republic"] = "Czechia"
Total$location_name[Total$location_name == "Democratic People's Republic of Korea"] = "North Korea"
Total$location_name[Total$location_name == "Slovak Republic"] = "Slovakia"
Total$location_name[Total$location_name == "Republic of Latvia"] = "Latvia"
Total$location_name[Total$location_name == "Brunei Darussalam"] = "Brunei"
Total$location_name[Total$location_name == "Republic of Austria"] = "Austria"
Total$location_name[Total$location_name == "Republic of Korea"] = "South Korea"
Total$location_name[Total$location_name == "Republic of Moldova"] = "Moldova"
Total$location_name[Total$location_name == "Republic of Estonia"] = "Estonia"
Total$location_name[Total$location_name == "Republic of Lithuania"] = "Lithuania"
Total$location_name[Total$location_name == "Republic of Cyprus"] = "Cyprus"
Total$location_name[Total$location_name == "Republic of Singapore"] = "Singapore"
Total$location_name[Total$location_name == "Republic of Finland"] = "Finland"
Total$location_name[Total$location_name == "Republic of Serbia"] = "Serbia"
Total$location_name[Total$location_name == "Republic of Poland"] = "Poland"
Total$location_name[Total$location_name == "Kingdom of Sweden"] = "Sweden"
Total$location_name[Total$location_name == "Kingdom of Denmark"] = "Denmark"
Total$location_name[Total$location_name == "Federal Republic of Germany"] = "Germany"
Total$location_name[Total$location_name == "Kingdom of Belgium"] = "Belgium"
Total$location_name[Total$location_name == "Republic of Iceland"] = "Iceland"
Total$location_name[Total$location_name == "Russian Federation"] = "Russia"
Total$location_name[Total$location_name == "Republic of Slovenia"] = "Slovenia"
Total$location_name[Total$location_name == "Republic of Italy"] = "Italy"
Total$location_name[Total$location_name == "Portuguese Republic"] = "Portugal"
Total$location_name[Total$location_name == "Commonwealth of the Bahamas"] = "Bahamas"
Total$location_name[Total$location_name == "State of Israel"] = "Israel"
Total$location_name[Total$location_name == "French Republic"] = "France"
Total$location_name[Total$location_name == "Republic of Cuba"] = "Cuba"
Total$location_name[Total$location_name == "Republic of Haiti"] = "Haiti"
Total$location_name[Total$location_name == "Kingdom of the Netherlands"] = "Netherlands"
Total$location_name[Total$location_name == "Federative Republic of Brazil"] = "Brazil"
Total$location_name[Total$location_name == "Republic of Belarus"] = "Belarus"
Total$location_name[Total$location_name == "Republic of Malta"] = "Malta"
Total$location_name[Total$location_name == "Dominican Republic"] = "Dominican Republic"
Total$location_name[Total$location_name == "Republic of Guyana"] = "Guyana"
Total$location_name[Total$location_name == "Republic of Paraguay"] = "Paraguay"
Total$location_name[Total$location_name == "Republic of Yemen"] = "Yemen"
Total$location_name[Total$location_name == "Commonwealth of Dominica"] = "Dominica"
Total$location_name[Total$location_name == "Republic of Suriname"] = "Suriname"
Total$location_name[Total$location_name == "Grand Duchy of Luxembourg"] = "Luxembourg"
Total$location_name[Total$location_name == "Saint Vincent and the Grenadines"] = "Saint Vincent and the Grenadines"
Total$location_name[Total$location_name == "Republic of Trinidad and Tobago"] = "Trinidad and Tobago"
Total$location_name[Total$location_name == "Kingdom of Norway"] = "Norway"
Total$location_name[Total$location_name == "Eastern Republic of Uruguay"] = "Uruguay"
Total$location_name[Total$location_name == "Saint Lucia"] = "Saint Lucia"
Total$location_name[Total$location_name == "United Kingdom of Great Britain and Northern Ireland"] = "United Kingdom"
Total$location_name[Total$location_name == "Argentine Republic"] = "Argentina"
Total$location_name[Total$location_name == "Republic of Chile"] = "Chile"
Total$location_name[Total$location_name == "Republic of Honduras"] = "Honduras"
Total$location_name[Total$location_name == "Republic of Ecuador"] = "Ecuador"
Total$location_name[Total$location_name == "Republic of Peru"] = "Peru"
Total$location_name[Total$location_name == "Republic of Nicaragua"] = "Nicaragua"
Total$location_name[Total$location_name == "Plurinational State of Bolivia"] = "Bolivia"
Total$location_name[Total$location_name == "Bolivarian Republic of Venezuela"] = "Venezuela"
Total$location_name[Total$location_name == "United Mexican States"] = "Mexico"
Total$location_name[Total$location_name == "State of Kuwait"] = "Kuwait"
Total$location_name[Total$location_name == "People's Democratic Republic of Algeria"] = "Algeria"
Total$location_name[Total$location_name == "Arab Republic of Egypt"] = "Egypt"
Total$location_name[Total$location_name == "Palestine"] = "Palestine"
Total$location_name[Total$location_name == "Republic of Turkey"] = "Turkey"
Total$location_name[Total$location_name == "State of Qatar"] = "Qatar"
Total$location_name[Total$location_name == "Republic of Guatemala"] = "Guatemala"
Total$location_name[Total$location_name == "Islamic Republic of Iran"] = "Iran"
Total$location_name[Total$location_name == "Lebanese Republic"] = "Lebanon"
Total$location_name[Total$location_name == "Kingdom of Saudi Arabia"] = "Saudi Arabia"
Total$location_name[Total$location_name == "Central African Republic"] = "Central African Republic"
Total$location_name[Total$location_name == "Republic of Panama"] = "Panama"
Total$location_name[Total$location_name == "Islamic Republic of Pakistan"] = "Pakistan"
Total$location_name[Total$location_name == "Sultanate of Oman"] = "Oman"
Total$location_name[Total$location_name == "Republic of Iraq"] = "Iraq"
Total$location_name[Total$location_name == "Hashemite Kingdom of Jordan"] = "Jordan"
Total$location_name[Total$location_name == "State of Libya"] = "Libya"
Total$location_name[Total$location_name == "Kingdom of Bahrain"] = "Bahrain"
Total$location_name[Total$location_name == "Kingdom of Morocco"] = "Morocco"
Total$location_name[Total$location_name == "Republic of Equatorial Guinea"] = "Equatorial Guinea"
Total$location_name[Total$location_name == "Republic of Costa Rica"] = "Costa Rica"
Total$location_name[Total$location_name == "Syrian Arab Republic"] = "Syria"
Total$location_name[Total$location_name == "People's Republic of Bangladesh"] = "Bangladesh"
Total$location_name[Total$location_name == "Republic of Tunisia"] = "Tunisia"
Total$location_name[Total$location_name == "Republic of El Salvador"] = "El Salvador"
Total$location_name[Total$location_name == "Democratic Republic of the Congo"] = "Democratic Republic of Congo"
Total$location_name[Total$location_name == "Republic of Angola"] = "Angola"
Total$location_name[Total$location_name == "Republic of India"] = "India"
Total$location_name[Total$location_name == "Kingdom of Bhutan"] = "Bhutan"
Total$location_name[Total$location_name == "United Arab Emirates"] = "United Arab Emirates"
Total$location_name[Total$location_name == "Republic of Colombia"] = "Colombia"
Total$location_name[Total$location_name == "Jamaica"] = "Jamaica"
Total$location_name[Total$location_name == "Islamic Republic of Afghanistan"] = "Afghanistan"
Total$location_name[Total$location_name == "Republic of the Congo"] = "Republic of Congo"
Total$location_name[Total$location_name == "Republic of Burundi"] = "Burundi"
Total$location_name[Total$location_name == "Federal Democratic Republic of Nepal"] = "Nepal"
Total$location_name[Total$location_name == "Gabonese Republic"] = "Gabon"
Total$location_name[Total$location_name == "Republic of Mozambique"] = "Mozambique"
Total$location_name[Total$location_name == "State of Eritrea"] = "Eritrea"
Total$location_name[Total$location_name == "Union of the Comoros"] = "Comoros"
Total$location_name[Total$location_name == "Republic of Djibouti"] = "Djibouti"
Total$location_name[Total$location_name == "Republic of Madagascar"] = "Madagascar"
Total$location_name[Total$location_name == "Republic of Malawi"] = "Malawi"
Total$location_name[Total$location_name == "Republic of Mauritius"] = "Mauritius"
Total$location_name[Total$location_name == "Federal Democratic Republic of Ethiopia"] = "Ethiopia"
Total$location_name[Total$location_name == "Republic of Kenya"] = "Kenya"
Total$location_name[Total$location_name == "Republic of Namibia"] = "Namibia"
Total$location_name[Total$location_name == "Federal Republic of Somalia"] = "Somalia"
Total$location_name[Total$location_name == "Republic of Seychelles"] = "Seychelles"
Total$location_name[Total$location_name == "Republic of Rwanda"] = "Rwanda"
Total$location_name[Total$location_name == "United Republic of Tanzania"] = "Tanzania"
Total$location_name[Total$location_name == "Republic of Uganda"] = "Uganda"
Total$location_name[Total$location_name == "Republic of Botswana"] = "Botswana"
Total$location_name[Total$location_name == "Republic of Zambia"] = "Zambia"
Total$location_name[Total$location_name == "Republic of Cabo Verde"] = "Cabo Verde"
Total$location_name[Total$location_name == "Burkina Faso"] = "Burkina Faso"
Total$location_name[Total$location_name == "Kingdom of Eswatini"] = "Eswatini"
Total$location_name[Total$location_name == "Republic of Zimbabwe"] = "Zimbabwe"
Total$location_name[Total$location_name == "Kingdom of Lesotho"] = "Lesotho"
Total$location_name[Total$location_name == "Republic of Cameroon"] = "Cameroon"
Total$location_name[Total$location_name == "Republic of Ghana"] = "Ghana"
Total$location_name[Total$location_name == "Republic of C??te d'Ivoire"] = "Ivory Coast"
Total$location_name[Total$location_name == "Republic of South Africa"] = "South Africa"
Total$location_name[Total$location_name == "Republic of Benin"] = "Benin"
Total$location_name[Total$location_name == "Republic of Guinea-Bissau"] = "Guinea-Bissau"
Total$location_name[Total$location_name == "Republic of Mali"] = "Mali"
Total$location_name[Total$location_name == "Republic of Chad"] = "Chad"
Total$location_name[Total$location_name == "Republic of the Gambia"] = "Gambia"
Total$location_name[Total$location_name == "United States Virgin Islands"] = "U.S. Virgin Islands"
Total$location_name[Total$location_name == "Togolese Republic"] = "Togo"
Total$location_name[Total$location_name == "Republic of Liberia"] = "Liberia"
Total$location_name[Total$location_name == "American Samoa"] = "American Samoa"
Total$location_name[Total$location_name == "Greenland"] = "Greenland"
Total$location_name[Total$location_name == "Republic of Sierra Leone"] = "Sierra Leone"
Total$location_name[Total$location_name == "Republic of South Sudan"] = "South Sudan"
Total$location_name[Total$location_name == "Tuvalu"] = "Tuvalu"
Total$location_name[Total$location_name == "Democratic Republic of Sao Tome and Principe"] = "Sao Tome and Principe"
Total$location_name[Total$location_name == "Republic of Guinea"] = "Guinea"
Total$location_name[Total$location_name == "Guam"] = "Guam"
Total$location_name[Total$location_name == "Republic of Senegal"] = "Senegal"
Total$location_name[Total$location_name == "Cook Islands"] = "Cook Islands"
Total$location_name[Total$location_name == "Islamic Republic of Mauritania"] = "Mauritania"
Total$location_name[Total$location_name == "Saint Kitts and Nevis"] = "Saint Kitts and Nevis"
Total$location_name[Total$location_name == "Bermuda"] = "Bermuda"
Total$location_name[Total$location_name == "Puerto Rico"] = "Puerto Rico"
Total$location_name[Total$location_name == "Republic of San Marino"] = "San Marino"
Total$location_name[Total$location_name == "Principality of Monaco"] = "Monaco"
Total$location_name[Total$location_name == "Northern Mariana Islands"] = "Northern Mariana Islands"
Total$location_name[Total$location_name == "Federal Republic of Nigeria"] = "Nigeria"
Total$location_name[Total$location_name == "Republic of Nauru"] = "Nauru"
Total$location_name[Total$location_name == "Republic of Niue"] = "Niue"
Total$location_name[Total$location_name == "Republic of the Niger"] = "Niger"
Total$location_name[Total$location_name == "Tokelau"] = "Tokelau"
Total$location_name[Total$location_name == "Republic of Palau"] = "Palau"
Total$location_name[Total$location_name == "Republic of Sudan"] = "Sudan"







# 设置阈值

labels_to_plot <- Total %>%
  filter((ASR<350)|(ASR>450)) %>%
  top_n(10, case)  # 选择发病数最大的10个国家

# 绘制图像
p1 <- ggplot(Total, aes(ASR, EAPC, size = case)) +  
  geom_point(color='darkgreen') +
  geom_smooth(data = Total,aes(ASR, EAPC),se = .8,colour='black',span=1) +
  geom_text_repel(aes(label = location_name), data = labels_to_plot, 
                  na.rm = TRUE, 
                  size = 4,
                  box.padding = 4,  # 标签与点的距离
                  point.padding = 0.5,  # 标签与点的距离
                  segment.color = 'grey50',
                  max.overlaps = 35) +  # 标签连线颜色
  scale_size(name = 'Cases in 2021', breaks = c(100, 1000, 10000, 50000),
             labels = c("<500", "500-1,000", "10,000-50,000", ">50,000")) +
  theme_light()

p1


### 计算pearson相关系数及对应P值
cor.test(Total_incidence$EAPC,Total_incidence$ASR,method="pearson")




#### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### 
####  2021 HDI_EAPC
####  2021 ASIR_HDI
### 读取 HDI数据
HDI <- read.csv('HDI.csv',header = T)
names(HDI) <- c('location_name','HDI')


###获取绝对发病数
Incidence_case_2021 <- subset(EC,EC$year==2021 & 
                                EC$age_name=='All ages' & 
                                EC$metric_name== 'Number' &
                                EC$measure_name=='Incidence')
Incidence_case_2021 <- Incidence_case_2021[,c(4,14)]  ###只取需要的变量
names(Incidence_case_2021)[2] <- 'case'

#### 计算EAPC
EAPC <- subset(EC, EC$age_name=='Age-standardized' & 
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')

EAPC <- EAPC[,c(4,13,14)]

country <- Incidence_case_2021$location_name  ###获取国家名称
EAPC_cal <- data.frame(location_name=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location_name==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]






### 合并三者数据
Total <- merge(Incidence_case_2021,EAPC_cal, by='location_name')

# 修改国家名称
Total$location_name[Total$location_name == "Kingdom of Thailand"] = "Thailand"
Total$location_name[Total$location_name == "Republic of Indonesia"] = "Indonesia"
Total$location_name[Total$location_name == "Republic of Azerbaijan"] = "Azerbaijan"
Total$location_name[Total$location_name == "Independent State of Samoa"] = "Samoa"
Total$location_name[Total$location_name == "Socialist Republic of Viet Nam"] = "Vietnam"
Total$location_name[Total$location_name == "Republic of Albania"] = "Albania"
Total$location_name[Total$location_name == "Republic of Kazakhstan"] = "Kazakhstan"
Total$location_name[Total$location_name == "People's Republic of China"] = "China"
Total$location_name[Total$location_name == "Republic of Bulgaria"] = "Bulgaria"
Total$location_name[Total$location_name == "Kingdom of Tonga"] = "Tonga"
Total$location_name[Total$location_name == "Republic of Fiji"] = "Fiji"
Total$location_name[Total$location_name == "Republic of the Union of Myanmar"] = "Myanmar"
Total$location_name[Total$location_name == "Taiwan (Province of China)"] = "Taiwan"
Total$location_name[Total$location_name == "Republic of the Marshall Islands"] = "Marshall Islands"
Total$location_name[Total$location_name == "Democratic Socialist Republic of Sri Lanka"] = "Sri Lanka"
Total$location_name[Total$location_name == "Republic of Armenia"] = "Armenia"
Total$location_name[Total$location_name == "Independent State of Papua New Guinea"] = "Papua New Guinea"
Total$location_name[Total$location_name == "Kingdom of Cambodia"] = "Cambodia"
Total$location_name[Total$location_name == "Democratic Republic of Timor-Leste"] = "Timor-Leste"
Total$location_name[Total$location_name == "Republic of the Philippines"] = "Philippines"
Total$location_name[Total$location_name == "Republic of Uzbekistan"] = "Uzbekistan"
Total$location_name[Total$location_name == "Federated States of Micronesia"] = "Micronesia"
Total$location_name[Total$location_name == "Lao People's Democratic Republic"] = "Laos"
Total$location_name[Total$location_name == "Kyrgyz Republic"] = "Kyrgyzstan"
Total$location_name[Total$location_name == "Republic of Vanuatu"] = "Vanuatu"
Total$location_name[Total$location_name == "Hellenic Republic"] = "Greece"
Total$location_name[Total$location_name == "Republic of Tajikistan"] = "Tajikistan"
Total$location_name[Total$location_name == "Bosnia and Herzegovina"] = "Bosnia and Herzegovina"
Total$location_name[Total$location_name == "Republic of Maldives"] = "Maldives"
Total$location_name[Total$location_name == "Republic of Croatia"] = "Croatia"
Total$location_name[Total$location_name == "Republic of Kiribati"] = "Kiribati"
Total$location_name[Total$location_name == "Czech Republic"] = "Czechia"
Total$location_name[Total$location_name == "Democratic People's Republic of Korea"] = "North Korea"
Total$location_name[Total$location_name == "Slovak Republic"] = "Slovakia"
Total$location_name[Total$location_name == "Republic of Latvia"] = "Latvia"
Total$location_name[Total$location_name == "Brunei Darussalam"] = "Brunei"
Total$location_name[Total$location_name == "Republic of Austria"] = "Austria"
Total$location_name[Total$location_name == "Republic of Korea"] = "South Korea"
Total$location_name[Total$location_name == "Republic of Moldova"] = "Moldova"
Total$location_name[Total$location_name == "Republic of Estonia"] = "Estonia"
Total$location_name[Total$location_name == "Republic of Lithuania"] = "Lithuania"
Total$location_name[Total$location_name == "Republic of Cyprus"] = "Cyprus"
Total$location_name[Total$location_name == "Republic of Singapore"] = "Singapore"
Total$location_name[Total$location_name == "Republic of Finland"] = "Finland"
Total$location_name[Total$location_name == "Republic of Serbia"] = "Serbia"
Total$location_name[Total$location_name == "Republic of Poland"] = "Poland"
Total$location_name[Total$location_name == "Kingdom of Sweden"] = "Sweden"
Total$location_name[Total$location_name == "Kingdom of Denmark"] = "Denmark"
Total$location_name[Total$location_name == "Federal Republic of Germany"] = "Germany"
Total$location_name[Total$location_name == "Kingdom of Belgium"] = "Belgium"
Total$location_name[Total$location_name == "Republic of Iceland"] = "Iceland"
Total$location_name[Total$location_name == "Russian Federation"] = "Russia"
Total$location_name[Total$location_name == "Republic of Slovenia"] = "Slovenia"
Total$location_name[Total$location_name == "Republic of Italy"] = "Italy"
Total$location_name[Total$location_name == "Portuguese Republic"] = "Portugal"
Total$location_name[Total$location_name == "Commonwealth of the Bahamas"] = "Bahamas"
Total$location_name[Total$location_name == "State of Israel"] = "Israel"
Total$location_name[Total$location_name == "French Republic"] = "France"
Total$location_name[Total$location_name == "Republic of Cuba"] = "Cuba"
Total$location_name[Total$location_name == "Republic of Haiti"] = "Haiti"
Total$location_name[Total$location_name == "Kingdom of the Netherlands"] = "Netherlands"
Total$location_name[Total$location_name == "Federative Republic of Brazil"] = "Brazil"
Total$location_name[Total$location_name == "Republic of Belarus"] = "Belarus"
Total$location_name[Total$location_name == "Republic of Malta"] = "Malta"
Total$location_name[Total$location_name == "Dominican Republic"] = "Dominican Republic"
Total$location_name[Total$location_name == "Republic of Guyana"] = "Guyana"
Total$location_name[Total$location_name == "Republic of Paraguay"] = "Paraguay"
Total$location_name[Total$location_name == "Republic of Yemen"] = "Yemen"
Total$location_name[Total$location_name == "Commonwealth of Dominica"] = "Dominica"
Total$location_name[Total$location_name == "Republic of Suriname"] = "Suriname"
Total$location_name[Total$location_name == "Grand Duchy of Luxembourg"] = "Luxembourg"
Total$location_name[Total$location_name == "Saint Vincent and the Grenadines"] = "Saint Vincent and the Grenadines"
Total$location_name[Total$location_name == "Republic of Trinidad and Tobago"] = "Trinidad and Tobago"
Total$location_name[Total$location_name == "Kingdom of Norway"] = "Norway"
Total$location_name[Total$location_name == "Eastern Republic of Uruguay"] = "Uruguay"
Total$location_name[Total$location_name == "Saint Lucia"] = "Saint Lucia"
Total$location_name[Total$location_name == "United Kingdom of Great Britain and Northern Ireland"] = "United Kingdom"
Total$location_name[Total$location_name == "Argentine Republic"] = "Argentina"
Total$location_name[Total$location_name == "Republic of Chile"] = "Chile"
Total$location_name[Total$location_name == "Republic of Honduras"] = "Honduras"
Total$location_name[Total$location_name == "Republic of Ecuador"] = "Ecuador"
Total$location_name[Total$location_name == "Republic of Peru"] = "Peru"
Total$location_name[Total$location_name == "Republic of Nicaragua"] = "Nicaragua"
Total$location_name[Total$location_name == "Plurinational State of Bolivia"] = "Bolivia"
Total$location_name[Total$location_name == "Bolivarian Republic of Venezuela"] = "Venezuela"
Total$location_name[Total$location_name == "United Mexican States"] = "Mexico"
Total$location_name[Total$location_name == "State of Kuwait"] = "Kuwait"
Total$location_name[Total$location_name == "People's Democratic Republic of Algeria"] = "Algeria"
Total$location_name[Total$location_name == "Arab Republic of Egypt"] = "Egypt"
Total$location_name[Total$location_name == "Palestine"] = "Palestine"
Total$location_name[Total$location_name == "Republic of Turkey"] = "Turkey"
Total$location_name[Total$location_name == "State of Qatar"] = "Qatar"
Total$location_name[Total$location_name == "Republic of Guatemala"] = "Guatemala"
Total$location_name[Total$location_name == "Islamic Republic of Iran"] = "Iran"
Total$location_name[Total$location_name == "Lebanese Republic"] = "Lebanon"
Total$location_name[Total$location_name == "Kingdom of Saudi Arabia"] = "Saudi Arabia"
Total$location_name[Total$location_name == "Central African Republic"] = "Central African Republic"
Total$location_name[Total$location_name == "Republic of Panama"] = "Panama"
Total$location_name[Total$location_name == "Islamic Republic of Pakistan"] = "Pakistan"
Total$location_name[Total$location_name == "Sultanate of Oman"] = "Oman"
Total$location_name[Total$location_name == "Republic of Iraq"] = "Iraq"
Total$location_name[Total$location_name == "Hashemite Kingdom of Jordan"] = "Jordan"
Total$location_name[Total$location_name == "State of Libya"] = "Libya"
Total$location_name[Total$location_name == "Kingdom of Bahrain"] = "Bahrain"
Total$location_name[Total$location_name == "Kingdom of Morocco"] = "Morocco"
Total$location_name[Total$location_name == "Republic of Equatorial Guinea"] = "Equatorial Guinea"
Total$location_name[Total$location_name == "Republic of Costa Rica"] = "Costa Rica"
Total$location_name[Total$location_name == "Syrian Arab Republic"] = "Syria"
Total$location_name[Total$location_name == "People's Republic of Bangladesh"] = "Bangladesh"
Total$location_name[Total$location_name == "Republic of Tunisia"] = "Tunisia"
Total$location_name[Total$location_name == "Republic of El Salvador"] = "El Salvador"
Total$location_name[Total$location_name == "Democratic Republic of the Congo"] = "Democratic Republic of Congo"
Total$location_name[Total$location_name == "Republic of Angola"] = "Angola"
Total$location_name[Total$location_name == "Republic of India"] = "India"
Total$location_name[Total$location_name == "Kingdom of Bhutan"] = "Bhutan"
Total$location_name[Total$location_name == "United Arab Emirates"] = "United Arab Emirates"
Total$location_name[Total$location_name == "Republic of Colombia"] = "Colombia"
Total$location_name[Total$location_name == "Jamaica"] = "Jamaica"
Total$location_name[Total$location_name == "Islamic Republic of Afghanistan"] = "Afghanistan"
Total$location_name[Total$location_name == "Republic of the Congo"] = "Republic of Congo"
Total$location_name[Total$location_name == "Republic of Burundi"] = "Burundi"
Total$location_name[Total$location_name == "Federal Democratic Republic of Nepal"] = "Nepal"
Total$location_name[Total$location_name == "Gabonese Republic"] = "Gabon"
Total$location_name[Total$location_name == "Republic of Mozambique"] = "Mozambique"
Total$location_name[Total$location_name == "State of Eritrea"] = "Eritrea"
Total$location_name[Total$location_name == "Union of the Comoros"] = "Comoros"
Total$location_name[Total$location_name == "Republic of Djibouti"] = "Djibouti"
Total$location_name[Total$location_name == "Republic of Madagascar"] = "Madagascar"
Total$location_name[Total$location_name == "Republic of Malawi"] = "Malawi"
Total$location_name[Total$location_name == "Republic of Mauritius"] = "Mauritius"
Total$location_name[Total$location_name == "Federal Democratic Republic of Ethiopia"] = "Ethiopia"
Total$location_name[Total$location_name == "Republic of Kenya"] = "Kenya"
Total$location_name[Total$location_name == "Republic of Namibia"] = "Namibia"
Total$location_name[Total$location_name == "Federal Republic of Somalia"] = "Somalia"
Total$location_name[Total$location_name == "Republic of Seychelles"] = "Seychelles"
Total$location_name[Total$location_name == "Republic of Rwanda"] = "Rwanda"
Total$location_name[Total$location_name == "United Republic of Tanzania"] = "Tanzania"
Total$location_name[Total$location_name == "Republic of Uganda"] = "Uganda"
Total$location_name[Total$location_name == "Republic of Botswana"] = "Botswana"
Total$location_name[Total$location_name == "Republic of Zambia"] = "Zambia"
Total$location_name[Total$location_name == "Republic of Cabo Verde"] = "Cabo Verde"
Total$location_name[Total$location_name == "Burkina Faso"] = "Burkina Faso"
Total$location_name[Total$location_name == "Kingdom of Eswatini"] = "Eswatini"
Total$location_name[Total$location_name == "Republic of Zimbabwe"] = "Zimbabwe"
Total$location_name[Total$location_name == "Kingdom of Lesotho"] = "Lesotho"
Total$location_name[Total$location_name == "Republic of Cameroon"] = "Cameroon"
Total$location_name[Total$location_name == "Republic of Ghana"] = "Ghana"
Total$location_name[Total$location_name == "Republic of C??te d'Ivoire"] = "Ivory Coast"
Total$location_name[Total$location_name == "Republic of South Africa"] = "South Africa"
Total$location_name[Total$location_name == "Republic of Benin"] = "Benin"
Total$location_name[Total$location_name == "Republic of Guinea-Bissau"] = "Guinea-Bissau"
Total$location_name[Total$location_name == "Republic of Mali"] = "Mali"
Total$location_name[Total$location_name == "Republic of Chad"] = "Chad"
Total$location_name[Total$location_name == "Republic of the Gambia"] = "Gambia"
Total$location_name[Total$location_name == "United States Virgin Islands"] = "U.S. Virgin Islands"
Total$location_name[Total$location_name == "Togolese Republic"] = "Togo"
Total$location_name[Total$location_name == "Republic of Liberia"] = "Liberia"
Total$location_name[Total$location_name == "American Samoa"] = "American Samoa"
Total$location_name[Total$location_name == "Greenland"] = "Greenland"
Total$location_name[Total$location_name == "Republic of Sierra Leone"] = "Sierra Leone"
Total$location_name[Total$location_name == "Republic of South Sudan"] = "South Sudan"
Total$location_name[Total$location_name == "Tuvalu"] = "Tuvalu"
Total$location_name[Total$location_name == "Democratic Republic of Sao Tome and Principe"] = "Sao Tome and Principe"
Total$location_name[Total$location_name == "Republic of Guinea"] = "Guinea"
Total$location_name[Total$location_name == "Guam"] = "Guam"
Total$location_name[Total$location_name == "Republic of Senegal"] = "Senegal"
Total$location_name[Total$location_name == "Cook Islands"] = "Cook Islands"
Total$location_name[Total$location_name == "Islamic Republic of Mauritania"] = "Mauritania"
Total$location_name[Total$location_name == "Saint Kitts and Nevis"] = "Saint Kitts and Nevis"
Total$location_name[Total$location_name == "Bermuda"] = "Bermuda"
Total$location_name[Total$location_name == "Puerto Rico"] = "Puerto Rico"
Total$location_name[Total$location_name == "Republic of San Marino"] = "San Marino"
Total$location_name[Total$location_name == "Principality of Monaco"] = "Monaco"
Total$location_name[Total$location_name == "Northern Mariana Islands"] = "Northern Mariana Islands"
Total$location_name[Total$location_name == "Federal Republic of Nigeria"] = "Nigeria"
Total$location_name[Total$location_name == "Republic of Nauru"] = "Nauru"
Total$location_name[Total$location_name == "Republic of Niue"] = "Niue"
Total$location_name[Total$location_name == "Republic of the Niger"] = "Niger"
Total$location_name[Total$location_name == "Tokelau"] = "Tokelau"
Total$location_name[Total$location_name == "Republic of Palau"] = "Palau"
Total$location_name[Total$location_name == "Republic of Sudan"] = "Sudan"









Total <- merge(Total,HDI, by='location_name')
Total_incidence <- Total
write.csv(Incidence_case_2021,'Incidence_case_2021.csv')
write.csv(HDI,'HDI.csv')




Total <- Total_incidence

# 设置阈值
threshold <- 0.5
labels_to_plot <- Total %>%
  filter((HDI < threshold)|(HDI > 0.9)) %>%
  top_n(10, case)  # 选择发病数最大的10个国家

### 作图
library(ggplot2)
p1 <- ggplot(Total,aes(HDI, EAPC, size = case))+  
  geom_point(color='darkblue')+
  geom_smooth(data = Total,aes(HDI, EAPC),se = .8,colour='black',span=1) +
  scale_size(name = 'Cases in 2021', breaks = c(100,1000,10000,50000),
             labels = c("<500","500-1,000","10,000-50,000",
                        '>50,000')) +
  geom_text_repel(aes(label = location_name), data = labels_to_plot, 
                  na.rm = TRUE, 
                  size = 4,  # 设置标签字体大小
                  box.padding = 2,  # 标签与点的距离
                  point.padding = 0.5,  # 标签与点的距离
                  segment.color = 'grey50',
                  max.overlaps = 30) +  # 标签连线颜色
  theme_light()
p1


### 计算pearson相关性系数
cor.test(Total_incidence$EAPC,Total_incidence$HDI,method="pearson")
cor.test(Total_Deaths$EAPC,Total_Deaths$HDI,method="pearson")

#########SDI
######################################################
######################################################

SDI <- read.csv('SDI2.csv',header = T)
SDI$year=as.numeric(SDI$year)
SDI2021 <- subset(SDI,year==2021)
SDI2021 <- SDI2021[,c(2,4)]  ###只取需要的变量
names(SDI2021) <- c('location_name','SDI')
Total <- merge(Total,SDI2021, by='location_name')
Total=Total[,c(1,2,3,4,6)]

labels_to_plot <- Total %>%
  filter((SDI < 0.5)|(SDI > 0.7)) %>%
  top_n(10, case)  # 选择发病数最大的10个国家

### 作图
library(ggplot2)
p1 <- ggplot(Total,aes(SDI, EAPC, size = case))+  
  geom_point(color='darkred')+
  geom_smooth(data = Total,aes(SDI, EAPC),se = .8,colour='black',span=1) +
  scale_size(name = 'Cases in 2021', breaks = c(100,1000,10000,50000),
             labels = c("<500","500-1,000","10,000-50,000",
                        '>50,000')) +
  geom_text_repel(aes(label = location_name), data = labels_to_plot, 
                  na.rm = TRUE, 
                  size = 4,  # 设置标签字体大小
                  box.padding = 2,  # 标签与点的距离
                  point.padding = 0.5,  # 标签与点的距离
                  segment.color = 'grey50',
                  max.overlaps = 30) +  # 标签连线颜色
  theme_light()
p1

cor.test(Total$EAPC,Total$SDI,method="pearson")
