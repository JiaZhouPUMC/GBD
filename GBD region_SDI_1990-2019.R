setwd('/Users/zhoujia/Desktop/AA1030') ##设置工作路径
library(reshape)
library(ggplot2)
library(ggrepel)

EC <- read.csv('worldyear.csv',header = T)  ## 读取我们的数据
order_SDI <- read.csv('order_SDI.csv',header = F)
SDI <- read.csv('SDI.csv',header = T)


library(reshape2)
SDI$mean_value <- as.numeric(as.character(SDI$mean_value))

# 然后重排数据
SDI_reformatted <- dcast(SDI, location_name ~ year_id, value.var = "mean_value", fun.aggregate = mean, na.rm = TRUE)
write.csv(SDI_reformatted,'SDI_reformatted.csv')
write.csv(SDI,'SDI2.csv')
SDI=read.csv('SDI_reformatted.csv',header=T)
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式
SDI <- melt(SDI_reformatted,id='location_name')
SDI$value <- as.numeric(gsub('\\X',replacement = '', SDI$value))
names(SDI) <- c('location_name','year','SDI')


### 获取EC的1990-2019的标准发病率数据
EC <- subset(EC, EC$age_name=='Age-standardized' & 
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')
EC <- EC[,c(4,13,14)]
names(EC)[3] <- 'ASR'


# 修改国家名称
EC$location_name[EC$location_name == "Kingdom of Thailand"] = "Thailand"
EC$location_name[EC$location_name == "Republic of Indonesia"] = "Indonesia"
EC$location_name[EC$location_name == "Republic of Azerbaijan"] = "Azerbaijan"
EC$location_name[EC$location_name == "Independent State of Samoa"] = "Samoa"
EC$location_name[EC$location_name == "Socialist Republic of Viet Nam"] = "Vietnam"
EC$location_name[EC$location_name == "Republic of Albania"] = "Albania"
EC$location_name[EC$location_name == "Republic of Kazakhstan"] = "Kazakhstan"
EC$location_name[EC$location_name == "People's Republic of China"] = "China"
EC$location_name[EC$location_name == "Republic of Bulgaria"] = "Bulgaria"
EC$location_name[EC$location_name == "Kingdom of Tonga"] = "Tonga"
EC$location_name[EC$location_name == "Republic of Fiji"] = "Fiji"
EC$location_name[EC$location_name == "Republic of the Union of Myanmar"] = "Myanmar"
EC$location_name[EC$location_name == "Taiwan (Province of China)"] = "Taiwan"
EC$location_name[EC$location_name == "Republic of the Marshall Islands"] = "Marshall Islands"
EC$location_name[EC$location_name == "Democratic Socialist Republic of Sri Lanka"] = "Sri Lanka"
EC$location_name[EC$location_name == "Republic of Armenia"] = "Armenia"
EC$location_name[EC$location_name == "Independent State of Papua New Guinea"] = "Papua New Guinea"
EC$location_name[EC$location_name == "Kingdom of Cambodia"] = "Cambodia"
EC$location_name[EC$location_name == "Democratic Republic of Timor-Leste"] = "Timor-Leste"
EC$location_name[EC$location_name == "Republic of the Philippines"] = "Philippines"
EC$location_name[EC$location_name == "Republic of Uzbekistan"] = "Uzbekistan"
EC$location_name[EC$location_name == "Federated States of Micronesia"] = "Micronesia"
EC$location_name[EC$location_name == "Lao People's Democratic Republic"] = "Laos"
EC$location_name[EC$location_name == "Kyrgyz Republic"] = "Kyrgyzstan"
EC$location_name[EC$location_name == "Republic of Vanuatu"] = "Vanuatu"
EC$location_name[EC$location_name == "United States of America"] = "United States of America"
EC$location_name[EC$location_name == "Hellenic Republic"] = "Greece"
EC$location_name[EC$location_name == "Republic of Tajikistan"] = "Tajikistan"
EC$location_name[EC$location_name == "Bosnia and Herzegovina"] = "Bosnia and Herzegovina"
EC$location_name[EC$location_name == "Republic of Maldives"] = "Maldives"
EC$location_name[EC$location_name == "Republic of Croatia"] = "Croatia"
EC$location_name[EC$location_name == "Republic of Kiribati"] = "Kiribati"
EC$location_name[EC$location_name == "Czech Republic"] = "Czechia"
EC$location_name[EC$location_name == "Democratic People's Republic of Korea"] = "North Korea"
EC$location_name[EC$location_name == "Slovak Republic"] = "Slovakia"
EC$location_name[EC$location_name == "Republic of Latvia"] = "Latvia"
EC$location_name[EC$location_name == "Brunei Darussalam"] = "Brunei"
EC$location_name[EC$location_name == "Republic of Austria"] = "Austria"
EC$location_name[EC$location_name == "Republic of Korea"] = "South Korea"
EC$location_name[EC$location_name == "Republic of Moldova"] = "Moldova"
EC$location_name[EC$location_name == "Republic of Estonia"] = "Estonia"
EC$location_name[EC$location_name == "Republic of Lithuania"] = "Lithuania"
EC$location_name[EC$location_name == "Republic of Cyprus"] = "Cyprus"
EC$location_name[EC$location_name == "Republic of Singapore"] = "Singapore"
EC$location_name[EC$location_name == "Republic of Finland"] = "Finland"
EC$location_name[EC$location_name == "Republic of Serbia"] = "Serbia"
EC$location_name[EC$location_name == "Republic of Poland"] = "Poland"
EC$location_name[EC$location_name == "Kingdom of Sweden"] = "Sweden"
EC$location_name[EC$location_name == "Kingdom of Denmark"] = "Denmark"
EC$location_name[EC$location_name == "Federal Republic of Germany"] = "Germany"
EC$location_name[EC$location_name == "Kingdom of Belgium"] = "Belgium"
EC$location_name[EC$location_name == "Republic of Iceland"] = "Iceland"
EC$location_name[EC$location_name == "Russian Federation"] = "Russia"
EC$location_name[EC$location_name == "Republic of Slovenia"] = "Slovenia"
EC$location_name[EC$location_name == "Republic of Italy"] = "Italy"
EC$location_name[EC$location_name == "Portuguese Republic"] = "Portugal"
EC$location_name[EC$location_name == "Commonwealth of the Bahamas"] = "Bahamas"
EC$location_name[EC$location_name == "State of Israel"] = "Israel"
EC$location_name[EC$location_name == "French Republic"] = "France"
EC$location_name[EC$location_name == "Republic of Cuba"] = "Cuba"
EC$location_name[EC$location_name == "Republic of Haiti"] = "Haiti"
EC$location_name[EC$location_name == "Kingdom of the Netherlands"] = "Netherlands"
EC$location_name[EC$location_name == "Federative Republic of Brazil"] = "Brazil"
EC$location_name[EC$location_name == "Republic of Belarus"] = "Belarus"
EC$location_name[EC$location_name == "Republic of Malta"] = "Malta"
EC$location_name[EC$location_name == "Dominican Republic"] = "Dominican Republic"
EC$location_name[EC$location_name == "Republic of Guyana"] = "Guyana"
EC$location_name[EC$location_name == "Republic of Paraguay"] = "Paraguay"
EC$location_name[EC$location_name == "Republic of Yemen"] = "Yemen"
EC$location_name[EC$location_name == "Commonwealth of Dominica"] = "Dominica"
EC$location_name[EC$location_name == "Republic of Suriname"] = "Suriname"
EC$location_name[EC$location_name == "Grand Duchy of Luxembourg"] = "Luxembourg"
EC$location_name[EC$location_name == "Saint Vincent and the Grenadines"] = "Saint Vincent and the Grenadines"
EC$location_name[EC$location_name == "Republic of Trinidad and Tobago"] = "Trinidad and Tobago"
EC$location_name[EC$location_name == "Kingdom of Norway"] = "Norway"
EC$location_name[EC$location_name == "Eastern Republic of Uruguay"] = "Uruguay"
EC$location_name[EC$location_name == "Saint Lucia"] = "Saint Lucia"
EC$location_name[EC$location_name == "United Kingdom of Great Britain and Northern Ireland"] = "United Kingdom"
EC$location_name[EC$location_name == "Argentine Republic"] = "Argentina"
EC$location_name[EC$location_name == "Republic of Chile"] = "Chile"
EC$location_name[EC$location_name == "Republic of Honduras"] = "Honduras"
EC$location_name[EC$location_name == "Republic of Ecuador"] = "Ecuador"
EC$location_name[EC$location_name == "Republic of Peru"] = "Peru"
EC$location_name[EC$location_name == "Republic of Nicaragua"] = "Nicaragua"
EC$location_name[EC$location_name == "Plurinational State of Bolivia"] = "Bolivia"
EC$location_name[EC$location_name == "Bolivarian Republic of Venezuela"] = "Venezuela"
EC$location_name[EC$location_name == "United Mexican States"] = "Mexico"
EC$location_name[EC$location_name == "State of Kuwait"] = "Kuwait"
EC$location_name[EC$location_name == "People's Democratic Republic of Algeria"] = "Algeria"
EC$location_name[EC$location_name == "Arab Republic of Egypt"] = "Egypt"
EC$location_name[EC$location_name == "Palestine"] = "Palestine"
EC$location_name[EC$location_name == "Republic of Turkey"] = "Turkey"
EC$location_name[EC$location_name == "State of Qatar"] = "Qatar"
EC$location_name[EC$location_name == "Republic of Guatemala"] = "Guatemala"
EC$location_name[EC$location_name == "Islamic Republic of Iran"] = "Iran"
EC$location_name[EC$location_name == "Lebanese Republic"] = "Lebanon"
EC$location_name[EC$location_name == "Kingdom of Saudi Arabia"] = "Saudi Arabia"
EC$location_name[EC$location_name == "Central African Republic"] = "Central African Republic"
EC$location_name[EC$location_name == "Republic of Panama"] = "Panama"
EC$location_name[EC$location_name == "Islamic Republic of Pakistan"] = "Pakistan"
EC$location_name[EC$location_name == "Sultanate of Oman"] = "Oman"
EC$location_name[EC$location_name == "Republic of Iraq"] = "Iraq"
EC$location_name[EC$location_name == "Hashemite Kingdom of Jordan"] = "Jordan"
EC$location_name[EC$location_name == "State of Libya"] = "Libya"
EC$location_name[EC$location_name == "Kingdom of Bahrain"] = "Bahrain"
EC$location_name[EC$location_name == "Kingdom of Morocco"] = "Morocco"
EC$location_name[EC$location_name == "Republic of Equatorial Guinea"] = "Equatorial Guinea"
EC$location_name[EC$location_name == "Republic of Costa Rica"] = "Costa Rica"
EC$location_name[EC$location_name == "Syrian Arab Republic"] = "Syria"
EC$location_name[EC$location_name == "People's Republic of Bangladesh"] = "Bangladesh"
EC$location_name[EC$location_name == "Republic of Tunisia"] = "Tunisia"
EC$location_name[EC$location_name == "Republic of El Salvador"] = "El Salvador"
EC$location_name[EC$location_name == "Democratic Republic of the Congo"] = "Democratic Republic of Congo"
EC$location_name[EC$location_name == "Republic of Angola"] = "Angola"
EC$location_name[EC$location_name == "Republic of India"] = "India"
EC$location_name[EC$location_name == "Kingdom of Bhutan"] = "Bhutan"
EC$location_name[EC$location_name == "United Arab Emirates"] = "United Arab Emirates"
EC$location_name[EC$location_name == "Republic of Colombia"] = "Colombia"
EC$location_name[EC$location_name == "Jamaica"] = "Jamaica"
EC$location_name[EC$location_name == "Islamic Republic of Afghanistan"] = "Afghanistan"
EC$location_name[EC$location_name == "Republic of the Congo"] = "Republic of Congo"
EC$location_name[EC$location_name == "Republic of Burundi"] = "Burundi"
EC$location_name[EC$location_name == "Federal Democratic Republic of Nepal"] = "Nepal"
EC$location_name[EC$location_name == "Gabonese Republic"] = "Gabon"
EC$location_name[EC$location_name == "Republic of Mozambique"] = "Mozambique"
EC$location_name[EC$location_name == "State of Eritrea"] = "Eritrea"
EC$location_name[EC$location_name == "Union of the Comoros"] = "Comoros"
EC$location_name[EC$location_name == "Republic of Djibouti"] = "Djibouti"
EC$location_name[EC$location_name == "Republic of Madagascar"] = "Madagascar"
EC$location_name[EC$location_name == "Republic of Malawi"] = "Malawi"
EC$location_name[EC$location_name == "Republic of Mauritius"] = "Mauritius"
EC$location_name[EC$location_name == "Federal Democratic Republic of Ethiopia"] = "Ethiopia"
EC$location_name[EC$location_name == "Republic of Kenya"] = "Kenya"
EC$location_name[EC$location_name == "Republic of Namibia"] = "Namibia"
EC$location_name[EC$location_name == "Federal Republic of Somalia"] = "Somalia"
EC$location_name[EC$location_name == "Republic of Seychelles"] = "Seychelles"
EC$location_name[EC$location_name == "Republic of Rwanda"] = "Rwanda"
EC$location_name[EC$location_name == "United Republic of Tanzania"] = "Tanzania"
EC$location_name[EC$location_name == "Republic of Uganda"] = "Uganda"
EC$location_name[EC$location_name == "Republic of Botswana"] = "Botswana"
EC$location_name[EC$location_name == "Republic of Zambia"] = "Zambia"
EC$location_name[EC$location_name == "Republic of Cabo Verde"] = "Cabo Verde"
EC$location_name[EC$location_name == "Burkina Faso"] = "Burkina Faso"
EC$location_name[EC$location_name == "Kingdom of Eswatini"] = "Eswatini"
EC$location_name[EC$location_name == "Republic of Zimbabwe"] = "Zimbabwe"
EC$location_name[EC$location_name == "Kingdom of Lesotho"] = "Lesotho"
EC$location_name[EC$location_name == "Republic of Cameroon"] = "Cameroon"
EC$location_name[EC$location_name == "Republic of Ghana"] = "Ghana"
EC$location_name[EC$location_name == "Republic of C??te d'Ivoire"] = "Ivory Coast"
EC$location_name[EC$location_name == "Republic of South Africa"] = "South Africa"
EC$location_name[EC$location_name == "Republic of Benin"] = "Benin"
EC$location_name[EC$location_name == "Republic of Guinea-Bissau"] = "Guinea-Bissau"
EC$location_name[EC$location_name == "Republic of Mali"] = "Mali"
EC$location_name[EC$location_name == "Republic of Chad"] = "Chad"
EC$location_name[EC$location_name == "Republic of the Gambia"] = "Gambia"
EC$location_name[EC$location_name == "United States Virgin Islands"] = "U.S. Virgin Islands"
EC$location_name[EC$location_name == "Togolese Republic"] = "Togo"
EC$location_name[EC$location_name == "Republic of Liberia"] = "Liberia"
EC$location_name[EC$location_name == "American Samoa"] = "American Samoa"
EC$location_name[EC$location_name == "Greenland"] = "Greenland"
EC$location_name[EC$location_name == "Republic of Sierra Leone"] = "Sierra Leone"
EC$location_name[EC$location_name == "Republic of South Sudan"] = "South Sudan"
EC$location_name[EC$location_name == "Tuvalu"] = "Tuvalu"
EC$location_name[EC$location_name == "Democratic Republic of Sao Tome and Principe"] = "Sao Tome and Principe"
EC$location_name[EC$location_name == "Republic of Guinea"] = "Guinea"
EC$location_name[EC$location_name == "Guam"] = "Guam"
EC$location_name[EC$location_name == "Republic of Senegal"] = "Senegal"
EC$location_name[EC$location_name == "Cook Islands"] = "Cook Islands"
EC$location_name[EC$location_name == "Islamic Republic of Mauritania"] = "Mauritania"
EC$location_name[EC$location_name == "Saint Kitts and Nevis"] = "Saint Kitts and Nevis"
EC$location_name[EC$location_name == "Bermuda"] = "Bermuda"
EC$location_name[EC$location_name == "Puerto Rico"] = "Puerto Rico"
EC$location_name[EC$location_name == "Republic of San Marino"] = "San Marino"
EC$location_name[EC$location_name == "Principality of Monaco"] = "Monaco"
EC$location_name[EC$location_name == "Northern Mariana Islands"] = "Northern Mariana Islands"
EC$location_name[EC$location_name == "Federal Republic of Nigeria"] = "Nigeria"
EC$location_name[EC$location_name == "Republic of Nauru"] = "Nauru"
EC$location_name[EC$location_name == "Republic of Niue"] = "Niue"
EC$location_name[EC$location_name == "Republic of the Niger"] = "Niger"
EC$location_name[EC$location_name == "Tokelau"] = "Tokelau"
EC$location_name[EC$location_name == "Republic of Palau"] = "Palau"
EC$location_name[EC$location_name == "Republic of Sudan"] = "Sudan"





### 合并SDI与ASR数据
EC_ASR_SDI <- merge(EC,SDI,by=c('location_name','year'))

EC_ASR_SDI$location_name <- factor(EC_ASR_SDI$location_name, 
                              levels=order_SDI$V1, 
                              ordered=TRUE) ## location图例按照我们的顺序排列

##开始作图，主变量为ASR以及SDI,图形的颜色和形状根据不同区域来调整即可
### 同时以所有数据画出拟合曲线
ggplot(EC_ASR_SDI, aes(SDI,ASR)) + geom_point(aes(color = location_name, shape= location_name))+
  scale_shape_manual(values = 1:183) + 
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) 


#### figure 4B
EC <- read.csv('EC_nation.csv',header = T)  ## 读取我们的数据
SDI <- read.csv('GBD_SDI_1990-2019.csv',header = T)

## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式，
SDI <- melt(SDI,id='Location')
SDI$variable <- as.numeric(gsub('\\X',replacement = '', SDI$variable))
names(SDI) <- c('location','year','SDI')
SDI <- SDI[SDI$year== 2021,] ###只取2019年的数据

### 获取EC的2019的标准发病率数据
EC <- subset(EC, EC$age=='Age-standardized' & 
               EC$metric== 'Rate' &
               EC$measure=='Incidence' &
               EC$year=='2019')
EC <- EC[,c(2,7,8)]
names(EC)[3] <- 'ASR'

### 调整SDI与EC里location命名一致
EC$location[EC$location == "Democratic People's Republic of Korea"] = 'North Korea'
EC$location[EC$location == 'Russian Federation'] = 'Russia'
EC$location[EC$location == 'United Kingdom'] = 'UK'
EC$location[EC$location == "Iran (Islamic Republic of)"] = 'Iran'
EC$location[EC$location == "Taiwan"] = 'Taiwan (Province of China)'
EC$location[EC$location == "Republic of Korea"] = 'South Korea'
EC$location[EC$location == "United Republic of Tanzania"] = 'Tanzania'
EC$location[EC$location == "C么te d'Ivoire"] = 'Saint Helena'
EC$location[EC$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
EC$location[EC$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
EC$location[EC$location == "Czechia"] = 'Czech Republic'
EC$location[EC$location == "Republic of Moldova"] = 'Moldova'
EC$location[EC$location == "Viet Nam"] = 'Vietnam'
EC$location[EC$location == "Lao People's Democratic Republic"] = 'Laos'
EC$location[EC$location == "Syrian Arab Republic"] = 'Syria'
EC$location[EC$location == "North Macedonia"] = 'Macedonia'
EC$location[EC$location == "Brunei Darussalam"] = 'Brunei'
EC$location[EC$location == "Gambia"] = 'The Gambia'
EC$location[EC$location == "United States of America"] = 'USA'
EC$location[EC$location == "Micronesia (Federated States of)"] = 'Federated States of Micronesia'
EC$location[EC$location == "Bahamas"] = 'The Bahamas'
EC$location[EC$location == "United States Virgin Islands"] = 'Virgin Islands'
EC$location[EC$location == "Macedonia"] = 'North Macedonia'
EC$location[EC$location == 'Democratic Republic of the Congo'] = 'DR Congo'
EC$location[EC$location == 'Congo'] = 'Congo (Brazzaville)'
EC$location[EC$location == 'Cabo Verde'] = 'Cape Verde'

###合并两者数据
EC_ASR_SDI_2021 <- EC_ASR_SDI[EC_ASR_SDI$year==2021,]

##作图
ggplot(EC_ASR_SDI_2021, aes(SDI,ASR,label=location_name)) + 
  geom_point(aes(color = location_name)) + 
  geom_text_repel(aes(color = location_name),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
   theme(legend.position="none")

