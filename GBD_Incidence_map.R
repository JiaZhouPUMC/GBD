setwd('/Users/zhoujia/Desktop/AA1030') ##设置工作路径
#  install.packages('ggmap')
#install.packages('rgdal')
#install.packages("sf")
#  install.packages('maps')
#  install.packages('dplyr')
#setwd('/Users/zhoujia/Downloads')
#install.packages('rgdal_1.6-7.tar.gz', repos = NULL, type = 'source')

library(ggmap)
library(sf)
library(maps)
library(dplyr)

EC <- read.csv('world204.csv',header = T)  ## 读取我们的数据
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$age_name=='Age-standardized' & 
                     EC$metric_name== 'Rate' &
                     EC$measure_name=='Incidence') ## 获取2021年EC年龄校正后发病率
ASR_2021 <- ASR_2021[,c(4,14,15,16)]
ASR_2021$val <- round(ASR_2021$val,1) ###保留一位小数点
ASR_2021$lower <- round(ASR_2021$lower,1) ###保留一位小数点
ASR_2021$upper <- round(ASR_2021$upper,1) ###保留一位小数点

####  map for ASR
worldData <- map_data('world')
country_asr <- ASR_2021
country_asr$location <- as.character(country_asr$location) 
###以下代码的目的是让country_asr$location的国家名称与worldData的国家名称一致
### 这样才能让数据映射到地图上
country_asr$location[country_asr$location == 'Arab Republic of Egypt'] = 'Egypt'
country_asr$location[country_asr$location == 'Argentine Republic'] = 'Argentina'
country_asr$location[country_asr$location == "Bolivarian Republic of Venezuela"] = 'Venezuela'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'United Kingdom of Great Britain and Northern Ireland'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Islamic Republic of Iran"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "Republic of C??te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Plurinational State of Bolivia"] = 'Bolivia'
country_asr$location[country_asr$location == "Bolivarian Republic of Venezuela"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Federated States of Micronesia"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Republic of Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Republic of C??te d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Socialist Republic of Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Socialist Republic of Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "French"] = 'France'
country_asr$location[country_asr$location == "Dominican"] = 'Dominican Republic'




# 假设 country_asr$location 中已有国家名称
country_asr$location <- gsub("^Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Federal Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Islamic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Kingdom of ", "", country_asr$location)
country_asr$location <- gsub("^the ", "", country_asr$location)
country_asr$location <- gsub("^Principality of ", "", country_asr$location)
country_asr$location <- gsub("^Independent State of ", "", country_asr$location)
country_asr$location <- gsub("^Federal Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Federative Republic of ", "", country_asr$location)
country_asr$location <- gsub("^People's Republic of ", "", country_asr$location)
country_asr$location <- gsub("^People's Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^State of ", "", country_asr$location)
country_asr$location <- gsub("^Union of ", "", country_asr$location)
country_asr$location <- gsub(" Republic$", "", country_asr$location)
country_asr$location <- gsub("^Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Commonwealth of ", "", country_asr$location)
country_asr$location <- gsub(" Confederation$", "", country_asr$location)


country_asr$location[country_asr$location == "Central African"] = "Central African Republic"
country_asr$location[country_asr$location == "Congo"] = "Republic of the Congo"  # 或者 "Democratic Republic of the Congo"
country_asr$location[country_asr$location == "Czech"] = "Czech Republic"
country_asr$location[country_asr$location == "Democratic Socialist Republic of Sri Lanka"] = "Sri Lanka"
country_asr$location[country_asr$location == "Eastern Republic of Uruguay"] = "Uruguay"
country_asr$location[country_asr$location == "Gabonese"] = "Gabon"
country_asr$location[country_asr$location == "Grand Duchy of Luxembourg"] = "Luxembourg"
country_asr$location[country_asr$location == "Hellenic"] = "Greece"
country_asr$location[country_asr$location == "Kyrgyz"] = "Kyrgyzstan"
country_asr$location[country_asr$location == "Lebanese"] = "Lebanon"
country_asr$location[country_asr$location == "Portuguese"] = "Portugal"
country_asr$location[country_asr$location == "Slovak"] = "Slovakia"
country_asr$location[country_asr$location == "Swiss"] = "Switzerland"
country_asr$location[country_asr$location == "Sultanate of Oman"] = "Oman"
country_asr$location[country_asr$location == "Togolese"] = "Togo"

country_asr$location[country_asr$location == "Cabo Verde"] = "Cape Verde"
country_asr$location[country_asr$location == "Eastern Republic of Uruguay"] = "Uruguay"
country_asr$location[country_asr$location == "Hashemite Kingdom of Jordan"] = "Jordan"
country_asr$location[country_asr$location == "United Mexican States"] = "Mexico"
country_asr$location[country_asr$location == "Republic of the Congo"] = "Republic of Congo"


# 找到"Republic of Congo"这一行的索引
rep_congo_index <- which(country_asr$location == "Republic of Congo")

# 复制这一行的所有信息
new_row <- country_asr[rep_congo_index, ]

# 修改location字段
new_row$location <- "Democratic Republic of the Congo"

# 将新行添加到country_asr数据框中
country_asr <- rbind(country_asr, new_row)


# 特殊情况处理，例如 "Arab Republic of Egypt"
country_asr$location[country_asr$location == 'Arab Republic of Egypt'] <- 'Egypt'

# 处理其他特殊情况（如有需要）
country_asr$location[country_asr$location == 'Saint Helena'] <- 'Ivory Coast'




locations_to_remove <- c("Barbuda", "Grenadines", "Nevis", "Tobago")
# 删除指定 location 重复的行，只保留唯一行
country_asr <- country_asr %>%
  filter(!(location %in% locations_to_remove)) %>%
  distinct()  # 保留唯一行

worldData <- map_data('world')
unique(worldData$region)
unique_regions <- unique(worldData$region)
num_unique_regions <- length(unique_regions)
print(num_unique_regions)
unique(country_asr$location)
# 统计 region 列的出现次数
region_counts <- as.data.frame(table(worldData$region))
print(region_counts)
# 统计 location 列的出现次数
location_counts <- as.data.frame(table(country_asr$location))
print(location_counts)
# 导出 region_counts 为 CSV 文件
write.csv(region_counts, file = "region_counts.csv", row.names = FALSE)
# 导出 location_counts 为 CSV 文件
write.csv(location_counts, file = "location_counts.csv", row.names = FALSE)

# 查看未匹配的值
non_matching_regions <- setdiff(unique(worldData$region), unique(country_asr$location))
non_matching_locations <- setdiff(unique(country_asr$location), unique(worldData$region))

print(non_matching_regions)
print(non_matching_locations)



total <- full_join(worldData,country_asr,by = c('region'='location'))
write.csv(total, file = "total.csv", row.names = FALSE)

total <- total %>% mutate(val2 = cut(val, breaks = c(250, 300, 350, 400, 450, 500, 550, 600, Inf),
                                     labels = c("250~300", "300~350", "350~400", 
                                                "400~450", "450~500", "500~550", 
                                                "550~600", "600+"),
                                     include.lowest = TRUE, right = TRUE))
p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group = group, fill=val2),
                       colour="black", size = .2) + 
  scale_fill_brewer(palette = "Reds") +
  theme_void() + labs(x="", y="") +
  guides(fill = guide_legend(title='Age-Standardized Incidence Rate')) +
  theme(legend.position = 'right')

p2

p2 <- p + geom_polygon(data = total, 
                       aes(x = long, y = lat, group = group, fill = val2),
                       colour = "black", size = .2) + 
  scale_fill_brewer(palette = "Reds") +
  theme_void() +
  labs(x = "", y = "") +
  guides(fill = guide_legend(title = 'Age-standardized incidence rate\nper 100,000 individuals\nfor 2021', title.position = "top")) +
  theme(legend.position = c(0.94, 0.6),  # 调整图例位置，数值范围从0到1
        legend.key.size = unit(0.6, "cm"),  # 调整图例大小
        legend.title = element_text(size = 8),  # 图例标题字体大小
        legend.text = element_text(size = 10))  # 图例文本字体大小

p2
### case change
####   case change MAP
EC <- read.csv('worldyear.csv',header = T)  ## 读取我们的数据
case_2021 <- subset(EC,EC$year==2021 & 
                     EC$age_name=='All ages' & 
                      EC$sex_name=='Both'&
                     EC$metric_name== 'Number' &
                     EC$measure_name=='Incidence') ## 获取2021年EC发病数

case_1990 <- subset(EC,EC$year==1990 & 
                      EC$age_name=='All ages' & 
                      EC$sex_name=='Both'&
                      EC$metric_name== 'Number' &
                      EC$measure_name=='Incidence') ## 获取1990年EC发病数

case_1990 <- case_1990[,c(4,14)]
case_2021 <- case_2021[,c(4,14)]
names(case_1990) <- c('location','case_1990')
names(case_2021) <- c('location','case_2021')
country_asr <- merge(case_1990, case_2021, by='location')
country_asr$val <- (country_asr$case_2021-country_asr$case_1990)/country_asr$case_1990*100  ### 获取我们的结果

country_asr$location <- as.character(country_asr$location) 
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'

country_asr$location[country_asr$location == 'Arab Republic of Egypt'] = 'Egypt'
country_asr$location[country_asr$location == 'Argentine Republic'] = 'Argentina'
country_asr$location[country_asr$location == "Bolivarian Republic of Venezuela"] = 'Venezuela'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'United Kingdom of Great Britain and Northern Ireland'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Islamic Republic of Iran"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "Republic of C??te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Plurinational State of Bolivia"] = 'Bolivia'
country_asr$location[country_asr$location == "Bolivarian Republic of Venezuela"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Federated States of Micronesia"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Republic of Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Republic of C??te d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Socialist Republic of Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Socialist Republic of Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "French"] = 'France'
country_asr$location[country_asr$location == "Dominican"] = 'Dominican Republic'




# 假设 country_asr$location 中已有国家名称
country_asr$location <- gsub("^Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Federal Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Islamic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Kingdom of ", "", country_asr$location)
country_asr$location <- gsub("^the ", "", country_asr$location)
country_asr$location <- gsub("^Principality of ", "", country_asr$location)
country_asr$location <- gsub("^Independent State of ", "", country_asr$location)
country_asr$location <- gsub("^Federal Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Federative Republic of ", "", country_asr$location)
country_asr$location <- gsub("^People's Republic of ", "", country_asr$location)
country_asr$location <- gsub("^People's Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^State of ", "", country_asr$location)
country_asr$location <- gsub("^Union of ", "", country_asr$location)
country_asr$location <- gsub(" Republic$", "", country_asr$location)
country_asr$location <- gsub("^Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Commonwealth of ", "", country_asr$location)
country_asr$location <- gsub(" Confederation$", "", country_asr$location)


country_asr$location[country_asr$location == "Central African"] = "Central African Republic"
country_asr$location[country_asr$location == "Congo"] = "Republic of the Congo"  # 或者 "Democratic Republic of the Congo"
country_asr$location[country_asr$location == "Czech"] = "Czech Republic"
country_asr$location[country_asr$location == "Democratic Socialist Republic of Sri Lanka"] = "Sri Lanka"
country_asr$location[country_asr$location == "Eastern Republic of Uruguay"] = "Uruguay"
country_asr$location[country_asr$location == "Gabonese"] = "Gabon"
country_asr$location[country_asr$location == "Grand Duchy of Luxembourg"] = "Luxembourg"
country_asr$location[country_asr$location == "Hellenic"] = "Greece"
country_asr$location[country_asr$location == "Kyrgyz"] = "Kyrgyzstan"
country_asr$location[country_asr$location == "Lebanese"] = "Lebanon"
country_asr$location[country_asr$location == "Portuguese"] = "Portugal"
country_asr$location[country_asr$location == "Slovak"] = "Slovakia"
country_asr$location[country_asr$location == "Swiss"] = "Switzerland"
country_asr$location[country_asr$location == "Sultanate of Oman"] = "Oman"
country_asr$location[country_asr$location == "Togolese"] = "Togo"

country_asr$location[country_asr$location == "Cabo Verde"] = "Cape Verde"
country_asr$location[country_asr$location == "Eastern Republic of Uruguay"] = "Uruguay"
country_asr$location[country_asr$location == "Hashemite Kingdom of Jordan"] = "Jordan"
country_asr$location[country_asr$location == "United Mexican States"] = "Mexico"
country_asr$location[country_asr$location == "Republic of the Congo"] = "Republic of Congo"


# 找到"Republic of Congo"这一行的索引
rep_congo_index <- which(country_asr$location == "Republic of Congo")

# 复制这一行的所有信息
new_row <- country_asr[rep_congo_index, ]

# 修改location字段
new_row$location <- "Democratic Republic of the Congo"

# 将新行添加到country_asr数据框中
country_asr <- rbind(country_asr, new_row)


# 特殊情况处理，例如 "Arab Republic of Egypt"
country_asr$location[country_asr$location == 'Arab Republic of Egypt'] <- 'Egypt'

# 处理其他特殊情况（如有需要）
country_asr$location[country_asr$location == 'Saint Helena'] <- 'Ivory Coast'

country_asr$location[country_asr$location == 'Arab Republic of Egypt'] <- 'Egypt'
country_asr$location[country_asr$location == 'Dominican'] <- 'Dominican Republic'
country_asr$location[country_asr$location == 'French'] <- 'France'
country_asr$location[country_asr$location == 'the Bahamas'] <- 'Bahamas'
country_asr$location[country_asr$location == 'the Comoros'] <- 'Comoros'
country_asr$location[country_asr$location == 'the Congo'] <- 'Republic of Congo'
country_asr$location[country_asr$location == 'Eswatini'] <- 'Swaziland'  # 如果需要
country_asr$location[country_asr$location == 'C??te d\'Ivoire'] <- 'Ivory Coast'
country_asr$location[country_asr$location == 'the Congo'] <- 'Democratic Republic of the Congo'



locations_to_remove <- c("Barbuda", "Grenadines", "Nevis", "Tobago")
# 删除指定 location 重复的行，只保留唯一行
country_asr <- country_asr %>%
  filter(!(location %in% locations_to_remove)) %>%
  distinct()  # 保留唯一行

worldData <- map_data('world')
unique(worldData$region)
unique_regions <- unique(worldData$region)
num_unique_regions <- length(unique_regions)
print(num_unique_regions)
unique(country_asr$location)
# 统计 region 列的出现次数
region_counts <- as.data.frame(table(worldData$region))
print(region_counts)
# 统计 location 列的出现次数
location_counts <- as.data.frame(table(country_asr$location))
print(location_counts)
# 导出 region_counts 为 CSV 文件
write.csv(region_counts, file = "region_counts.csv", row.names = FALSE)
# 导出 location_counts 为 CSV 文件
write.csv(location_counts, file = "location_counts.csv", row.names = FALSE)

# 查看未匹配的值
non_matching_regions <- setdiff(unique(worldData$region), unique(country_asr$location))
non_matching_locations <- setdiff(unique(country_asr$location), unique(worldData$region))

print(non_matching_regions)
print(non_matching_locations)

total <- full_join(worldData,country_asr,by = c('region'='location'))
write.csv(total, file = "total.csv", row.names = FALSE)



total <- full_join(worldData,country_asr,by = c('region'='location'))

p <- ggplot()

total <- total %>% mutate(val2 = cut(val, breaks = c(-60, -34.81, 0, 50, 100, 200, 300, 642.55),
                                     labels = c("30% to 60% decrease", "<30% decrease", "<50% increase",
                                                "50% to 100% increase", "100% to 200% increase", 
                                                "200% to 300% increase", ">300% increase"),
                                     include.lowest = TRUE, right = TRUE))

p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group = group, fill=val2),
                       colour="black", size = 0.2) + 
  scale_fill_manual(values = c("#006400", "#66CD00", "#FFE4C4", "#FF7256", "#FF4040", "#CD3333", "#8B2323")) +
  theme_void() + labs(x="", y="") +
  guides(fill = guide_legend(title='Change in cases from 1990-2021')) +
  theme(legend.position = c(0.93, 0.6),  # 调整图例位置，数值范围从0到1
        legend.key.size = unit(0.6, "cm"),  # 调整图例大小
        legend.title = element_text(size = 8),  # 图例标题字体大小
        legend.text = element_text(size = 10))  # 图例文本字体大小
p2



## map for EAPC
## EAPC
EAPC <- subset(EC, EC$age_name=='Age-standardized' & 
                 EC$metric_name== 'Rate' &
                 EC$sex_name=='Both'&
               EC$measure_name=='Incidence')

EAPC <- EAPC[,c(4,13,14)]

country <- case_2021$location  ###获取国家名称
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
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
country_asr <- EAPC_cal

country_asr$location <- as.character(country_asr$location) 

country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'

country_asr$location[country_asr$location == 'Arab Republic of Egypt'] = 'Egypt'
country_asr$location[country_asr$location == 'Argentine Republic'] = 'Argentina'
country_asr$location[country_asr$location == "Bolivarian Republic of Venezuela"] = 'Venezuela'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'United Kingdom of Great Britain and Northern Ireland'] = 'UK'
country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == "Islamic Republic of Iran"] = 'Iran'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asr$location[country_asr$location == "Republic of C??te d'Ivoire"] = 'Saint Helena'
country_asr$location[country_asr$location == "Plurinational State of Bolivia"] = 'Bolivia'
country_asr$location[country_asr$location == "Bolivarian Republic of Venezuela"] = 'Venezuela'
country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
country_asr$location[country_asr$location == "Federated States of Micronesia"] = 'Micronesia'
country_asr$location[country_asr$location == "Macedonia"] = 'North Macedonia'
country_asr$location[country_asr$location == "Republic of Trinidad and Tobago"] = 'Trinidad'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
country_asr$location[country_asr$location == "Republic of C??te d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
country_asr$location[country_asr$location == "Socialist Republic of Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Socialist Republic of Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "French"] = 'France'
country_asr$location[country_asr$location == "Dominican"] = 'Dominican Republic'




# 假设 country_asr$location 中已有国家名称
country_asr$location <- gsub("^Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Federal Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Islamic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Kingdom of ", "", country_asr$location)
country_asr$location <- gsub("^the ", "", country_asr$location)
country_asr$location <- gsub("^Principality of ", "", country_asr$location)
country_asr$location <- gsub("^Independent State of ", "", country_asr$location)
country_asr$location <- gsub("^Federal Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Federative Republic of ", "", country_asr$location)
country_asr$location <- gsub("^People's Republic of ", "", country_asr$location)
country_asr$location <- gsub("^People's Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^State of ", "", country_asr$location)
country_asr$location <- gsub("^Union of ", "", country_asr$location)
country_asr$location <- gsub(" Republic$", "", country_asr$location)
country_asr$location <- gsub("^Democratic Republic of ", "", country_asr$location)
country_asr$location <- gsub("^Commonwealth of ", "", country_asr$location)
country_asr$location <- gsub(" Confederation$", "", country_asr$location)


country_asr$location[country_asr$location == "Central African"] = "Central African Republic"
country_asr$location[country_asr$location == "Congo"] = "Republic of the Congo"  # 或者 "Democratic Republic of the Congo"
country_asr$location[country_asr$location == "Czech"] = "Czech Republic"
country_asr$location[country_asr$location == "Democratic Socialist Republic of Sri Lanka"] = "Sri Lanka"
country_asr$location[country_asr$location == "Eastern Republic of Uruguay"] = "Uruguay"
country_asr$location[country_asr$location == "Gabonese"] = "Gabon"
country_asr$location[country_asr$location == "Grand Duchy of Luxembourg"] = "Luxembourg"
country_asr$location[country_asr$location == "Hellenic"] = "Greece"
country_asr$location[country_asr$location == "Kyrgyz"] = "Kyrgyzstan"
country_asr$location[country_asr$location == "Lebanese"] = "Lebanon"
country_asr$location[country_asr$location == "Portuguese"] = "Portugal"
country_asr$location[country_asr$location == "Slovak"] = "Slovakia"
country_asr$location[country_asr$location == "Swiss"] = "Switzerland"
country_asr$location[country_asr$location == "Sultanate of Oman"] = "Oman"
country_asr$location[country_asr$location == "Togolese"] = "Togo"

country_asr$location[country_asr$location == "Cabo Verde"] = "Cape Verde"
country_asr$location[country_asr$location == "Eastern Republic of Uruguay"] = "Uruguay"
country_asr$location[country_asr$location == "Hashemite Kingdom of Jordan"] = "Jordan"
country_asr$location[country_asr$location == "United Mexican States"] = "Mexico"
country_asr$location[country_asr$location == "Republic of the Congo"] = "Republic of Congo"


# 找到"Republic of Congo"这一行的索引
rep_congo_index <- which(country_asr$location == "Republic of Congo")

# 复制这一行的所有信息
new_row <- country_asr[rep_congo_index, ]

# 修改location字段
new_row$location <- "Democratic Republic of the Congo"

# 将新行添加到country_asr数据框中
country_asr <- rbind(country_asr, new_row)


# 特殊情况处理，例如 "Arab Republic of Egypt"
country_asr$location[country_asr$location == 'Arab Republic of Egypt'] <- 'Egypt'

# 处理其他特殊情况（如有需要）
country_asr$location[country_asr$location == 'C??te d\'Ivoire'] <- 'Ivory Coast'
country_asr$location[country_asr$location == 'Arab Republic of Egypt'] <- 'Egypt'
country_asr$location[country_asr$location == 'Dominican'] <- 'Dominican Republic'
country_asr$location[country_asr$location == 'French'] <- 'France'
country_asr$location[country_asr$location == 'the Bahamas'] <- 'Bahamas'
country_asr$location[country_asr$location == 'the Comoros'] <- 'Comoros'
country_asr$location[country_asr$location == 'the Congo'] <- 'Democratic Republic of the Congo'
country_asr$location[country_asr$location == 'Eswatini'] <- 'Swaziland'  # 如果需要



worldData <- map_data('world')
total <- full_join(worldData,country_asr,by = c('region'='location'))



p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group=group, fill=EAPC),
                       colour="black", size=0.2) + 
  scale_fill_gradientn(colors = c("forestgreen", "white", "#FF7256"), 
                       values = scales::rescale(c(-1, 0, 1))) +  # 设定渐变颜色
  theme_void() + labs(x="", y="") +
  guides(fill = guide_colorbar(title='EAPC')) +
  theme(legend.position = c(0.93, 0.6),  # 调整图例位置，数值范围从0到1
        legend.key.size = unit(0.6, "cm"),  # 调整图例大小
        legend.title = element_text(size = 12),  # 图例标题字体大小
        legend.text = element_text(size = 10))  # 图例文本字体大小
p1
