setwd('/Users/zhoujia/Desktop/AA1029') ##???霉???路??
library(dplyr)
library(ggplot2)
library(ggsci)
library(factoextra)

# 创建一个空的数据框来存储合并的数据
combined_data <- data.frame()

# 循环读取和合并CSV文件
for (i in 1:9) {
  # 读取每个CSV文件
  file_name <- paste0(i, ".csv")  # 假设文件名为 file1.csv, file2.csv, ...
  data <- read.csv(file_name, header = TRUE)
  
  # 合并数据
  combined_data <- rbind(combined_data, data)
}
# 获取不同的 cause_name
cause_names <- unique(combined_data$cause_name)

# 创建一个列表来存储每个 cause_name 对应的数据框
data_list <- list()

# 根据 cause_name 分割数据
for (cause in cause_names) {
  data_list[[cause]] <- subset(combined_data, cause_name == cause)
}

# 可选：将每个数据框写入单独的 CSV 文件
for (cause in cause_names) {
  write.csv(data_list[[cause]], file = paste0(cause, ".csv"), row.names = FALSE)
}



EC <- read.csv('Alopecia areata.csv',header = T)  ## ??取???堑?????


### ???惴????实?EAPC????
EAPC <- subset(EC, EC$age_name=='Age-standardized' & 
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')

EAPC <- EAPC[,c(4,13,14)]

country_name <- subset(EC,EC$year==1990 & 
                        EC$sex_name=='Both'&
                         EC$age_name=='Age-standardized' & 
                         EC$metric_name== 'Rate' &
                         EC$measure_name=='Incidence') ###??取????????
country <- country_name$location_name  ###??取????????

EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=10),
                       UCI=rep(0,times=10),LCI=rep(0,times=10))
for (i in 1:10){
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
EAPC_incidence <- EAPC_cal[,c(1,2)]
names(EAPC_incidence)[2] <- 'EAPC_incidence'

# 加载必要的库
library(ggplot2)

# 确保 location 列是因子，并设置级别顺序
EAPC_cal$location <- factor(EAPC_cal$location, 
                            levels = c("Global", "America", "Europe", "Asia", "Africa", 
                                       "High SDI", "High-middle SDI", "Middle SDI", 
                                       "Low-middle SDI", "Low SDI"))

# 创建可视化
ggplot(EAPC_cal, aes(x = location, y = EAPC)) +
  geom_bar(stat = "identity", fill = "darkred") + # 以柱状图形式展示
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "#FFC0CB") + # 添加误差条
  labs(title = "Estimated Annual Percentage Change (EAPC) in Incidence",
       x = "Country/Region",
       y = "EAPC (%)") +
  theme_minimal() + # 使用简约主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中并放大
        axis.title.x = element_text(size = 14),  # x轴标签放大
        axis.title.y = element_text(size = 14)) # 旋转x轴标签







###?喜????????莩?一?????菁?
#cluster <- merge(EAPC_incidence,EAPC_Deaths, by='location')
data <- EAPC_incidence
data <- EAPC_incidence
data <- data.frame(EAPC = data$EAPC_incidence, row.names = data$location)  # 创建新数据框，保留行名

## ??????图??
df <- scale(data)
# Hierarchical clustering
res.hc <- hclust(dist(df,method="euclidean"),method = "complete")
###??图-?芽???
fviz_dend(res.hc, cex = 0.5, k = 4, color_labels_by_k = TRUE,
          k_colors=c('#0072B2', '#56B4E9', '#D55E00', 'darkred'),
          horiz = T, rect = TRUE)
###??图-???装?
fviz_dend(res.hc, cex = 1.4, k = 4, color_labels_by_k = TRUE,
          k_colors = c('#0072B2', '#56B4E9', '#D55E00', 'darkred'),
          horiz = TRUE, rect = TRUE,
          rect_fill = TRUE,
          ggtheme = theme_minimal(base_size = 14) +
            theme(axis.text = element_text(size = 14),  # 放大坐标轴文字
                  plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中并放大
                  legend.position = "bottom")) +
  labs(title = "Clustering of Alopecia Areata Incidence by Region or Country") +  # 添加标题
  theme(plot.margin = margin(1, 5, 1, 1, "cm"))


############复杂版柱状图########################
################################################
################################################
# 读取数据
EC <- read.csv('Alopecia areata.csv', header = TRUE)

# 筛选EAPC数据
EAPC <- subset(EC, age_name == 'Age-standardized' & 
                 metric_name == 'Rate' &
                 measure_name == 'Incidence')

EAPC <- EAPC[, c(4, 6,13, 14)]  # 添加性别列

# 获取国家名称
country_name <- subset(EC, year == 1990 & 
                         sex_name == 'Both' &
                         age_name == 'Age-standardized' & 
                         metric_name == 'Rate' &
                         measure_name == 'Incidence')
country <- country_name$location_name

# 初始化EAPC_cal数据框
EAPC_cal <- data.frame(location = rep(country, each = 3), 
                       sex = rep(c('Both','Male', 'Female'), times = length(country)),
                       EAPC = rep(0, times = 3 * length(country)),
                       UCI = rep(0, times = 3 * length(country)), 
                       LCI = rep(0, times = 3 * length(country)))

# 计算每个国家性别的EAPC
for (i in 1:30) {
  for (j in c('Both','Male', 'Female')) {
    country_cal <- as.character(EAPC_cal[i, 1])
    a <- subset(EAPC, location_name == country_cal & (sex_name == j | (j == 'Both' & sex_name %in% c('Male', 'Female'))))
    if (nrow(a) > 0) {
      a$y <- log(a$val)
      mod_simp_reg <- lm(y ~ year, data = a)
      estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100
      low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100
      high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100
      
      EAPC_cal[EAPC_cal$location == country_cal & EAPC_cal$sex == j, 'EAPC'] <- estimate
      EAPC_cal[EAPC_cal$location == country_cal & EAPC_cal$sex == j, 'LCI'] <- low
      EAPC_cal[EAPC_cal$location == country_cal & EAPC_cal$sex == j, 'UCI'] <- high
    }
  }
}




# 加载必要的库
library(ggplot2)

# 确保 location 和 sex 列是因子，并设置级别顺序
EAPC_cal$location <- factor(EAPC_cal$location, 
                            levels = c("Global", "America", "Europe", "Asia", "Africa", 
                                       "High SDI", "High-middle SDI", "Middle SDI", 
                                       "Low-middle SDI", "Low SDI"))
EAPC_cal$sex <- factor(EAPC_cal$sex, levels = c("Both", "Male", "Female"))

# 创建可视化
ggplot(EAPC_cal, aes(x = location, y = EAPC, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +  # 使用位置分隔的柱状图
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, position = position_dodge(0.9), color = "#FFC0CB") +  # 添加误差条
  labs(title = "Estimated Annual Percentage Change (EAPC) in Incidence by Region and Gender",
       x = "Country/Region",
       y = "EAPC") +
  theme_minimal() +  # 使用简约主题
  scale_fill_manual(values = c("Both" = "#B0C4DE", "Male" = "#ADD8E6", "Female" = "#F08080")) +  # 设置低饱和度纯色
  ylim(-0.6, 0.6) +  # 缩短纵坐标轴范围
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))  # 旋转x轴标签


############复杂版柱状图########################
################################################
################################################
# 读取数据
EC <- read.csv('Alopecia areata.csv', header = TRUE)

# 筛选EAPC数据
EAPC <- subset(EC, sex_name == 'Both' & 
                 metric_name == 'Rate' &
                 measure_name == 'Incidence')

EAPC <- EAPC[, c(4, 8,13, 14)]  # 添加性别列


order <- read.csv('order_year.csv', header = T)

# 设置地区顺序
EC$age_name <- factor(EC$age_name, 
                      levels = order$V1, 
                      ordered = TRUE)

EAPC <- EAPC %>% filter(age_name %in% order$V1)

# 获取国家名称
country_name <- subset(EC, year == 1990 & 
                         sex_name == 'Both' &
                         age_name == '20-24 years' & 
                         metric_name == 'Rate' &
                         measure_name == 'Incidence')
country <- country_name$location_name

# 初始化EAPC_cal数据框
EAPC_cal <- data.frame(location = rep(country, each = 9), 
                       sex = rep(c( "<20 years","20-24 years","25-29 years","30-34 years","35-39 years","40-44 years","45-49 years","50-69 years",
                                   "70+ years"), times = length(country)),
                       EAPC = rep(0, times = 9 * length(country)),
                       UCI = rep(0, times = 9 * length(country)), 
                       LCI = rep(0, times = 9 * length(country)))

# 计算每个国家和年龄段的EAPC
for (i in 1:90) {
  for (j in c("<20 years", "20-24 years", "25-29 years", "30-34 years", 
              "35-39 years", "40-44 years", "45-49 years", 
              "50-69 years", "70+ years")) {
    country_cal <- as.character(EAPC_cal[i, 1])
    a <- subset(EAPC, location_name == country_cal & age_name == j)  # 仅筛选当前国家和年龄段
    
    if (nrow(a) > 0) {
      a$y <- log(a$val)
      mod_simp_reg <- lm(y ~ year, data = a)
      estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100
      low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100
      high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100
      
      EAPC_cal[EAPC_cal$location == country_cal & EAPC_cal$sex == j, 'EAPC'] <- estimate
      EAPC_cal[EAPC_cal$location == country_cal & EAPC_cal$sex == j, 'LCI'] <- low
      EAPC_cal[EAPC_cal$location == country_cal & EAPC_cal$sex == j, 'UCI'] <- high
    }
  }
}

# 加载必要的库
library(ggplot2)

# 确保 location 和 sex 列是因子，并设置级别顺序
EAPC_cal$location <- factor(EAPC_cal$location, 
                            levels = c("Global", "America", "Europe", "Asia", "Africa", 
                                       "High SDI", "High-middle SDI", "Middle SDI", 
                                       "Low-middle SDI", "Low SDI"))
EAPC_cal$sex <- factor(EAPC_cal$sex, 
                       levels = c("<20 years", "20-24 years", "25-29 years", 
                                  "30-34 years", "35-39 years", "40-44 years", 
                                  "45-49 years", "50-69 years", "70+ years"))

# 创建可视化
ggplot(EAPC_cal, aes(x = location, y = EAPC, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +  # 使用位置分隔的柱状图
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, 
                position = position_dodge(0.9), color = "#B0C4DE") +  # 添加误差条
  labs(title = "Estimated Annual Percentage Change (EAPC) in Incidence by Region and Age Group",
       x = "Country/Region",
       y = "EAPC",
       fill = "age group") +
  theme_minimal() +  # 使用简约主题
  scale_fill_manual(values = c("<20 years" = "#B0C4DE", 
                               "20-24 years" = "#ADD8E6", 
                               "25-29 years" = "#FFC0CB", 
                               "30-34 years" = "#F08080", 
                               "35-39 years" = "#FF6347", 
                               "40-44 years" = "#FF4500", 
                               "45-49 years" = "#FF8C00", 
                               "50-69 years" = "#32CD32", 
                               "70+ years" = "#008000")) +  # 设置低饱和度纯色
  ylim(-0.6, 0.6) +  # 缩短纵坐标轴范围
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))  # 旋转x轴标签
