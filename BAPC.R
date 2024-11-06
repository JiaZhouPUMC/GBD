#BAPC
setwd('/Users/zhoujia/Desktop/AA1029') ##设置工作路径
library(brms)
library(rstanarm)
library(ggplot2)
library(gridExtra)

EC <- read.csv('Alopecia areata.csv', header = TRUE)  ## 读取我们的数据
order_year <- read.csv('order_year.csv', header = T)
EC <- EC %>% filter(age_name %in% order_year$V1)
order_region <- read.csv('order.csv',header = F)
EC$location_name <- factor(EC$location_name, 
                           levels=order_region$V1, 
                           ordered=TRUE)
EC$age_name <- factor(EC$age_name, 
                      levels = order_year$V1, 
                      ordered = TRUE)

library(dplyr)
# 按照不同的measure_name创建子数据框
dalys_df <- filter(EC, measure_name == "DALYs (Disability-Adjusted Life Years)")
ylds_df <- filter(EC, measure_name == "YLDs (Years Lived with Disability)")
prevalence_df <- filter(EC, measure_name == "Prevalence")
incidence_df <- filter(EC, measure_name == "Incidence")

# 查看子数据框
head(dalys_df)
head(ylds_df)
head(prevalence_df)
head(incidence_df)


dalys_num <- filter(dalys_df, metric_name == 'Number',location_name == 'Global',age_name=='All ages')
dalys_rate <- filter(dalys_df, metric_name == 'Rate')
ylds_num <- filter(ylds_df, metric_name == 'Number')
ylds_rate <- filter(ylds_df, metric_name == 'Rate')
prevalence_num <- filter(prevalence_df, metric_name == 'Number')
prevalence_rate <- filter(prevalence_df, metric_name == 'Rate')
incidence_num <- filter(incidence_df, metric_name == 'Number')
incidence_rate <- filter(incidence_df, metric_name == 'Rate')

dalys_num$val <- round(dalys_num$val)

####################################
######贝叶斯##################
##############################
dalys_num$val <- round(dalys_num$val)
model <- brm(
  val ~ year +(1|sex_name),
  data = dalys_num,
  family = negbinomial(), # 或者选择适合你数据的分布
  prior = c(set_prior("normal(0, 10)", class = "b")),
  chains = 8,
  iter = 2000
)

# 预测到2050年
data=dalys_num
new_data=data.frame()
# 创建包含年份、地点、性别和年龄的完整数据集
years <- 2022:2050
# 使用 expand.grid 创建所有组合
new_data <- expand.grid(year = years,sex_name=unique(EC$sex_name))

# 查看新数据框的结构
head(new_data)

predictions <- predict(model, newdata = new_data)

# 将预测结果合并回数据框
predicted_dalys_num <- data.frame(year = new_data$year, val = predictions[,1]) # 获取预测值的第一列

# 合并实际和预测数据
combined_dalys_num <- rbind(data.frame(year = dalys_num$year, val = dalys_num$val, type = "Actual"),
                       data.frame(year = predicted_dalys_num$year, val = predicted_dalys_num$val, type = "Predicted"))

# 绘制图形
ggplot(combined_dalys_num, aes(x = year, y = val, fill = type)) +
  geom_col(position = "dodge", alpha = 0.5) +  # 柱状图
  labs(title = "DALYs Prediction from 1990 to 2050",
       x = "Year", 
       y = "Value") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(combined_dalys_num, aes(x = year, y = val, fill = type)) +
  geom_col(position = "dodge", alpha = 0.5) +  # 柱状图
  labs(title = "DALYs Prediction from 1990 to 2050",
       x = "Year", 
       y = "Value") +
  theme_minimal() +
  theme(legend.title = element_text(size = 14),  # 放大图注标题
        legend.text = element_text(size = 12),   # 放大图注内容
        plot.title = element_text(hjust = 0.5)) +  # 居中标题
  scale_fill_manual(values = c("#A3C1DA", "pink"))


