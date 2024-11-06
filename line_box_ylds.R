setwd('/Users/zhoujia/Desktop/AA1029') ##设置工作路径
library(dplyr)                            ## 读取需要的R包
library(ggplot2)

###按地区分类
EC <- read.csv('Alopecia areata.csv',header = T)  ## 读取我们的数据
order <- read.csv('order.csv',header = F)
EC$location_name <- factor(EC$location_name, 
                           levels=order$V1, 
                           ordered=TRUE)


# 筛选数据
EAPC_data <- EC %>%
  filter(measure_name == "YLDs (Years Lived with Disability)" & 
           sex_name %in% c("Both", "Male", "Female") & 
           location_name == 'Global' & 
           age_name == 'All ages' & 
           metric_name %in% c("Number", "Rate"))%>%
  select(year, sex_name, val, upper, lower,metric_name)


# 对Rate数据进行缩放以适应第二个Y轴
max_number <- max(EAPC_data$val[EAPC_data$metric_name == "Number"], na.rm = TRUE) / 1e6
max_rate <- max(EAPC_data$val[EAPC_data$metric_name == "Rate"], na.rm = TRUE)
scale_factor <- max_number / max_rate

# 创建基础图形对象
ggplot() +
  # 添加柱状图（Number）
  geom_bar(data = EAPC_data %>% filter(metric_name == "Number"), 
           aes(x = year, y = val / 1e6, fill = sex_name),  # 将Number值除以1百万
           stat = "identity", 
           position = "dodge", 
           alpha = 1) +  # 调整透明度
  # 添加折线图（Rate）
  geom_line(data = EAPC_data %>% filter(metric_name == "Rate"), 
            aes(x = year, y = val * scale_factor, color = sex_name, group = sex_name), 
            size = 1) +  # 设置折线宽度
  geom_ribbon(data = EAPC_data %>% filter(metric_name == "Rate"), 
              aes(x = year, ymin = lower * scale_factor, ymax = upper * scale_factor, fill = sex_name), 
              alpha = 0.2) +  # 添加阴影区域
  geom_errorbar(data = EAPC_data %>% filter(metric_name == "Rate"), 
                aes(x = year, ymin = lower * scale_factor, ymax = upper * scale_factor, color = sex_name), 
                width = 0.2) +  # 添加误差条
  labs(title = "A Total Number of YLDs and Age-standardized Rates (Per 100,000 Persons) by Sex",
       x = "Year",
       y = "Number of YLDs (million)",  # 修改Y轴标签
       color = "sex (rate)",
       fill = "sex (number)") +
  theme_minimal() +  # 使用简约主题
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_color_manual(values = c("Both" = "grey", "Male" = "#ADD8E6", "Female" = "#F08080")) +  # 设置颜色
  scale_fill_manual(values = c("Both" = "grey", "Male" = "#ADD8E6", "Female" = "#F08080")) +  # 设置填充颜色
  scale_y_continuous(sec.axis = sec_axis(~ . / scale_factor, name = "Age-standardized YLDs Rate (per 100,000 persons)")) +  # 添加第二个Y轴
  scale_x_continuous(breaks = seq(1990, 2021, by = 1)) +  # 每年标记
  guides(fill = guide_legend(title = "Sex (Number)"), 
         color = guide_legend(title = "Sex (Rate)"))  # 分开图例标题





