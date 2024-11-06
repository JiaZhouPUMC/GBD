# 加载必要的包
library(ggplot2)
library(reshape2)
order_y <- read.csv('order_y.csv',header = F)
order_x <- read.csv('order_comorbidity.csv',header = F)
# 设置工作目录
setwd('/Users/zhoujia/Desktop/AA1029')
# 创建文件名列表
file_names <- c(
  "Atopic dermatitis.csv",
  "Nonalcoholic fatty liver disease including cirrhosis.csv",
  "Urticaria.csv",
  "Pruritus.csv",
  "Alopecia areata.csv",
  "Acne vulgaris.csv",
  "Depressive disorders.csv",
  "Drug use disorders.csv",
  "Alcohol use disorders.csv",
  "COVID-19.csv",
  "Viral skin diseases.csv",
  "Fungal skin diseases.csv",
  "Psoriasis.csv",
  "Rheumatoid arthritis.csv",
  "Inflammatory bowel disease.csv",
  "Cirrhosis due to other causes.csv",
  "Cirrhosis due to alcohol.csv",
  "Chronic hepatitis C including cirrhosis.csv",
  "Chronic hepatitis B including cirrhosis.csv",
  "Seborrheic dermatitis.csv",
  "Contact dermatitis.csv",
  "Chronic kidney disease.csv",
  "Diabetes mellitus.csv",
  "Eating disorders.csv",
  "Anxiety disorders.csv",
  "Bipolar disorder.csv",
  "Other nutritional deficiencies.csv",
  "Dietary iron deficiency.csv",
  "Vitamin A deficiency.csv",
  "Iodine deficiency.csv",
  "Protein-energy malnutrition.csv"
)
# 读取文件并存储在列表中
data_list <- lapply(file_names, function(file) {
  read.csv(file, stringsAsFactors = FALSE)
})
# 将数据框命名为文件名（去掉.csv）
names(data_list) <- sub("\\.csv$", "", file_names)
# 检查数据
str(data_list)
# 将 data_list 中的每个数据框分配到独立变量
list2env(data_list, envir = .GlobalEnv)
# 检查变量是否创建成功
ls()
########################################################
###############第一行p值############################
########################################################
AA_male <- subset(`Alopecia areata`,
                  `Alopecia areata`$age_name=='Age-standardized' & 
                    `Alopecia areata`$metric_name== 'Rate' &
                    `Alopecia areata`$measure_name=='Incidence'&
                    `Alopecia areata`$sex_name=="Male")
# 假设 data_list 中存储了所有数据框
# 创建一个空列表来存储结果
male_data_list <- lapply(names(data_list), function(name) {
  df <- data_list[[name]]
  # 筛选符合条件的数据
  subset(df, age_name == 'Age-standardized' & 
           metric_name == 'Rate' &
           measure_name == 'Incidence' &
           sex_name == "Male")
})

# 为每个数据框命名
names(male_data_list) <- names(data_list)

# 提取其中的每个数据框
for (i in names(male_data_list)) {
  assign(paste0(i, "_male"), male_data_list[[i]])
}

list2env(male_data_list, envir = .GlobalEnv)

############计算相关系数和p值
cor.test(AA_male$val,`Acne vulgaris_male`$val,method="pearson")
# 假设 data_list 是存储所有共病数据框的列表
# data_list <- list(AA_male, acne_male, psoriasis_male, ...)  # 添加你的共病数据集
# 创建一个空的数据框来存储结果
# 创建一个空的数据框来存储结果
cor_results <- data.frame(
  Disease = character(),
  Correlation = numeric(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)

# 依次进行相关性测试
for (disease_name in names(male_data_list)) {
  if (disease_name != "Alopecia areata") {  # 排除自身比较
    # 确保 val 向量长度一致
    if (length(AA_male$val) == length(male_data_list[[disease_name]]$val)) {
      test_result <- cor.test(AA_male$val, male_data_list[[disease_name]]$val, method = "pearson")
      
      # 将结果添加到数据框
      cor_results <- rbind(cor_results, data.frame(
        Disease = disease_name,
        Correlation = test_result$estimate,
        P_value = test_result$p.value
      ))
    } else {
      warning(paste("Lengths do not match for", disease_name))
    }
  }
}

# 查看结果
print(cor_results)

iron_male=subset(data_list[["Dietary iron deficiency"]], age_name == 'Age-standardized' & 
         metric_name == 'Rate' &
         measure_name == 'Prevalence' &
         sex_name == "Male")
c=cor.test(AA_male$val,iron_male$val,method="pearson")
write.csv(cor_results,"line1.csv")







#############作图##########################
#################################################################
##############################################################################
# 假设您的相关系数数据框如下
# 这里的示例数据框请替换为您的实际数据




data <- data.frame(
  Comorbidity = c("Atopic dermatitis", "Seborrheic dermatitis", "Psoriasis", "Urticaria", 
                  "Acne vulgaris", "Pruritus", "Viral skin diseases", "Fungal skin diseases", 
                  "COVID-19", "Rheumatoid arthritis", "Inflammatory bowel disease", 
                  "Alcoholic cirrhosis", "Nonalcoholic cirrhosis", "Chronic kidney disease", 
                  "Diabetes mellitus", "Dietary iron deficiency", "Vitamin A deficiency", 
                  "Iodine deficiency", "Protein-energy malnutrition", 
                  "Depressive disorders", "Anxiety disorders", "Bipolar disorder"),
  Male = rnorm(22),  # 示例数据，实际应为相关系数
  Female = rnorm(22),
  `High SDI` = rnorm(22),
  `High-middle SDI` = rnorm(22),
  `Middle SDI` = rnorm(22),
  `Low-middle SDI` = rnorm(22),
  `Low SDI` = rnorm(22),
  America = rnorm(22),
  Europe = rnorm(22),
  Asia = rnorm(22),
  Africa = rnorm(22),
  `<20 years` = rnorm(22),
  `20-24 years` = rnorm(22),
  `25-29 years` = rnorm(22),
  `30-34 years` = rnorm(22),
  `35-39 years` = rnorm(22),
  `40-44 years` = rnorm(22),
  `45-49 years` = rnorm(22),
  `50-69 years` = rnorm(22),
  `70+ years` = rnorm(22)
)

# 转换数据框为长格式
data_melted <- melt(data, id.vars = "Comorbidity")

# 创建热图
ggplot(data_melted, aes(x = Comorbidity, y = variable, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "purple", mid = "white", high = "darkred", midpoint = 0, 
                       name = "Pearson Correlation Coefficient") +
  labs(title = "Heat map of Pearson correlation coefficient",
       x = "Comorbidities",
       y = "Demographic Groups") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = c(1:22), y = rep(0, 20), label = c("**", "***"), size = 5, vjust = -1)
