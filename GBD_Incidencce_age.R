######按年龄段分类
setwd('/Users/zhoujia/Desktop/AA1029') ## 设置工作路径
library(dplyr)                            ## 读取需要的R包
library(ggplot2)
# 创建年龄向量
age_names <- c("<20 years", "20-24 years", "25-29 years", "30-34 years", 
               "35-39 years", "40-44 years", "45-49 years", 
               "50-69 years", "70+ years")
# 将年龄存储为数据框
age_order <- data.frame(V1 = age_names)
# 写入CSV文件
write.csv(age_order, 'order_year.csv', row.names = FALSE)

# 读取数据
EC <- read.csv('Alopecia areata.csv', header = TRUE)  ## 读取我们的数据
order <- read.csv('order_year.csv', header = T)

# 设置地区顺序
EC$age_name <- factor(EC$age_name, 
                           levels = order$V1, 
                           ordered = TRUE)

EC <- EC %>% filter(age_name %in% order$V1)

## 1990发病人数
EC_1990 <- subset(EC,EC$year==1990 & 
                    EC$sex_name=='Both' & 
                    EC$metric_name== 'Number' &
                    EC$measure_name=='Incidence'&
                    EC$location_name=="Global")



EC_1990 <- EC_1990[,c(8,14,15,16)]  ### 只取需要的变量：地区以及对应的数值
EC_1990$val <- round(EC_1990$val,1)  ###取整
EC_1990$lower <- round(EC_1990$lower,1)###取整
EC_1990$upper <- round(EC_1990$upper,1) ###取整
EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## 用-连接95%UI上下数值
EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UI前后加括号             
EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UI前后加括号
EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

## 2021发病人数
EC_2021 <- subset(EC,EC$year==2021 & 
                    EC$sex_name=='Both' & 
                    EC$metric_name== 'Number' &
                    EC$measure_name=='Incidence'&
                    EC$location_name=="Global")

EC_2021 <- EC_2021[,c(8,14,15,16)]  ### 只取需要的变量：地区以及对应的数值
EC_2021$val <- round(EC_2021$val,1)  ###取整
EC_2021$lower <- round(EC_2021$lower,1)###取整
EC_2021$upper <- round(EC_2021$upper,1) ###取整
EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-') ## 用-连接95%UI上下数值
EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')  ##95%UI前后加括号             
EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')  ##95%UI前后加括号
EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接


## 1990 ASR
ASR_1990 <- subset(EC,EC$year==1990 & 
                     EC$sex_name=='Both' & 
                     EC$metric_name== 'Rate' &
                     EC$measure_name=='Incidence'&
                     EC$location_name=="Global")

ASR_1990 <- ASR_1990[,c(8,14,15,16)]  ### 只取需要的变量：地区以及对应的数值
ASR_1990$val <- round(ASR_1990$val,1)  ###取整
ASR_1990$lower <- round(ASR_1990$lower,1)###取整
ASR_1990$upper <- round(ASR_1990$upper,1) ###取整
ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号             
ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接


## 2021 ASR
ASR_2021 <- subset(EC,EC$year==2021 & 
                     EC$sex_name=='Both' & 
                     EC$metric_name== 'Rate' &
                     EC$measure_name=='Incidence'&
                     EC$location_name=="Global")

ASR_2021 <- ASR_2021[,c(8,14,15,16)]  ### 只取需要的变量：地区以及对应的数值
ASR_2021$val <- round(ASR_2021$val,1)  ###取整
ASR_2021$lower <- round(ASR_2021$lower,1)###取整
ASR_2021$upper <- round(ASR_2021$upper,1) ###取整
ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号             
ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接



#################################
############################################
#################################
EAPC <- subset(EC, EC$sex_name=='Both' & 
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence'&
                 EC$location_name=="Global")
EAPC <- EAPC[,c(8,13,14)] ##获取地区、年份以及对应的数值



agename <- EC_1990$age_name
EAPC_cal <- data.frame('age_name'=agename,EAPC=rep(0,times=9),UCI=rep(0,times=9),LCI=rep(0,times=9)) 
for (i in 1:9){  ###总共10个地区，所以循环10次
  age_cal <- as.character(EAPC_cal[i,1]) ### 依次取对应的地区
  a <- subset(EAPC, EAPC$'age_name'==age_cal)  ##取对应地区的数据子集
  a$y <- log(a$val)  ##根据EAPC计算方法计算y值
  mod_simp_reg<-lm(y~year,data=a) ##根据EAPC计算方法做线性回归方程
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##根据EAPC计算方法取方程beta值来计算EAPC
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  ### 计算EAPC的95%可信区间的上限值
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  ### 计算EAPC的95%可信区间的下限值
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}

EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##保留2位小数点
EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-') 
EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '') 
EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '') 
EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')  

### 数据整合
EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
ASR_1990 <- ASR_1990[,c(1,5)]
EC_2021 <- EC_2021[,c(1,5)]
ASR_2021 <- ASR_2021[,c(1,5)]
EAPC_cal <- EAPC_cal[,c(1,5)]
Incidence <- merge(EC_1990,ASR_1990,by='age_name')
Incidence <- merge(Incidence,EC_2021,by='age_name')
Incidence <- merge(Incidence,ASR_2021,by='age_name')
Incidence <- merge(Incidence,EAPC_cal,by='age_name')
write.csv(Incidence,'Results for incidence_age.csv')

