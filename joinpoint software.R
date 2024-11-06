#### for joinpoint
setwd('/Users/zhoujia/Desktop/AA1104') ##设置工作路径
library(dplyr)                            ## 读取需要的R包
library(ggplot2)
EC <- read.csv('Alopecia areata.csv',header = T)  ## 读取我们的数据
EAPC <- subset(EC, EC$age_name=='Age-standardized' & 
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')

EAPC$SE <- (EAPC$upper-EAPC$lower)/(1.96*2)   ##根据95%UI 计算标准误
EAPC <- EAPC[,c(2,3,6,7,10)] ##获取地区、年份以及对应的数值
EAPC <- EAPC[order(EAPC$location_name,EAPC$sex_name,EAPC$year),]   ### joinpoint要求年份按照升序排列，且一个组放在一起

write.csv(EAPC,'joinpoint.csv')


EC <- read.csv('Alopecia areata.csv',header = T)  ## 读取我们的数据
EAPC <- subset(EC, EC$age_name=='Age-standardized' & 
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Prevalence')


EAPC <- subset(EC, EC$location_name=='Global'&
                 EC$sex_name=='Both'&
                 EC$metric_name== 'Rate' &
                 EC$measure_name=='Incidence')

EAPC$SE <- (EAPC$upper-EAPC$lower)/(1.96*2)   ##根据95%UI 计算标准误
EAPC <- EAPC[,c(4,6,7,10)] ##获取地区、年份以及对应的数值
EAPC <- EAPC[order(EAPC$age_name,EAPC$year),]   ### joinpoint要求年份按照升序排列，且一个组放在一起

write.csv(EAPC,'joinpoint_inci_age.csv')

EAPC$SE <- (EAPC$upper-EAPC$lower)/(1.96*2)   ##根据95%UI 计算标准误
EAPC <- EAPC[,c(2,3,6,7,10)] ##获取地区、年份以及对应的数值
EAPC <- EAPC[order(EAPC$location_name,EAPC$sex_name,EAPC$year),]   ### joinpoint要求年份按照升序排列，且一个组放在一起

write.csv(EAPC,'joinpoint3.csv')


#### 数据处理好后,继续用R处理
AAPC <- read.table('yld.Export.AAPC.txt', header = TRUE, sep = ",")

AAPC <- AAPC[,c(1,2,7,8,9)]
names(AAPC)[3:5] <- c('val','lower','upper')
AAPC$val <- round(AAPC$val,2)
AAPC$lower=round(AAPC$lower,2)
AAPC$upper=round(AAPC$upper,2)
AAPC$AAPC <- paste(AAPC$lower,AAPC$upper,sep = ', ') ## 用"-"连接95%UI上下数值
AAPC$AAPC <- paste(AAPC$AAPC,')',sep = '')  ##95%UI前后加括号             
AAPC$AAPC <- paste('(',AAPC$AAPC,sep = '')  ##95%UI前后加括号
AAPC$AAPC <- paste(AAPC$val,AAPC$AAPC,sep = ' ') ##数据和95%UI用空格键连接


age_labels <- c("<20 years", "20-24 years", "25-29 years", "30-34 years", 
                "35-39 years", "40-44 years", "45-49 years", "50-69 years", 
                "70+ years", "Age-standardized", "All ages")

AAPC$age_name <- age_labels[AAPC$age_name + 1]  # +1 因为 R 的索引从 1 开始

write.csv(AAPC,'AGE_AAPC.csv')




# 替换 location_name
location_labels <- c("Africa", "America", "Asia", "Europe", "Global", 
                     "High SDI", "High-middle SDI", "Low SDI", 
                     "Low-middle SDI", "Middle SDI")
AAPC$location_name <- factor(AAPC$location_name, levels = 0:9, labels = location_labels)

# 替换 sex_name
sex_labels <- c("Both", "Male", "Female")
AAPC$sex_name <- factor(AAPC$sex_name, levels = 0:2, labels = sex_labels)
write.csv(AAPC,'AAPC_yld.csv')
library(reshape2)
library(pheatmap)
AAPC_matrix <- dcast(AAPC, location_name ~ sex_name, value.var = "val")

rownames(AAPC_matrix) <- AAPC_matrix$location_name
AAPC_matrix <- AAPC_matrix[,-1]  # 删除 Disease 列



pheatmap(as.matrix(AAPC_matrix),  # 转置矩阵
         main = "AAPC for YLDs",
         cluster_rows = F,
         cluster_cols = F,
         fontsize_number = 8,
         display_numbers = T,
         border_color = NA)  # 设置边框颜色为NA以取消关联性线



