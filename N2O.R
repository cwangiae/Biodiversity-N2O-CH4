
#### 绘制各变量图 ####
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(rcartocolor)

# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()

#### N2O with time ####
#导入数据
data_time_N2O=read.csv("E:/黄小轶数据/N2Odata/Times_N2O.csv", sep=",", header = T)
names(data_time_N2O)

#设置分类变量
data_time_N2O$Dilution <- factor(data_time_N2O$Dilution, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data_time_N2O$Temp <- factor(data_time_N2O$Temp, levels = c( "15","17"), ordered= T)
data_time_N2O$Landuse <- factor(data_time_N2O$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data_time_N2O)
data_time_N2O <- na.omit(data_time_N2O)

## N2O with time
Time_N2O <- ggline(data_time_N2O, x = "Time", y = "X15.17N20.flux", 
                   add = "mean_se", 
                   color = "Dilution",
                   facet.by = c("Landuse"),
                   #scales = "free",
                   palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),  # 自定义配色
                   size = 1.2)+ 
  theme(axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 22, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 24, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", size = 0.4))+ 
  border(color = "black", size = 1.5)

Time_N2O
ggsave("PlotN2O/N2O_Time.jpg", plot = Time_N2O, device = "jpeg", width = 18, height = 7, dpi = 600)


# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(ShannonF ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)


rm(list = ls())

# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()
#### 绘制各变量图 ####
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(rcartocolor)

# 导入数据 
data1=read.csv("E:/黄小轶数据/N2Odata/N2O_Dateset.csv", sep=",", header = T)
names(data1)
str(data1)

#设置分类变量
#data1$Plot <- as.factor(data1$Plot)
data1$Diversity <- factor(data1$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data1$Landuse <- factor(data1$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data1)

# N2O累计排放量图
fig_N2O <- ggboxplot(data1,
                     x = "Diversity",
                     y = "N2O",
                     combine= TRUE,
                     add = "jitter",
                     fill = "Diversity", 
                     scales = "free",
                     palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),)+ 
  #geom_jitter(size = 2, width = 0, height = 0) +
  scale_y_continuous(limits = c(0, 150))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),  # 设置图形背景透明
        panel.background = element_rect(fill = "transparent", color = NA))+
  border(color = "black", size = 1.5)
fig_N2O
ggsave("PlotN2O//N2O.png", plot = fig_N2O, device = "png", width = 6, height = 6, dpi = 490, bg = "transparent")
#ggsave("N2O-2.png", plot = fig_N2O, device = "jpeg", width = 6, height = 6, dpi = 300, bg = "transparent")


## N2O by Landuse
fig2_N2O <- ggboxplot(data1,
                      x = "Diversity",
                      y = "N2O",
                      combine= TRUE,
                      add = "jitter",
                      fill = "Diversity", 
                      #scales = "free",
                      palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                      facet.by = c("Landuse"))+
  #scale_y_continuous(limits = c(0, 700))+
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",    # 移除图例
        strip.text = element_text(size = 30, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
fig2_N2O
ggsave("plotN2O/N2O_landuse.jpg", plot = fig2_N2O, device = "jpeg", width = 18, height = 7, dpi = 600)

# qco2
# fig_qco2 <- ggboxplot(data1,
#                       x = "Diversity",
#                       y = "qco2",
#                       combine= TRUE,
#                       add = "jitter",
#                       fill = "Diversity", 
#                       scales = "free",
#                       palette = "uchicago")+ 
#                       scale_y_continuous(limits = c(0, 300))+
#                       theme(axis.text.x = element_text(size = 14),
#                             axis.text.y = element_text(size = 14),
#                             axis.title.x = element_blank(),
#                             axis.title.y = element_blank())
# fig_qco2
# ggsave("qco2.jpg", plot = fig_qco2, device = "jpeg", width = 6, height = 6, dpi = 300)

fig_qN2O <- ggboxplot(data1,
                      x = "Diversity",
                      y = "qN2O",
                      combine= TRUE,
                      add = "jitter",
                      fill = "Diversity", 
                      # scales = "free",
                      palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),)+ 
  #geom_jitter(size = 2, width = 0, height = 0) +
  scale_y_continuous(limits = c(0, 2000))+
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),  # 设置图形背景透明
        panel.background = element_rect(fill = "transparent", color = NA))+
  border(color = "black", size = 1.5)

fig_qN2O
ggsave("PlotN2O/qN2O.png", plot = fig_qN2O, device = "png", width = 6, height = 6, dpi = 600, bg = "transparent")


## qN2O by Landuse
fig2_qN2O <- ggboxplot(data1,
                       x = "Diversity",
                       y = "qN2O",
                       combine= TRUE,
                       add = "jitter",
                       fill = "Diversity", 
                       #scales = "free",
                       palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                       facet.by = c("Landuse"))+ 
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",    # 移除图例
        strip.text = element_text(size = 30, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
fig2_qN2O
ggsave("PlotN2O/qN2O_landuse.jpg", plot = fig2_qN2O, device = "jpeg", width = 18, height = 7, dpi = 600)


###qN2O
# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data1 %>%
  group_by(Landuse) %>%
  anova_test(qN2O ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)
###N2O
# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data1 %>%
  group_by(Landuse) %>%
  anova_test(N2O ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)


  #### 线性混合模型####
## 进行数据Z转换
library(tibble)
library(lmerTest)
library(lme4)
library(MASS)
library(tidyverse)
library(sjPlot) 
library(MuMIn)
library(performance)
library(dplyr)

# 读入数据 
data1=read.csv("E:/黄小轶数据/N2Odata/N2O_Dateset.csv", sep=",", header = T) # 这里的O处理名称变“C”
names(data1)
str(data1)

# 选择数据进行标准化
data2 <- data1 %>% dplyr::select("Name","Turnover","CUE", "NUE","MBC","MBN","RichnessB", "RichnessF","SOC","GNM","ShannonB","ShannonF")
rownames(data2) <- data2$Name
data2 <- data2 %>% dplyr::select(-"Name")
data1_z <- as.data.frame(scale(data2))
data1_z$Name <- rownames(data1_z)
str(data1_z)

# 具有Landuse的标准化数据库
data3 <- data1 %>% dplyr::select("Name","Landuse","Diversity2","Diversity1","Diversity","Plot","N2O","qN2O")
data4 <- merge(data3,data1_z,by = "Name")
names(data4)
str(data4)

#创建NUE和turnover的交互项
data4$turnoverNUE <- data4$Turnover*data4$NUE
data4$turnoverCUE <- data4$Turnover*data4$CUE
str(data4)

#### 分析变量间的相关性 ####
####相关性分析，做热图
library(tidyverse)
library(reshape2)
library(ggcorrplot)
library(ggplot2)
library(corrplot)
col2 = colorRampPalette(c('#053061', '#2166AC', '#4393C3', '#92C5DE',
                          '#D1E5F0', '#FFFFFF', '#FDDBC7', '#F4A582',
                          '#D6604D', '#B2182B', '#67001F'))
data4_heat <- data4%>% dplyr::select("N2O","qN2O",
                                      "ShannonB","ShannonF",
                                      "Turnover","CUE","NUE","MBN",
                                      "MBC","GNM")
data4_heat <- data1 %>% dplyr::select("N2O","qN2O",
                                      "ShannonB","ShannonF",
                                      "Turnover","CUE","NUE","MBN",
                                      "MBC")
cormat <- round(cor(data4_heat),2)
p.mat <- cor_pmat(data4_heat)

M <- cor(data4_heat, method = "spearman")
corheatmap = corrplot(M, col = col2(100),type = "lower", 
                      diag = FALSE, insig = "blank", 
                      method = 'square',p.mat = p.mat)
corheatmap


# 方法1-不进行转换，直接用data4的数据进行模型

# # 方法2-用as.factor对diversity进行转换
# data4$Plot <- as.factor(data4$Plot)
# data4$Diversity <- as.factor(data4$Diversity)
# data4$Landuse <- as.factor(data4$Landuse)
# str(data4)
# 

# 修改列的名称
#data4 <- data4 %>% rename(Diversity2 = Diversity)
#data4 <- data4 %>% rename(Diversity = Diversity1)
#str(data4)

# # 方法3-用factor对diversity进行转换
#data4$Diversity <- factor(data4$Diversity, levels = c("C", "D0", "D2","D4","D8"), ordered= T)
#data4$Landuse <- factor(data4$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)

#### N2O的模型####
LMM_N2O<-lmer(N2O~Diversity1+(1|Landuse),data=data4)
shapiro.test(resid(LMM_N2O))
summary(LMM_N2O)
tab_model(LMM_N2O)
anova(LMM_N2O,ddf="Kenward-Roger")
r_squared <- r.squaredGLMM(LMM_N2O)
print(r_squared)
AIC(LMM_N2O)
#plot(check_collinearity(LMM_N2O))

# 高度共线性，模型不能使用(ShannonB VIF超过十)
LMM_N2O_1<-lmer(N2O~Diversity1+ShannonB+ShannonF+(1|Landuse),data=data4)
shapiro.test(resid(LMM_N2O_1))
summary(LMM_N2O_1)
tab_model(LMM_N2O_1)
anova(LMM_N2O_1,ddf="Kenward-Roger")
r_squared_1 <- r.squaredGLMM(LMM_N2O_1)
print(r_squared_1)
AIC(LMM_N2O_1)
plot(check_collinearity(LMM_N2O_1))

# 没有显著性
LMM_N2O_2<-lmer(N2O~ShannonB+ShannonF+CUE+NUE+NUE*Turnover+CUE*Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_N2O_2))
summary(LMM_N2O_2)
tab_model(LMM_N2O_2)
anova(LMM_N2O_2,ddf="Kenward-Roger")
r_squared_2 <- r.squaredGLMM(LMM_N2O_2)
print(r_squared_2)
AIC(LMM_N2O_2)
plot(check_collinearity(LMM_N2O_2))


#都显著
LMM_N2O_3<-lmer(N2O~ShannonB+ShannonF+(1|Landuse),data=data4)
shapiro.test(resid(LMM_N2O_3))
summary(LMM_N2O_3)
tab_model(LMM_N2O_3)
anova(LMM_N2O_3,ddf="Kenward-Roger")
r_squared_3 <- r.squaredGLMM(LMM_N2O_3)
print(r_squared_3)
AIC(LMM_N2O_3)
plot(check_collinearity(LMM_N2O_3))


# 
LMM_N2O_4<-lmer(N2O~ShannonB+ShannonF+MBC+CUE+MBN+NUE+Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_N2O_4))
summary(LMM_N2O_4)
tab_model(LMM_N2O_4)
anova(LMM_N2O_4,ddf="Kenward-Roger")
r_squared_4 <- r.squaredGLMM(LMM_N2O_4)
print(r_squared_4)
AIC(LMM_N2O_4)
plot(check_collinearity(LMM_N2O_4))



#这个模型是N2O最好的模型
LMM_N2O_6<-lmer(log(N2O)~ShannonB+MBC+MBN+NUE+GNM+(1|Landuse),data=data4)
shapiro.test(resid(LMM_N2O_6))
summary(LMM_N2O_6)
tab_model(LMM_N2O_6)
anova(LMM_N2O_6,ddf="Kenward-Roger")
r_squared_6 <- r.squaredGLMM(LMM_N2O_6)
print(r_squared_6)
AIC(LMM_N2O_6)
plot(check_collinearity(LMM_N2O_6))




# #### qN2O的模型####
LMM_qN2O_1<-lmer(log(qN2O)~Diversity1+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qN2O_1))
summary(LMM_qN2O_1)
tab_model(LMM_qN2O_1)
anova(LMM_qN2O_1,ddf="Kenward-Roger")
r_squared_q1 <- r.squaredGLMM(LMM_qN2O_1)
print(r_squared_q1)
AIC(LMM_qN2O_1)
# plot(check_collinearity(LMM_qN2O_1))


# 存在高共线性，这个模型不能用
LMM_qN2O_2<-lmer(log(qN2O)~Diversity+ShannonB+ShannonF+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qN2O_2))
summary(LMM_qN2O_2)
tab_model(LMM_qN2O_2)
anova(LMM_qN2O_2,ddf="Kenward-Roger")
r_squared_q2 <- r.squaredGLMM(LMM_qN2O_2)
print(r_squared_q2)
AIC(LMM_qN2O_2)
plot(check_collinearity(LMM_qN2O_2))

#不交互更好
LMM_qN2O_3<-lmer(log(qN2O)~ShannonB+ShannonF+CUE+NUE+Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qN2O_3))
summary(LMM_qN2O_3)
tab_model(LMM_qN2O_3)
anova(LMM_qN2O_3,ddf="Kenward-Roger")
r_squared_q3 <- r.squaredGLMM(LMM_qN2O_3)
print(r_squared_q3)
AIC(LMM_qN2O_3)
plot(check_collinearity(LMM_qN2O_3))

#考虑了所有交互作用，不显著
LMM_qN2O_5<-lmer(log(qN2O)~ShannonB+ShannonF+CUE*Turnover+Turnover*NUE+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qN2O_5))
summary(LMM_qN2O_5)
tab_model(LMM_qN2O_5)
anova(LMM_qN2O_5,ddf="Kenward-Roger")
r_squared_q5 <- r.squaredGLMM(LMM_qN2O_5)
print(r_squared_q5)
AIC(LMM_qN2O_5)
plot(check_collinearity(LMM_qN2O_5))

#这个模型最好
# library(blme)
LMM_qN2O_4<-lmer(log(qN2O)~ShannonB+ShannonF+Turnover+turnoverNUE+CUE+NUE+GNM+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qN2O_4)) 
summary(LMM_qN2O_4) 
tab_model(LMM_qN2O_4)
anova(LMM_qN2O_4,ddf="Kenward-Roger")
r_squared_q4 <- r.squaredGLMM(LMM_qN2O_4) 
print(r_squared_q4)  
AIC(LMM_qN2O_4) 
plot(check_collinearity(LMM_qN2O_4))

#(不收敛)
LMM_qN2O_6<-lmer(qN2O~ShannonB+ShannonF+Turnover+NUE:Turnover+CUE+NUE+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qN2O_6))
summary(LMM_qN2O_6)
tab_model(LMM_qN2O_6)
anova(LMM_qN2O_6,ddf="Kenward-Roger")
r_squared_q6 <- r.squaredGLMM(LMM_qN2O_6)
print(r_squared_q6)
AIC(LMM_qN2O_6)
plot(check_collinearity(LMM_qN2O_6))

#
LMM_qN2O_7<-lmer(log(qN2O)~ShannonB+ShannonF+Turnover+NUE:Turnover+CUE+NUE+CUE:Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qN2O_7))
summary(LMM_qN2O_7)
tab_model(LMM_qN2O_7)
anova(LMM_qN2O_7,ddf="Kenward-Roger")
r_squared_q7 <- r.squaredGLMM(LMM_qN2O_7)
print(LMM_qN2O_7)
AIC(LMM_qN2O_7)
plot(check_collinearity(LMM_qN2O_7))



# 多个qco2模型比较，LMM_qCO2_5 IS THE BEST MODEL, SO IT WAS USED IN THE FOLLOWING PARTIAL ANALYSIS
AIC(LMM_qN2O_1,LMM_qN2O_2,LMM_qN2O_3,LMM_qN2O_4,LMM_qN2O_5,LMM_qN2O_6,LMM_qN2O_7) # 模型1是最好的模型
r.squaredGLMM(LMM_qN2O_1) # 模型1的R2相对模型2低很多
r.squaredGLMM(LMM_qN2O_2)
r.squaredGLMM(LMM_qN2O_3)## this model is the best one
r.squaredGLMM(LMM_qN2O_4)
r.squaredGLMM(LMM_qN2O_5)
r.squaredGLMM(LMM_qN2O_6)
r.squaredGLMM(LMM_qN2O_7)

#### 分析模型4获得的各个参数与qN2O的关系 ####
##分析turnover * NUE变量对N2O的影响


# 加载包
library(ggpubr)
library(ggplot2)

library(data.table)
head(data4)
LMM_data_1<-data.table(qN2O=c(resid(LMM_qN2O_4)+
                                fixef(LMM_qN2O_4)[1]+
                                (fixef(LMM_qN2O_4)["turnoverNUE"]*LMM_qN2O_4@frame[,"turnoverNUE"])),
                       turnoverNUE=c(LMM_qN2O_4@frame[,"turnoverNUE"]+mean(data4$turnoverNUE)),
                       Landuse= LMM_qN2O_4@frame[,"Landuse"],
                       ShannonB= LMM_qN2O_4@frame[,"ShannonB"])
LMM_data_1[,Pre_qN2O:=exp(qN2O)]

# qN2O vs turnoverNUE
fig_N2O_turnNUE <- ggscatter(LMM_data_1,
                             x = "turnoverNUE",
                             y = "Pre_qN2O",
                             # add = "reg.line",
                             #conf.int = TRUE, 
                             #cor.coef = TRUE,
                             size = 5,
                             palette = "uchicago",
                             color = "steelblue",
                             cor.coef.coord = c(4, 75),
                             cor.coef.size =6)+ 
 # scale_y_continuous(breaks = seq(0, 2000, by = 500), limits = c(0, 2000))+
  
  stat_smooth(method = "lm", color = "Black", se = TRUE)+  # 添加回归线并设置回归线颜色
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA))
fig_N2O_turnNUE
ggsave("qN2O_turnoverNUE.png", plot = fig_N2O_turnNUE, device = "png", width = 6, height = 7, dpi = 600, bg = "transparent")

                                      


##分析turnover变量对N2O的影响
LMM_data_2<-data.table(qN2O=c(resid(LMM_qN2O_4)+
                                fixef(LMM_qN2O_4)[1]+
                                (fixef(LMM_qN2O_4)["Turnover"]*LMM_qN2O_4@frame[,"Turnover"])),
                       Turnover=c(LMM_qN2O_4@frame[,"Turnover"]+mean(data4$Turnover)),
                       Landuse= LMM_qN2O_4@frame[,"Landuse"])
#                        Diversity= LMM_qN2O_4@frame[,"Diversity"]
# )
LMM_data_2[,Pre_qN2O:=exp(qN2O)]

# qN2O vs turnover
fig_N2O_turn <- ggscatter(LMM_data_2,
                          x = "Turnover",
                          y = "Pre_qN2O",
                          # add = "reg.line",
                          #conf.int = TRUE, 
                          #cor.coef = TRUE,
                          size = 5,
                          palette = "uchicago",
                          color = "red",
                          cor.coef.coord = c(-1, 140),
                          cor.coef.size =6)+ 
  scale_y_continuous(breaks = seq(0, 1200, by = 300), limits = c(0, 1200))+
  stat_smooth(method = "lm", color = "Black", se = TRUE)+  # 添加回归线并设置回归线颜色
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA))
#border(color = "black", size = 1.5)
fig_N2O_turn
#ggsave("qN2O_turnover.jpg", plot = fig_N2O_turn, device = "jpeg", width = 6, height = 7, dpi = 300)
ggsave("qN2O_turnover.png", plot = fig_N2O_turn, device = "png", width = 6, height = 7, dpi = 600, bg = "transparent")

##分析GNM变量对N2O的影响
LMM_data_3<-data.table(qN2O=c(resid(LMM_qN2O_4)+
                                fixef(LMM_qN2O_4)[1]+
                                (fixef(LMM_qN2O_4)["GNM"]*LMM_qN2O_4@frame[,"GNM"])),
                       GNM=c(LMM_qN2O_4@frame[,"GNM"]+mean(data4$GNM)),
                       Landuse= LMM_qN2O_4@frame[,"Landuse"])
                       # Diversity= LMM_qN2O_4@frame[,"Diversity"])
LMM_data_3[,Pre_qN2O:=exp(qN2O)]

# qN2O vs GNM
fig_N2O_GNM <- ggscatter(LMM_data_3,
                         x = "GNM",
                         y = "Pre_qN2O",
                         # add = "reg.line",
                         #conf.int = TRUE, 
                         #cor.coef = TRUE,
                         palette = "uchicago",
                         size = 5,
                         color = "red",
                         cor.coef.coord = c(0.5,75),
                         cor.coef.size = 6)+ 
  #scale_y_continuous(limits = c(0, 80))+
  # scale_x_continuous(limits = c(-1.4, 3.2))+
  # scale_y_continuous(breaks = seq(0, 680, by = 150), limits = c(0, 680))+
  stat_smooth(method = "lm", color = "Black", se = TRUE)+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA))
fig_N2O_GNM
#ggsave("qco2_CUE.jpg", plot = fig_CO2_CUE, device = "jpeg", width = 6, height = 7, dpi = 300)
ggsave("qN2O_GNM.png", plot = fig_N2O_GNM, device = "png", width = 6, height = 7, dpi = 300, bg = "transparent")


##分析ShannonB变量对N2O的影响
LMM_data_4<-data.table(qN2O=c(resid(LMM_qN2O_4)+
                                fixef(LMM_qN2O_4)[1]+
                                (fixef(LMM_qN2O_4)["ShannonB"]*LMM_qN2O_4@frame[,"ShannonB"])),
                       ShannonB=c(LMM_qN2O_4@frame[,"ShannonB"]+mean(data4$ShannonB)),
                       Landuse= LMM_qN2O_4@frame[,"Landuse"])
                       # Diversity= LMM_qN2O_4@frame[,"Diversity"])
LMM_data_4[,Pre_qN2O:=exp(qN2O)]

# qN2O vs ShannonB
fig_N2O_ShannonB <- ggscatter(LMM_data_4,
                         x = "ShannonB",
                         y = "Pre_qN2O",
                         # add = "reg.line",
                         #conf.int = TRUE, 
                         #cor.coef = TRUE,
                         palette = "uchicago",
                         size = 5,
                         color = "red",
                         cor.coef.coord = c(-0.5, 75),
                         cor.coef.size = 6)+ 
  #scale_y_continuous(breaks = seq(100, 700, by = 150), limits = c(100, 700))+
  stat_smooth(method = "lm", color = "Black", se = TRUE)+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA))
fig_N2O_ShannonB
#ggsave("qco2_NUE.jpg", plot = fig_CO2_NUE, device = "jpeg", width = 6, height = 7, dpi = 300)
ggsave("qN2O_ShannonB.png", plot = fig_N2O_ShannonB, device = "png", width = 6, height = 7, dpi = 300, bg = "transparent")



##分析ShannonF变量对N2O的影响
LMM_data_4<-data.table(qN2O=c(resid(LMM_qN2O_4)+
                                fixef(LMM_qN2O_4)[1]+
                                (fixef(LMM_qN2O_4)["ShannonF"]*LMM_qN2O_4@frame[,"ShannonF"])),
                       ShannonF=c(LMM_qN2O_4@frame[,"ShannonF"]+mean(data4$ShannonF)),
                       Landuse= LMM_qN2O_4@frame[,"Landuse"])
# Diversity= LMM_qN2O_4@frame[,"Diversity"])
LMM_data_4[,Pre_qN2O:=exp(qN2O)]

# qN2O vs ShannonF
fig_N2O_ShannonF <- ggscatter(LMM_data_4,
                              x = "ShannonF",
                              y = "Pre_qN2O",
                              # add = "reg.line",
                              #conf.int = TRUE, 
                              #cor.coef = TRUE,
                              palette = "uchicago",
                              size = 5,
                              color = "red",
                              cor.coef.coord = c(-0.5, 75),
                              cor.coef.size = 6)+ 
  #scale_y_continuous(breaks = seq(100, 700, by = 150), limits = c(100, 700))+
  stat_smooth(method = "lm", color = "Black", se = TRUE)+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.background = element_rect(fill = "transparent", color = NA))
fig_N2O_ShannonF
#ggsave("qco2_NUE.jpg", plot = fig_CO2_NUE, device = "jpeg", width = 6, height = 7, dpi = 300)
ggsave("qN2O_ShannonF.png", plot = fig_N2O_ShannonF, device = "png", width = 6, height = 7, dpi = 300, bg = "transparent")



library(cowplot)
figure_2 <- plot_grid(fig_N2O_turnNUE, fig_N2O_turn, fig_N2O_GNM, fig_N2O_ShannonB, fig_N2O_ShannonF, ncol = 5,align = "h"  )
print(figure_2)
ggsave("qN2O_vs_pred.jpg", plot = figure_2, width = 17, height = 7,device = "jpeg", dpi = 300)


#### 随机森林，微生物门对qN2O进行预测####
library(randomForest)
library(rfPermute)
library(ggplot2)
library(ggpubr)
library(pdp)
library(MASS)
library(A3)
library(caret)# 检验模型显著性
# 导入数据 
dir()
data_asv=read.csv("E:/黄小轶数据/N2Odata/N2O_Dateset.csv", sep=",", header = T)
names(data_asv)
str(data_asv)



## 分析微生物门对qN2O的影响
set.seed(123)
RF_model_taxa <- randomForest(qN2O~Chloroflexi+
                                Proteobacteria+
                                Actinobacteriota+
                                Verrucomicrobiota+
                                Bacteroidota+
                                Planctomycetota+
                                Acidobacteriota+
                                Gemmatimonadota+
                                Deinococcota+
                                Myxococcota+
                                Ascomycota+
                                Mortierellomycota+
                                Basidiomycota+
                                Chytridiomycota+
                                Rozellomycota,
                              data = data_asv,
                              ntree=1000,
                              ntry =2,
                              importance=TRUE,
                              proximity=TRUE)
summary(RF_model_taxa)
varImpPlot(RF_model_taxa)
plot(RF_model_taxa)
importance(RF_model_taxa)







set.seed(123) 
RF_model_taxa2 <- rfPermute(qN2O~Chloroflexi+
                              Proteobacteria+
                              Actinobacteriota+
                              Verrucomicrobiota+
                              Bacteroidota+
                              Planctomycetota+
                              Acidobacteriota+
                              Gemmatimonadota+
                              Deinococcota+
                              Myxococcota+
                              Ascomycota+
                              Mortierellomycota+
                              Basidiomycota+
                              Chytridiomycota+
                              Rozellomycota,
                            data = data_asv,
                            importance=TRUE,            
                            ntree=1000,            
                            nrep=100,           
                            num.cores = 1)            

print(RF_model_taxa2)    
imp.000 <- importance(RF_model_taxa2,scale = TRUE)            
imp.000
# 提取rfPermute模型的R²
rfPermute_r2 <- tail(RF_model_taxa2$rf$rsq, 1)
cat("rfPermute模型OOB R² =", round(rfPermute_r2, 4), "\n")

# 输出示例：
#              %IncMSE IncNodePurity   p.val
# Chloroflexi    15.23        320.45   0.003
# Proteobacteria 12.67        285.12   0.012

# # 安装包（如果尚未安装）
# install.packages("writexl")

# # 加载包并导出
# library(writexl)
# write_xlsx(as.data.frame(imp.000), "variable_importance.xlsx")
# 
# # 将结果转换为数据框并导出Excel
# results_df <- as.data.frame(RF_model_taxa)
# write.xlsx(results_df, file = "FR_taxaN2O", rowNames = FALSE)
# importance(RF_model_taxa)


#### 线性混合模型分析微生物对 qN2O的影响####




# # 导入数据 
# dir()
# data_asv=read.csv("E:/Huangxiaoyi/N2Odata/N2O_Dateset.csv", sep=",", header = T)
# names(data_asv)
# str(data_asv)


# # 选择数据进行标准化
# data_asv1 <- data_asv %>% dplyr::select("Chloroflexi",
#                                         "Proteobacteria",
#                                         "Actinobacteriota",
#                                         "Verrucomicrobiota",
#                                         "Bacteroidota",
#                                         "Planctomycetota",
#                                         "Acidobacteriota",
#                                         "Gemmatimonadota",
#                                         "Deinococcota",
#                                         "Myxococcota",
#                                         "Ascomycota",
#                                         "Mortierellomycota",
#                                         "Basidiomycota",
#                                         "Chytridiomycota",
#                                         "Rozellomycota",
#                                         "Name")
# rownames(data_asv1) <- data_asv1$Name
# data_asv1 <- data_asv1 %>% dplyr::select(-"Name")
# data1_z2 <- as.data.frame(scale(data_asv1))
# data1_z2$Name <- rownames(data_asv1)
# str(data1_z2)


# #线性混合模型
# LMM_qN2O_taxa<-lmer(qN2O~Chloroflexi+
#                       #Proteobacteria+
#                       Actinobacteriota+
#                       Verrucomicrobiota+
#                       Bacteroidota+
#                       Planctomycetota+
#                       Acidobacteriota+
#                       Gemmatimonadota+
#                       Deinococcota+
#                       Myxococcota+
#                       Ascomycota+
#                       Mortierellomycota+
#                       Basidiomycota+
#                       Chytridiomycota+
#                       Rozellomycota+(1|Landuse),data=data_asv)
# shapiro.test(resid(LMM_qN2O_taxa))
# summary(LMM_qN2O_taxa)
# tab_model(LMM_qN2O_taxa)
# anova(LMM_qN2O_taxa,ddf="Kenward-Roger")
# r_squared_sub_4 <- r.squaredGLMM(LMM_qN2O_taxa)
# print(r_squared_sub_4)
# AIC(LMM_qN2O_taxa)
# plot(check_collinearity(LMM_qN2O_taxa))
# #普通线性模型
# LMM_qN2O_taxa3<-lm(qN2O~Chloroflexi+
#                       #Proteobacteria+
#                       Actinobacteriota+
#                       Verrucomicrobiota+
#                       Bacteroidota+
#                       Planctomycetota+
#                       Acidobacteriota+
#                       Gemmatimonadota+
#                       Deinococcota+
#                       Myxococcota+
#                       Ascomycota+
#                       Mortierellomycota+
#                       Basidiomycota+
#                       Chytridiomycota+
#                       Rozellomycota,data=data_asv)
# 
# ##共线性检查
# library(performance)
# plot(check_collinearity(LMM_qN2O_taxa3))###VIF<10
# # 查看模型公式
# library(MuMIn)
# ##最优模型选择
# options(na.action = "na.fail")
# dre_model_N2O_Q<- dredge(LMM_qN2O_taxa3, 
#                          
#                          extra = c("R^2", F = function(x)
#                            summary(x)$fstatistic[[1]]))
# sub_model_N2O_Q <- subset(dre_model_N2O_Q, delta < 2)
# summary(sub_model_N2O_Q)
# 
# ##提取模型的参数
# mean_R2=mean(sub_model_N2O_Q$`R^2`)
# mean_AICc=mean(sub_model_N2O_Q$`AICc`)
# mean_delta_AICc=mean(sub_model_N2O_Q$`delta`)
# mean_R2
# mean_AICc
# mean_delta_AICc
# 
# ##模型平均值
# aveg_model_N2O_Q<-model.avg(dre_model_N2O_Q, subset = delta < 2)
# summary(aveg_model_N2O_Q)
# # 将结果转换为数据框并导出Excel
# results_df <- as.data.frame(sub_model_N2O_Q)
# write.xlsx(results_df, file = "最优模型结果.xlsx", rowNames = FALSE)
# sw(aveg_model_N2O_Q)
# 
# library(broom)
# broom::tidy(aveg_model_N2O_Q)
# library(MuMIn)
# library(openxlsx)
# 仅提取截距
# intercept_value <- coef(LMM_qN2O_taxa3)["(Intercept)"]


##### 随机森林结果影响的绘图 ####
library(ggpubr)  
# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()
dir()
data_rf_taxa=read.csv("E:/黄小轶数据/N2Odata/N2O_variable_importance.csv", sep=",", header = T)

names(data_rf_taxa)
str(data_rf_taxa)
data_rf_taxa$Taxa <- factor(data_rf_taxa$Taxa, levels = c("Rozellomycota",
                                                          "Basidiomycota",
                                                          "Myxococcota",
                                                          "Chytridiomycota",
                                                          "Chloroflexi",
                                                          "Gemmatimonadota",
                                                          "Ascomycota",
                                                          "Bacteroidota",
                                                          "Deinococcota",
                                                          "Verrucomicrobiota",
                                                          "Actinobacteriota",
                                                          "Planctomycetota",
                                                          "Acidobacteriota",
                                                          "Proteobacteria",
                                                          "Mortierellomycota"
                                                          
                                                         ), 
                            ordered= T)

# 每个微生物随机森林对qN2O影响的图
rf_taxa_qN2O <- ggbarplot(data_rf_taxa, 
                          x = "Taxa", 
                          y = "X.IncMSE",
                          fill = "#8a508f",
                          orientation = "horiz",
                          palette = "uchicago") +
  scale_y_continuous(position = "right") +  # Y轴在左侧
   #scale_x_discrete(position = "bottom") +
  #scale_y_continuous(expand = c(0, 0))+# 将柱状图水平显示
  theme_bw()+
  theme(
    # panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 18,color = "black"),
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),  # 强制移除 x 轴标题
    axis.title.y = element_blank()  # 强制移除 y 轴标题
  )


rf_taxa_qN2O
ggsave("PlotN2O/rf_taxa_qN2O.png", plot = rf_taxa_qN2O, device = "png", width = 5, height = 6, dpi = 600, bg = "transparent")


### 随机森林的partial analysis

#1# Mortierellomycota
Mortierellomycota <- partial(RF_model_taxa, pred.var = "Mortierellomycota", plot = TRUE, rug = TRUE,
                             main = "Partial Dependence of medv on rm",
                             xlab = "Average Number of Rooms per Dwelling (rm)",
                             ylab = "Predicted medv")
Mortierellomycota

Mortierellomycota <- partial(RF_model_taxa, 
                             pred.var = "Mortierellomycota", 
                             plot = FALSE)
head(Mortierellomycota)

Mortierellomycota <- ggplot(Mortierellomycota, aes(x = Mortierellomycota, y = yhat)) +
  geom_line(color = "black", size = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),  
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 12, color = "brown"),  
    legend.title = element_text(size = 14, color = "brown"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1))
Mortierellomycota
ggsave("PlotN2O/fig_Mortierellomycota.jpg", plot = Mortierellomycota, device = "jpeg", width = 4, height = 3, dpi = 600)


#2# Proteobacteria
Proteobacteria <- partial(RF_model_taxa, pred.var = "Proteobacteria", plot = TRUE, rug = TRUE,
                          main = "Partial Dependence of medv on rm",
                          xlab = "Average Number of Rooms per Dwelling (rm)",
                          ylab = "Predicted medv")
Proteobacteria

Proteobacteria <- partial(RF_model_taxa, 
                          pred.var = "Proteobacteria", 
                          plot = FALSE)
head(Proteobacteria)

Proteobacteria <- ggplot(Proteobacteria, aes(x = Proteobacteria, y = yhat)) +
  geom_line(color = "black", size = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),  
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 12, color = "brown"),  
    legend.title = element_text(size = 14, color = "brown"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1))
Proteobacteria
ggsave("PlotN2O/fig_Proteobacteria.jpg", plot = Proteobacteria, device = "jpeg", width = 4, height = 3, dpi = 600)


#3# Acidobacteriota
Acidobacteriota <- partial(RF_model_taxa, pred.var = "Acidobacteriota", plot = TRUE, rug = TRUE,
                           main = "Partial Dependence of medv on rm",
                           xlab = "Average Number of Rooms per Dwelling (rm)",
                           ylab = "Predicted medv")
Acidobacteriota

Acidobacteriota <- partial(RF_model_taxa, 
                           pred.var = "Acidobacteriota", 
                           plot = FALSE)
head(Acidobacteriota)

Acidobacteriota <- ggplot(Acidobacteriota, aes(x = Acidobacteriota, y = yhat)) +
  geom_line(color = "black", size = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),  
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 12, color = "brown"),  
    legend.title = element_text(size = 14, color = "brown"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1))
Acidobacteriota
ggsave("PlotN2O/fig_Acidobacteriota.jpg", plot = Acidobacteriota, device = "jpeg", width = 4, height = 3, dpi = 600)


#4# Planctomycetota
Planctomycetota <- partial(RF_model_taxa, pred.var = "Planctomycetota", plot = TRUE, rug = TRUE,
                           main = "Partial Dependence of medv on rm",
                           xlab = "Average Number of Rooms per Dwelling (rm)",
                           ylab = "Predicted medv")
Planctomycetota

Planctomycetota <- partial(RF_model_taxa, 
                           pred.var = "Planctomycetota", 
                           plot = FALSE)
head(Planctomycetota)

Planctomycetota <- ggplot(Planctomycetota, aes(x = Planctomycetota, y = yhat)) +
  geom_line(color = "black", size = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),  
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 12, color = "brown"),  
    legend.title = element_text(size = 14, color = "brown"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1))
Planctomycetota
ggsave("PlotN2O/fig_Planctomycetota.jpg", plot = Planctomycetota, device = "jpeg", width = 4, height = 3, dpi = 600)

#5# Actinobacteriota
Actinobacteriota <- partial(RF_model_taxa, pred.var = "Actinobacteriota", plot = TRUE, rug = TRUE,
                       main = "Partial Dependence of medv on rm",
                       xlab = "Average Number of Rooms per Dwelling (rm)",
                       ylab = "Predicted medv")
Actinobacteriota

Actinobacteriota <- partial(RF_model_taxa, 
                       pred.var = "Actinobacteriota", 
                       plot = FALSE)
head(Actinobacteriota)

Actinobacteriota <- ggplot(Actinobacteriota, aes(x = Actinobacteriota, y = yhat)) +
  geom_line(color = "black", size = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),  
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 12, color = "brown"),  
    legend.title = element_text(size = 14, color = "brown"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1))
Actinobacteriota
ggsave("PlotN2O/fig_Actinobacteriota.jpg", plot = Actinobacteriota, device = "jpeg", width = 4, height = 3, dpi = 600)

#6# Verrucomicrobiota
Verrucomicrobiota <- partial(RF_model_taxa, pred.var = "Verrucomicrobiota", plot = TRUE, rug = TRUE,
                             main = "Partial Dependence of medv on rm",
                             xlab = "Average Number of Rooms per Dwelling (rm)",
                             ylab = "Predicted medv")
Verrucomicrobiota

Verrucomicrobiota <- partial(RF_model_taxa, 
                             pred.var = "Verrucomicrobiota", 
                             plot = FALSE)
head(Verrucomicrobiota)

Verrucomicrobiota <- ggplot(Verrucomicrobiota, aes(x = Verrucomicrobiota, y = yhat)) +
  geom_line(color = "black", size = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),  
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 12, color = "brown"),  
    legend.title = element_text(size = 14, color = "brown"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1))
Verrucomicrobiota
ggsave("PlotN2O/fig_Verrucomicrobiota.jpg", plot = Verrucomicrobiota, device = "jpeg", width = 4, height = 3, dpi = 600)

#7# Deinococcota
Deinococcota <- partial(RF_model_taxa, pred.var = "Deinococcota", plot = TRUE, rug = TRUE,
                        main = "Partial Dependence of medv on rm",
                        xlab = "Average Number of Rooms per Dwelling (rm)",
                        ylab = "Predicted medv")
Deinococcota

Deinococcota <- partial(RF_model_taxa, 
                        pred.var = "Deinococcota", 
                        plot = FALSE)
head(Deinococcota)

Deinococcota <- ggplot(Deinococcota, aes(x = Deinococcota, y = yhat)) +
  geom_line(color = "black", size = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),  
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 12, color = "brown"),  
    legend.title = element_text(size = 14, color = "brown"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1))
Deinococcota
ggsave("PlotN2O/fig_Deinococcota.jpg", plot = Deinococcota, device = "jpeg", width = 4, height = 3, dpi = 600)


#8# Bacteroidota
Bacteroidota <- partial(RF_model_taxa, pred.var = "Bacteroidota", plot = TRUE, rug = TRUE,
                        main = "Partial Dependence of medv on rm",
                        xlab = "Average Number of Rooms per Dwelling (rm)",
                        ylab = "Predicted medv")
Bacteroidota

Bacteroidota <- partial(RF_model_taxa, 
                        pred.var = "Bacteroidota", 
                        plot = FALSE)
head(Bacteroidota)

Bacteroidota <- ggplot(Bacteroidota, aes(x = Bacteroidota, y = yhat)) +
  geom_line(color = "black", size = 1)+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),  
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 12, color = "brown"),  
    legend.title = element_text(size = 14, color = "brown"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1))
Bacteroidota
ggsave("PlotN2O/fig_Bacteroidota.jpg", plot = Bacteroidota, device = "jpeg", width = 4, height = 3, dpi = 600)







#### 用SEM分析CUE等与呼吸的关系 #########
library(piecewiseSEM)
library(tidyverse)
library(nlme)
library(lme4)
library(brms)
library(ape)
library(caper)
library(semPlot)
library(car) 

qN2O_sem<-psem(
  lme(log(qN2O)~ShannonB+ShannonF+CUE+Turnover*NUE+GNM, random = ~ 1 | Landuse,data= data4),
  lme(CUE~ShannonB+ShannonF+Turnover, random = ~ 1 | Landuse,data= data4),
  lme(NUE~ShannonB+ShannonF+Turnover, random = ~ 1 | Landuse,data= data4),
  lme(Turnover~ShannonB+ShannonF, random = ~ 1 | Landuse,data= data4),
  lme(ShannonB~Diversity2, random = ~ 1 | Landuse,data= data4),
  lme(ShannonF~Diversity2, random = ~ 1 | Landuse,data= data4),
  data=data4
)

# 
# qco2_sem<-psem(
#   lme(log(qco2)~ShannonB+ShannonF+CUE+Turnover*NUE, random = ~ 1 | Landuse,data= data4),
#   lme(CUE~ShannonB+ShannonF+Turnover, random = ~ 1 | Landuse,data= data4),
#   lme(NUE~ShannonB+ShannonF+Turnover, random = ~ 1 | Landuse,data= data4),
#   lme(Turnover~ShannonB+ShannonF, random = ~ 1 | Landuse,data= data4),
#   # lme(ShannonB~Diversity1, random = ~ 1 | Landuse,data= data4),
#   # lme(ShannonF~Diversity1, random = ~ 1 | Landuse,data= data4),
#   data=data4
# )

summary(qN2O_sem)
vif(qN2O_sem)
basisSet(qN2O_sem)
dSep(qN2O_sem)#conditioning=T,显示所有自变量
fisherC(qN2O_sem)#
coefs(qN2O_sem)#intercept=T,给出截距
rsquared(qN2O_sem)#默认方法是delta,可改为method="theoretical"则是用的日本人的方法计算R2了
AIC(qN2O_sem)
plot(qN2O_sem,node_attrs = list(shape = "rectangle", color = "Bule",
                                fillcolor = "Red"))



#### 微生物多样性绘图 ####
# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()
# 导入数据 
library(scales)
data_div=read.csv("E:/黄小轶数据/N2Odata/N2O_Dateset.csv", sep=",", header = T)
names(data_div)
str(data_div)

#设置分类变量
data_div$Diversity <- factor(data_div$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data_div$Landuse <- factor(data_div$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data_div)

# ggpubr绘制图  #
library(ggbarplot)
head(data_div)
# Bacteria

Bac_diversity <- ggbarplot(data_div,
                           x = "Diversity",
                           y = "ShannonB",
                           #color = "Diversity",
                           add = "mean_se",
                           palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                           fill = "Diversity",
                           facet.by = "Landuse")+
   scale_y_continuous(limits = c(-0.5, 10.5))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(color = "black", 
                                        size = 1, fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),  # 设置图形背景透明
        panel.background = element_rect(fill = "transparent", color = NA))+
  border(color = "black", size = 1)
Bac_diversity

  ggsave("PlotN2O/Bac_diversityN2O.png", 
       plot = Bac_diversity, 
       device = "png", 
       width = 10, 
       height = 4, 
       dpi = 600, 
       bg = "transparent")

  library(rstatix)
  library(dplyr)
  
  # 按 Landuse 和 Diversity 分组检查正态性
  norm_test <- data_div %>%
    group_by(Landuse, Diversity) %>%
    shapiro_test(ShannonB)
  print(norm_test)
  
  # 检查方差齐性（Levene检验）
  levene_test(ShannonB ~ interaction(Landuse, Diversity), data = data_div)
  
 ##非参数检验### # 对每个 Landuse 类型进行Kruskal-Wallis检验
  kruskal_results <- data_div %>%
    group_by(Landuse) %>%
    kruskal_test(ShannonB ~ Diversity) %>%
    adjust_pvalue(method = "bonferroni")
  print(kruskal_results)
  
  # 事后Dunn检验（查看两两比较的显著性）
  dunn_results <- data_div %>%
    group_by(Landuse) %>%
    dunn_test(ShannonB ~ Diversity, p.adjust.method = "BH") %>%  # FDR校正
    add_significance()
  print(dunn_results)
  
  library(ggpubr)
  
  Bac_diversity + 
    stat_pvalue_manual(
      dunn_results,
      label = "p.adj.signif", 
      y.position = seq(9, 12, length.out = nrow(dunn_results)),  # 根据数据范围调整
      step.increase = 0.1,
      tip.length = 0.01
    ) +
    labs(caption = "NS: p > 0.05, *: p ≤ 0.05, **: p ≤ 0.01, ***: p ≤ 0.001")
  
  
#####参数检验 ANOVA + Tukey HSD（参数检验）
  
  # 对每个 Landuse 类型分别进行ANOVA
  anova_results <- data_div %>%
    group_by(Landuse) %>%
    anova_test(ShannonB ~ Diversity) %>%
    adjust_pvalue(method = "bonferroni")
  print(anova_results)
  
  # 事后多重比较（Tukey HSD）
  tukey_results <- data_div %>%
    group_by(Landuse) %>%
    tukey_hsd(ShannonB ~ Diversity) %>%
    add_significance()
  print(tukey_results)

  

# Fungi 真菌
Fun_diversity <- ggbarplot(data_div,
                           x = "Diversity",
                           y = "ShannonF",
                           #color = "Diversity",
                           add = "mean_sd",
                           palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                           fill = "Diversity",
                           facet.by = "Landuse")+
  scale_y_continuous(limits = c(-0.5, 10.5),labels = number_format(accuracy = 0.1))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(color = "black", 
                                        size = 1, fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),  # 设置图形背景透明
        panel.background = element_rect(fill = "transparent", color = NA))+
  border(color = "black", size = 1)
Fun_diversity

ggsave("E:/黄小轶数据/N2Odata/PlotN2O/Fun_diversityN2O.png", 
       plot = Fun_diversity, 
       device = "png", 
       width = 10, 
       height = 4, 
       dpi = 600, 
       bg = "transparent")
# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(ShannonF ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)


#### 各变量与梯度关系的图 ####
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(rcartocolor)

# 导入数据 
data1=read.csv("E:/黄小轶数据/N2Odata/N2O_Dateset.csv", sep=",", header = T)
names(data1)
str(data1)

#设置分类变量
#data1$Plot <- as.factor(data1$Plot)
data1$Diversity <- factor(data1$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data1$Landuse <- factor(data1$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data1)

# ## ShannonB by Diversity
# figShanB_Diver <- ggboxplot(data1,
#                              x = "Diversity",
#                              y = "ShannonB",
#                              combine= TRUE,
#                              add = "jitter",
#                              fill = "Diversity", 
#                              #scales = "free",
#                              palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
#                              facet.by = c("Landuse"))+
#   #scale_y_continuous(limits = c(-500, 50))+
#   theme(axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         strip.text = element_text(size = 18, face = "bold"),
#         strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
#   border(color = "black", size = 1.5)
# figShanB_Diver
# ggsave("PlotN2O/figShanB_Diver.png", plot = figShanB_Diver, device = "png", width = 6, height = 6, dpi = 490, bg = "transparent")
# 
# 
# ## ShannonF by Diversity
# figShanF_Diver <- ggboxplot(data1,
#                              x = "Diversity",
#                              y = "ShannonF",
#                              combine= TRUE,
#                              add = "jitter",
#                              fill = "Diversity", 
#                              #scales = "free",
#                              palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
#                              facet.by = c("Landuse"))+
#   #scale_y_continuous(limits = c(-500, 50))+
#   theme(axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         strip.text = element_text(size = 18, face = "bold"),
#         strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
#   border(color = "black", size = 1.5)
# figShanF_Diver
# ggsave("PlotN2O/figShanF_Diver.png", plot = figShanF_Diver, device = "png", width = 6, height = 6, dpi = 490, bg = "transparent")


##########各变量条形图########

# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()
# Turnover by Diversity

figTurn_Diver <- ggbarplot(data_div,
                           x = "Diversity",
                           y = "Turnover",
                           #color = "Diversity",
                           add = "mean_se",
                           palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                           fill = "Diversity",
                           facet.by = "Landuse")+
  # scale_y_continuous(limits = c(0, 10.5))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(color = "black", 
                                        size = 1, fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),  # 设置图形背景透明
        panel.background = element_rect(fill = "transparent", color = NA))+
  border(color = "black", size = 1)
figTurn_Diver

ggsave("PlotN2O/figTurn_Diver.png", 
       plot = figTurn_Diver, 
       device = "png", 
       width = 10, 
       height = 4, 
       dpi = 600, 
       bg = "transparent")

##########参数检验 ANOVA + Tukey HSD（参数检验）

# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(Turnover ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)



# # GNM by Diversity
# figGNM_Diver<- ggboxplot(data1,
#                           x = "Diversity",
#                           y = "GNM",
#                           combine= TRUE,
#                           add = "jitter",
#                           fill = "Diversity", 
#                           #scales = "free",
#                          palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
#                           facet.by = c("Landuse"))+
#   #scale_y_continuous(limits = c(-500, 50))+
#   theme(axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         strip.text = element_text(size = 18, face = "bold"),
#         strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
#   border(color = "black", size = 1.5)
# figGNM_Diver
# ggsave("PlotN2O/figGNM_Diver.png", plot = figGNM_Diver, device = "png", width = 6, height = 6, dpi = 490, bg = "transparent")
# 

# GNM by Diversity
figGNM_Diver <- ggbarplot(data_div,
                           x = "Diversity",
                           y = "GNM",
                           #color = "Diversity",
                           add = "mean_se",
                           palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                           fill = "Diversity",
                           facet.by = "Landuse")+
  # scale_y_continuous(limits = c(0, 10.5))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(color = "black", 
                                        size = 1, fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),  # 设置图形背景透明
        panel.background = element_rect(fill = "transparent", color = NA))+
  border(color = "black", size = 1)
figGNM_Diver

ggsave("PlotN2O/figGNM_Diver.png", 
       plot = figGNM_Diver, 
       device = "png", 
       width = 10, 
       height = 4, 
       dpi = 600, 
       bg = "transparent")
##########参数检验 ANOVA + Tukey HSD（参数检验）

# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(GNM ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)

#MBN与多样性梯度的关系
# MBN by Diversity
figMBN_Diver <- ggboxplot(data1,
                          x = "Diversity",
                          y = "MBN",
                          combine= TRUE,
                          add = "jitter",
                          fill = "Diversity", 
                          #scales = "free",
                          palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                          facet.by = c("Landuse"))+
  #scale_y_continuous(limits = c(-500, 50))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
figMBN_Diver
ggsave("PlotN2O/figMBN_Diver.png", plot = figMBN_Diver, device = "png", width = 8, height = 6, dpi = 600, bg = "transparent")

# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(MBN ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)

# MBC by Diversity
figMBC_Diver <- ggboxplot(data1,
                          x = "Diversity",
                          y = "MBC",
                          combine= TRUE,
                          add = "jitter",
                          fill = "Diversity", 
                          #scales = "free",
                          palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                          facet.by = c("Landuse"))+
  #scale_y_continuous(limits = c(-500, 50))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
figMBC_Diver
ggsave("PlotN2O/figMBC_Diver.png", plot = figMBC_Diver, device = "png", width = 8, height = 6, dpi = 600, bg = "transparent")

# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(MBC ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)



# # CUE by Diversity
# figCUE_Diver <- ggboxplot(data1,
#                              x = "Diversity",
#                              y = "CUE",
#                              combine= TRUE,
#                              add = "jitter",
#                              fill = "Diversity", 
#                              #scales = "free",
#                           palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
#                              facet.by = c("Landuse"))+
#   #scale_y_continuous(limits = c(-500, 50))+
#   theme(axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         strip.text = element_text(size = 18, face = "bold"),
#         strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
#   border(color = "black", size = 1.5)
# figCUE_Diver
# ggsave("PlotN2O/figCUE_Diver.png", plot = figCUE_Diver, device = "png", width = 6, height = 6, dpi = 490, bg = "transparent")

# CUE by Diversity
figCUE_Diver <- ggbarplot(data_div,
                          x = "Diversity",
                          y = "CUE",
                          #color = "Diversity",
                          add = "mean_se",
                          palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                          fill = "Diversity",
                          facet.by = "Landuse")+
  # scale_y_continuous(limits = c(0, 10.5))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(color = "black", 
                                        size = 1, fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),  # 设置图形背景透明
        panel.background = element_rect(fill = "transparent", color = NA))+
  border(color = "black", size = 1)
figCUE_Diver

ggsave("PlotN2O/figCUE_Diver.png", 
       plot = figCUE_Diver, 
       device = "png", 
       width = 10, 
       height = 4, 
       dpi = 600, 
       bg = "transparent")


##########参数检验 ANOVA + Tukey HSD（参数检验）

# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(CUE ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)



# NUE by Diversity
figNUE_Diver <- ggbarplot(data_div,
                          x = "Diversity",
                          y = "NUE",
                          #color = "Diversity",
                          add = "mean_se",
                          palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                          fill = "Diversity",
                          facet.by = "Landuse")+
  # scale_y_continuous(limits = c(0, 10.5))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(color = "black", 
                                        size = 1, fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),  # 设置图形背景透明
        panel.background = element_rect(fill = "transparent", color = NA))+
  border(color = "black", size = 1)
figNUE_Diver

ggsave("PlotN2O/figNUE_Diver.png", 
       plot = figNUE_Diver, 
       device = "png", 
       width = 10, 
       height = 4, 
       dpi = 600, 
       bg = "transparent")

library(rstatix)
library(dplyr)
# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(NUE ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)

########评估N2O混合效应模型因素权重关系#######
###########sd值需要乘以1.96
# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()

library(readxl)
data1 <- read_excel("E:/黄小轶数据/N2Odata/N2O混合效应模型结果.xlsx")


library(tidyverse)
library(ggplot2)
# 示例：自定义顺序（替换为你的实际变量名）
data1$variable <- factor(data1$variable, levels = c("TurnoverNUE","Turnover","CUE", "NUE","GNM","ShannonF","ShannonB"), ordered= T)

figure_value <- ggplot(data1,aes(value,variable,col=subgroup))+
  geom_point(size=6,shape=15)+
  geom_errorbar(aes(xmin=value-sd, xmax=value+sd), size= 0.8, width=0.01)+
  geom_vline(aes(xintercept=0),linetype=2,col="black")+ 
  scale_x_continuous(limits = c(-1.5,1.7))+
  scale_y_discrete(position = 'right')+
  scale_color_manual(values = c("#730220", "#194a7a" ),) +  # 使用自定义配色
  labs(y=NULL)+
  labs(x=NULL)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 15,color = "black"),
        axis.text.y = element_text(size = 15,color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        legend.position="none")
figure_value

ggsave("PlotN2O/figure_value.png", plot = figure_value, device = "png", width = 4, height = 6, dpi = 600, bg = "transparent")

data2 <- read_excel("E:/黄小轶数据/N2Odata/N2O_Relative effect of estimates (%).xlsx")

fig_estimates <- ggplot(data = data2,aes(Year, proporation, fill=subgroup))+
  geom_bar(stat="identity", width = 0.7,size=0.5)+# 柱子宽度和边框粗细
  scale_fill_manual(values =c("#730220", "#194a7a" ),) +  # 使用自定义配色
  theme_classic(base_size = 18)+
  scale_y_continuous(name = "Relative effect of estimates (%)")+
  theme(line = element_line(color = "black"),
        # 将坐标轴文本设为黑色
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x =element_blank())+
  theme(legend.position="none")+
  labs(x=NULL) 
fig_estimates
ggsave("PlotN2O/fig_estimates.png", plot = fig_estimates, device = "png", width = 2.2, height = 6, dpi = 600, bg = "transparent")


# 点线图显著性标记
sig_labels <- data.frame(
  variable = levels(data1$variable),
  x_pos = 1.4,
  label = c("**", "*", "", "", "*", "*", "***") # 根据实际结果填写
)

figure_value <- figure_value +
  geom_text(data = sig_labels, aes(x = x_pos, y = variable, label = label),
            size = 8, color ="black" )
figure_value

# 保存图形
ggsave("PlotN2O/figure_value_sig.png", figure_value, width = 4, height = 6,dpi = 600, bg = "transparent")


####热图#######

library(ggplot2)
library(reshape2)
library(dplyr)
library(corrplot)
library(psych)
library(pheatmap)
library(tidyverse)
library(ggsci)
library(aplot)
library(patchwork)
library(Hmisc)  # corr.test函数所在的包
library(openxlsx)  # 用于输出到Excel
###N2O##DATASET:
data1<-read.csv("E:/黄小轶数据/N2Odata/N2O_Dateset.csv",header=T,na.strings=c("NA"))

names(data1)

# 检查目标列是否为数值型
str(data1[c(10,11:15,18:21)])

corr<- corr.test(data1[c(10:12,14:15,18:21)],
                 data1[c(10:12,14:15,18:21)],use = 'pairwise',
                 method = 'pearson',adjust="none")
r1<-corr$r
p1<- round(corr$p,3)#3是小数点后三位的意思
p1[p1>=0 & p1 < 0.001] <- "***"
p1[p1>=0.001 & p1 < 0.01] <- "**"
p1[p1>=0.01 & p1 < 0.05] <- "*"
p1[p1>=0.05 & p1 <= 0.1] <- "."
p1[p1>=0.1 & p1 <= 1] <- " "
# 创建一个数据框，将r1和p1结合
result <- data.frame(
  Variable = rownames(r1),  # 假设物种名是行名
  
  qN2O = r1[, 1],
  MBC = r1[, 2],  # Growth与物种的相关系数
  MBN = r1[, 3],  # Respiration与物种的相关系数
  
  CUE = r1[, 4],  # Turnover_rate与物种的相关系数
  NUE = r1[, 5],
  GNM = r1[, 6], 
  Turnover = r1[, 7],
  ShannonB = r1[, 8],
  ShannonF = r1[, 9],
  
  qN2O_p = p1[, 1],
  MBC_p = p1[, 2],  # Growth的p值
  MBN_p = p1[, 3],  # Respiration的p值
  
  CUE_p = p1[, 4], 
  NUE_p = p1[, 5],
  GNM_p = p1[, 6],
  Turnover_p = p1[, 7],
  ShannonB_p = p1[, 8],
  ShannonF_p = p1[, 9]
  
)

# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()
# 将结果写入到Excel文件
write.xlsx(result, "correlations_N2O.xlsx", rowNames = TRUE)

library(reshape2)
library(ggplot2)

##先画O层的heatmap：
# 读取数据
cor_data <- read.xlsx("correlations_N2O.xlsx",  na.strings = c("NA"))
var_order <- cor_data$Variable  # 保留原始顺序
var_order <- var_order[!duplicated(var_order)]  # 去重并保留原始顺序
# 假设列 2–4 是相关系数，列 5–7 是星号
r_long <- reshape2::melt(cor_data[ ,2:11], id.vars = "Variable", variable.name = "Function", value.name = "r")
p_long <- reshape2::melt(cor_data[, c(2, 12:20)], id.vars = "Variable", variable.name = "Function", value.name = "stars")


# 去掉 Function 名里的 "_p"
p_long$Function <- gsub("_p", "", p_long$Function)

# 合并相关系数和星号
cor_long <- merge(r_long, p_long, by = c("Variable", "Function"))

# 可选：设定因子顺序（根据你原始数据的顺序）
cor_long$Variable <- factor(cor_long$Variable, levels = var_order)
cor_long$Function <- factor(cor_long$Function, levels = c("qN2O","MBC","MBN","CUE","NUE", "GNM", "Turnover","ShannonB", "ShannonF"  
                                                           ))

# 画图
hp_N2O  <- ggplot(cor_long, aes(x = Variable, y = Function, fill = r)) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = stars), size = 5, color = "black") +
  scale_fill_gradient2(low = "#f6c63c", high = "#692f7c", mid = "white", midpoint = 0,limits=c(-0.8,1)
  ) +
  theme_minimal() +
  scale_x_discrete(position = 'top') +
  theme(
    text = element_text(size = 18),
    axis.text.x = element_text(angle = 60, hjust = 0.06,size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank(),
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), 'cm')
  )

print(hp_N2O )
ggsave("PlotN2O/N2Ohp1.jpg", plot = hp_N2O , device = "jpeg", width = 8, height = 7, dpi = 600)




#####################################################################################################################################
#######################################################################################################################################

# 加载数据 + 训练 + 评估
data(iris)
set.seed(42)
train_index <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

# 训练模型
rf <- ranger(Species ~ ., data = train_data, num.trees = 100, importance = "impurity")

# 预测并评估
pred <- predict(rf, test_data)$predictions
confusionMatrix(pred, test_data$Species)

# 特征重要性
importance(rf) |> sort(decreasing = TRUE)


###############一般线性混合模型
# 读入数据 
data1=read.csv("E:/Huangxiaoyi/N2O数据csv/250409_N2O_Dateset_v1_Yang.csv", sep=",", header = T) # 这里的O处理名称变“C”
names(data1)
str(data1)
data1$Landuse <- as.factor(data1$Landuse)


# 选择数据进行标准化
data2 <- data1 %>% dplyr::select("Name","Turnover","CUE", "NUE","MBC","MBN","RichnessB", "RichnessF","SOC","GNM","ShannonB","ShannonF")
rownames(data2) <- data2$Name
data2 <- data2 %>% dplyr::select(-"Name")
data1_z <- as.data.frame(scale(data2))
data1_z$Name <- rownames(data1_z)
str(data1_z)


# 具有Landuse的标准化数据库
data3 <- data1 %>% dplyr::select("Name","Landuse","Diversity2","Diversity1","Diversity","Plot","N2O","qN2O")
data1_z <- merge(data3,data1_z,by = "Name")
names(data1_z)
str(data1_z)

#创建NUE和turnover的交互项
data1_z$turnoverNUE <- data1_z$Turnover*data1_z$NUE
data1_z$turnoverCUE <- data1_z$Turnover*data1_z$CUE
str(data1_z)



# ## 模型一：SOC全模型，去除共线性，最终拟加入的10个变量 ####
model_N2O_Q <- lm(N2O~Turnover*NUE+Turnover*CUE+MBN+GNM+MBC+SOC+ShannonB+ShannonF,
                  data=data1_z)
summary(model_N2O_Q)

##共线性检查
plot(check_collinearity(model_N2O_Q))###VIF<10
# 查看模型公式
formula(model_N2O_Q)

# 重新拟合模型（确保包含预测变量）
model_N2O_Q <- lm(N2O ~ Turnover + NUE + SOC + GNM + MBC + MBN + CUE + ShannonB + ShannonF +Turnover*NUE +Turnover*CUE , data = data1_z)


##最优模型选择
options(na.action = "na.fail")
dre_model_N2O_Q<- dredge(model_N2O_Q, 
                         
                         extra = c("R^2", F = function(x)
                           summary(x)$fstatistic[[1]]))
sub_model_N2O_Q <- subset(dre_model_N2O_Q, delta < 2)
summary(sub_model_N2O_Q)

##提取模型的参数
mean_R2=mean(sub_model_N2O_Q$`R^2`)
mean_AICc=mean(sub_model_N2O_Q$`AICc`)
mean_delta_AICc=mean(sub_model_N2O_Q$`delta`)
mean_R2
mean_AICc
mean_delta_AICc

##模型平均值
aveg_model_N2O_Q<-model.avg(dre_model_N2O_Q, subset = delta < 2)
summary(aveg_model_N2O_Q)
sw(aveg_model_N2O_Q)

library(broom)
broom::tidy(aveg_model_N2O_Q)





#############合并N2O和CH4的混合效应模型筛选结果的表格
install.packages(c("broom.mixed", "dplyr", "flextable", "tidyverse"))

library(broom.mixed)
library(dplyr)
library(flextable)

tidy_n2o <- tidy(LMM_qN2O, effects = "fixed") %>%
  mutate(
    term = gsub("`", "", term),
    stat_n2o = paste0(round(estimate, 2), 
                      " (", round(conf.low, 2), "–", round(conf.high, 2), 
                      ", ", ifelse(p.value < 0.001, "<0.001", round(p.value, 3)), ")")
  ) %>%
  select(term, stat_n2o)

tidy_ch4 <- tidy(LMM_qCH4_3, effects = "fixed") %>%
  mutate(
    term = gsub("`", "", term),
    stat_ch4 = paste0(round(estimate, 2), 
                      " (", round(conf.low, 2), "–", round(conf.high, 2), 
                      ", ", ifelse(p.value < 0.001, "<0.001", round(p.value, 3)), ")")
  ) %>%
  select(term, stat_ch4)


compare_table <- full_join(tidy_n2o, tidy_ch4, by = "term") %>%
  rename(Predictor = term)

ft <- flextable(compare_table) %>%
  set_header_labels(stat_n2o = "log(qN2O)", stat_ch4 = "qCH4") %>%
  autofit() %>%
  add_header_lines("Linear Mixed Models Comparison") %>%
  theme_booktabs()

ft
library(officer)

doc <- read_docx() %>%
  body_add_par("Model Comparison: log(qN2O) vs qCH4", style = "heading 1") %>%
  body_add_flextable(ft)
print(doc, target = "model_comparison.docx")

#这个N2O模型最好
# library(blme)
LMM_qN2O_4<-lmer(log(qN2O)~ShannonB+ShannonF+Turnover+turnoverNUE+NUE+CUE+GNM+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qN2O_4)) 
summary(LMM_qN2O_4) 
tab_model(LMM_qN2O_4)
anova(LMM_qN2O_4,ddf="Kenward-Roger")
r_squared_q4 <- r.squaredGLMM(LMM_qN2O_4) 
print(r_squared_q4)  
AIC(LMM_qN2O_4) 
plot(check_collinearity(LMM_qN2O_4))

#这个模型最好
LMM_qCH4_3<-lmer(qCH4~ShannonB+ShannonF+NUE+Turnover+CUE*Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qCH4_3))
summary(LMM_qCH4_3)
tab_model(LMM_qCH4_3)
anova(LMM_qCH4_3,ddf="Kenward-Roger")
r_squared_3 <- r.squaredGLMM(LMM_qCH4_3)
print(r_squared_3)
AIC(LMM_qCH4_3)
plot(check_collinearity(LMM_qCH4_3))

# 加载必要包
library(sjPlot)
library(lme4)

# 合并两个模型的输出表格
combined_table <- tab_model(
  LMM_qN2O_4, 
  LMM_qCH4_3,
  show.se = TRUE,          # 显示标准误
  show.stat = TRUE,        # 显示统计量
  show.p = TRUE,           # 显示p值
  show.df = TRUE,          # 显示自由度
  show.icc = TRUE,         # 显示组内相关性
  show.r2 = TRUE,          # 显示R平方
  show.aic = TRUE,         # 显示AIC值
  title = "混合效应模型结果对比",
  dv.labels = c("log(qN2O)", "qCH4"),  # 因变量标签
  pred.labels = c(
    "(Intercept)", "ShannonB", "ShannonF", "Turnover", "turnoverNUE",
    "NUE", "CUE", "GNM", "CUE:Turnover"  # 自定义预测变量标签
  ),
  collapse.ci = FALSE      # 不折叠置信区间
)

# 输出表格（支持HTML和Word格式）
combined_table

# 保存为Word文档
tab_model(
  LMM_qN2O_4, 
  LMM_qCH4_3,
  file = "Combined_Models.docx"
)

# 保存为HTML文件
tab_model(
  LMM_qN2O_4, 
  LMM_qCH4_3,
  file = "Combined_Models.html"
)



#############################分析门水平相关性
# 读入数据 
data1=read.csv("E:/Huangxiaoyi/N2Odata/N2O_Dateset.csv", sep=",", header = T) # 这里的O处理名称变“C”
names(data1)
str(data1)

# 选择数据进行标准化
data2 <- data1 %>% dplyr::select("Name","Turnover","CUE", "NUE","MBC","MBN","RichnessB", "RichnessF","SOC","GNM","ShannonB","ShannonF","Chloroflexi","Proteobacteria","Actinobacteriota", "Verrucomicrobiota",
                                 "Bacteroidota","Planctomycetota","Acidobacteriota","Gemmatimonadota",
                                 "Deinococcota","Myxococcota","Basidiomycota","Chytridiomycota","Rozellomycota")
rownames(data2) <- data2$Name
data2 <- data2 %>% dplyr::select(-"Name")
data1_z <- as.data.frame(scale(data2))
data1_z$Name <- rownames(data1_z)
str(data1_z)

# 具有Landuse的标准化数据库
data3 <- data1 %>% dplyr::select("Name","Landuse","Diversity2","Diversity1","Diversity","Plot","N2O","qN2O")
data4 <- merge(data3,data1_z,by = "Name")
names(data4)
str(data4)

#创建NUE和turnover的交互项
data4$turnoverNUE <- data4$Turnover*data4$NUE
data4$turnoverCUE <- data4$Turnover*data4$CUE
str(data4)


#### 分析变量间的相关性 ####
####相关性分析，做热图
library(tidyverse)
library(reshape2)
library(ggcorrplot)
library(ggplot2)
library(corrplot)
col2 = colorRampPalette(c('#053061', '#2166AC', '#4393C3', '#92C5DE',
                          '#D1E5F0', '#FFFFFF', '#FDDBC7', '#F4A582',
                          '#D6604D', '#B2182B', '#67001F'))
data4_heat <- data4%>% dplyr::select("N2O","qN2O",
                                     "Chloroflexi","Proteobacteria","Actinobacteriota", "Verrucomicrobiota",
                                     "Bacteroidota","Planctomycetota","Acidobacteriota","Gemmatimonadota",
                                     "Deinococcota","Myxococcota","Basidiomycota","Chytridiomycota","Rozellomycota")
data4_heat <- data1 %>% dplyr::select("N2O","qN2O",
                                      "Chloroflexi","Proteobacteria","Actinobacteriota", "Verrucomicrobiota",
                                      "Bacteroidota","Planctomycetota","Acidobacteriota","Gemmatimonadota",
                                      "Deinococcota","Myxococcota","Basidiomycota","Chytridiomycota","Rozellomycota")
cormat <- round(cor(data4_heat),2)
p.mat <- cor_pmat(data4_heat)

M <- cor(data4_heat, method = "spearman")
corheatmap = corrplot(M, col = col2(100),type = "lower", 
                      diag = FALSE, insig = "blank", 
                      method = 'square',p.mat = p.mat)
corheatmap




#########线性回归添加显著性标记
library(ggpubr)
library(ggpmisc)

fig_N2O_turnNUE <- ggscatter(
  LMM_data_1,
  x = "turnoverNUE",
  y = "Pre_qN2O",
  size = 5,
  color = "red",  # 点颜色
  palette = "uchicago",  # 调色板（如果按组着色）
  cor.coef = FALSE  # 关闭默认的相关系数
) +
  # 添加回归线
  stat_smooth(
    method = "lm", 
    color = "black", 
    se = TRUE
  ) +
  # 添加R²和p值标注（方法1：ggpubr的stat_cor）
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 4,  # 标签x轴位置
    label.y = 75,  # 标签y轴位置
    size = 6,
    color = "black"
  ) +
  # 或者使用ggpmisc的stat_poly_eq（方法2，更灵活）
  # stat_poly_eq(
  #   formula = y ~ x,
  #   aes(label = paste(..rr.label.., ..p.label.., sep = "*\", \"~")),
  #   parse = TRUE,
  #   label.x = "left",
  #   label.y = "top",
  #   size = 6
  # ) +
  # 坐标轴和主题调整
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent")
  )

# 保存图形
ggsave(
  "qN2O_turnoverNUE.png", 
  plot = fig_N2O_turnNUE, 
  width = 6, 
  height = 7, 
  dpi = 600, 
  bg = "transparent"
)

# 在代码最后添加
print(fig_N2O_turnNUE)
# 或者直接输入对象名
fig_N2O_turnNUE





