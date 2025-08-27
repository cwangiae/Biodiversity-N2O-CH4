

# 设置工作空间
setwd("E:/黄小轶数据/CH4data")
dir()
########黄小轶数据，杨雨欣分析
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(rcartocolor)

#导入数据
data_time_CH4=read.csv("E:/黄小轶数据/CH4data/Times_CH4.csv", sep=",", header = T)
names(data_time_CH4)

#设置分类变量
data_time_CH4$Dilution <- factor(data_time_CH4$Dilution, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data_time_CH4$Temp <- factor(data_time_CH4$Temp, levels = c( "15","17"), ordered= T)
data_time_CH4$Landuse <- factor(data_time_CH4$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data_time_CH4)
data_time_CH4 <- na.omit(data_time_CH4)

######CH4 with time#####
Time_CH4 <- ggline(data_time_CH4, x = "Time", y = "X15.17CH4.flux", 
                   add = "mean_se", 
                   color = "Dilution",
                   facet.by = c("Landuse"),
                   #scale_y_continuous(limits = c(-600, 100)),
                   palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),  # 自定义配色
                   size = 1.2)+ 
  scale_y_continuous(limits = c(-500, 200))+
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 21, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", size = 0.4))+ 
  border(color = "black", size = 1.5)

Time_CH4
ggsave("plotCH4/CH4_Time.jpg", plot = Time_CH4, device = "jpeg", width = 18, height = 7, dpi = 600)




# 设置工作空间
setwd("E:/黄小轶数据/CH4data")
dir()


library(ggplot2)
library(ggpubr)
library(scales) # 提供pseudo_log变换

# 数据准备
data_time_CH4 <- read.csv("E:/黄小轶数据/CH4data/Times_CH4.csv", header = TRUE)

# 设置分类变量
data_time_CH4$Dilution <- factor(data_time_CH4$Dilution, 
                                 levels = c("O", "D0", "D2", "D4", "D8"), 
                                 ordered = TRUE)
data_time_CH4$Temp <- factor(data_time_CH4$Temp, 
                             levels = c("15", "17"), 
                             ordered = TRUE)
data_time_CH4$Landuse <- factor(data_time_CH4$Landuse, 
                                levels = c("Forest", "Grassland", "Cropland"), 
                                ordered = TRUE)

# 删除NA值
data_time_CH4 <- na.omit(data_time_CH4)

# 定义颜色方案
dilution_colors <- c("#fcb735", "#bd2323", "#256e35", "#175b91", "#8a508f")

# 使用pseudo_log变换的绘图函数
plot_pseudo_log <- function(data, title = "") {
  ggline(data, 
         x = "Time", 
         y = "X15.17CH4.flux", 
         add = "mean_se", 
         color = "Dilution",
         facet.by = c("Landuse"),
         palette = dilution_colors,
         size = 1.2) +
    scale_y_continuous(
      trans = pseudo_log_trans(sigma = 0.1, base = 10),
      breaks = c(-500, -100, -10, 0, 10, 100, 200),
      labels = function(x) ifelse(x == 0, "0", x)
    ) +
    labs(title = title) +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 22),
          axis.title = element_blank(),
          plot.title = element_blank(),   # 移除分面标题
          legend.position = "none",    # 移除图例
          strip.text = element_text(size = 24, face = "bold"),
          strip.background = element_rect(fill = "grey", color = "black", size = 0.4))+ 
    border(color = "black", size = 1.5) 
    
}

# 绘制完整数据图形
full_plot <- plot_pseudo_log(data_time_CH4, "CH4通量 (pseudo_log变换)")

# 绘制仅稀释组图形 (排除O组)
# 定义颜色方案
dilution_colors <- c( "#bd2323", "#256e35", "#175b91", "#8a508f")
diluted_plot <- plot_pseudo_log(
  subset(data_time_CH4, Dilution != "O"),
  "仅稀释组 (D0-D8) CH4通量"
) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 0.1, base = 10), # 调整sigma参数
    breaks = c(-50, -10, -1, 0, 1, 10, 50),
    limits = c(-50, 50)
  )

# 显示图形
print(full_plot)
print(diluted_plot)

# 保存图形
ggsave("plotCH4/full_plot.jpg", plot = full_plot, device = "jpeg", width = 18, height = 7, dpi = 600)








rm(list = ls())

# 设置工作空间
setwd("E:/黄小轶数据/CH4data")
dir()
#### 绘制各变量图 ####
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(rcartocolor)

# 导入数据 
data1=read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv", sep=",", header = T)
names(data1)
str(data1)

#设置分类变量
#data1$Plot <- as.factor(data1$Plot)
data1$Diversity <- factor(data1$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data1$Landuse <- factor(data1$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data1)

# CH4累计排放量图

fig_CH4 <- ggboxplot(data1,
                     x = "Diversity",
                     y = "CH4",
                     combine= TRUE,
                     add = "jitter",
                     fill = "Diversity", 
                     scales = "free",
                     palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),  # 自定义配色
                     )+ 
  #geom_jitter(size = 2, width = 0.3, height = 0) +
  
  scale_y_continuous(limits = c(-500, 50))+
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
fig_CH4
ggsave("plotCH4/CH4.png", plot = fig_CH4, device = "png", width = 6, height = 6, dpi = 490, bg = "transparent")
#ggsave("CH4-2.png", plot = fig_CH4, device = "jpeg", width = 6, height = 6, dpi = 300, bg = "transparent")


## CH4 by Landuse
fig2_CH4 <- ggboxplot(data1,
                      x = "Diversity",
                      y = "CH4",
                      combine= TRUE,
                      add = "jitter",
                      fill = "Diversity", 
                      #scales = "free",
                      palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),  # 自定义配色
                      facet.by = c("Landuse"))+
  #scale_y_continuous(limits = c(-500, 50))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
fig2_CH4
ggsave("plotCH4/CH4_Landuse.png", plot = fig2_CH4, device = "png", width = 18, height = 7, dpi = 600, bg = "transparent")


## CH4 by Landuse缩放方法

library(ggpubr)
library(scales) # 用于伪对数变换

# 创建伪对数变换函数
pseudo_log_trans <- function(sigma = 1) {
  trans_new(
    "pseudo_log",
    transform = function(x) ifelse(x < 0, -log1p(-x/sigma), log1p(x/sigma)),
    inverse = function(x) ifelse(x < 0, -expm1(-x)*sigma, expm1(x)*sigma),
    breaks = log_breaks()
  )
}

# 绘制图形
fig2_CH4 <- ggboxplot(data1,
                      x = "Diversity",
                      y = "CH4",
                      combine = TRUE,
                      add = "jitter",
                      fill = "Diversity",
                      palette = c("#fcb735","#bd2323","#256e35","#175b91", "#8a508f"),
                      facet.by = c("Landuse")) +
  # 应用伪对数变换
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 10), # 调整sigma值控制线性区间
    breaks = c(-1000, -100, -10, 0, 10, 100, 1000), # 自定义刻度
    labels = function(x) round(x, 1) # 标签显示
   ) +
  theme(
    # axis.text.x = element_text(size = 18, angle = 45, hjust = 1), # 旋转x轴标签
    axis.text.x = element_text(size = 28),
    axis.text.y = element_text(size = 28),
    axis.title = element_blank(),
    legend.position = "none",    # 移除图例
    strip.text = element_text(size = 30, face = "bold"),
    strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2)
  ) +
  border(color = "black", size = 1.5) +
   labs() # 添加说明
# caption = "Y轴使用伪对数变换 (σ=10)"
fig2_CH4
# 保存图形
ggsave("plotCH4/CH4_Landuse_log.png", 
       plot = fig2_CH4, 
       device = "png", 
       width = 18, 
       height = 7, 
       dpi = 600, 
       bg = "transparent")



# qCH4不分生态系统
# 导入数据 
data1=read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv", sep=",", header = T)
names(data1)
str(data1)

#设置分类变量
#data1$Plot <- as.factor(data1$Plot)
data1$Diversity <- factor(data1$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data1$Landuse <- factor(data1$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data1)

fig_qCH4 <- ggboxplot(data1,
                      x = "Diversity",
                      y = "qCH4",
                      combine= TRUE,
                      add = "jitter",
                      fill = "Diversity", 
                      # scales = "free",
                      palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ))  # 自定义配色+ 
  #geom_jitter(size = 2, width = 0, height = 0) +
  #scale_y_continuous(limits = c(0, 2000))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
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

fig_qCH4
ggsave("plotCH4/qCH4.png", plot = fig_qCH4, device = "png", width = 6, height = 6, dpi = 300, bg = "transparent")


# 导入数据 
data1=read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv", sep=",", header = T)
names(data1)
str(data1)

#设置分类变量
#data1$Plot <- as.factor(data1$Plot)
data1$Diversity <- factor(data1$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data1$Landuse <- factor(data1$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data1)

## qCH4 by Landuse

fig2_qCH4 <- ggboxplot(data1,
                       x = "Diversity",
                       y = "qCH4",
                       combine= TRUE,
                       add = "jitter",
                       fill = "Diversity", 
                       #scales = "free",
                       palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                       facet.by = c("Landuse"))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
fig2_qCH4
ggsave("plotCH4/qCH4_landuse.jpg", plot = fig2_qCH4, device = "jpeg", width = 18, height = 7, dpi = 600)

## qCH4 by Landuse缩放方法

library(ggpubr)
library(scales) # 用于伪对数变换

# 创建伪对数变换函数
pseudo_log_trans <- function(sigma = 1) {
  trans_new(
    "pseudo_log",
    transform = function(x) ifelse(x < 0, -log1p(-x/sigma), log1p(x/sigma)),
    inverse = function(x) ifelse(x < 0, -expm1(-x)*sigma, expm1(x)*sigma),
    breaks = log_breaks()
  )
}

# 绘制图形
fig2_CH4 <- ggboxplot(data1,
                      x = "Diversity",
                      y = "qCH4",
                      combine = TRUE,
                      add = "jitter",
                      fill = "Diversity",
                      palette = c("#fcb735","#bd2323","#256e35","#175b91", "#8a508f"),
                      facet.by = c("Landuse")) +
  # 应用伪对数变换
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 10), # 调整sigma值控制线性区间
    breaks = c(-1000, -100, -10, 0, 10, 100, 1000), # 自定义刻度
    labels = function(x) round(x, 1) # 标签显示
  ) +
  theme(
    # axis.text.x = element_text(size = 18, angle = 45, hjust = 1), # 旋转x轴标签
    axis.text.x = element_text(size = 28),
    axis.text.y = element_text(size = 28),
    axis.title = element_blank(),
    legend.position = "none",    # 移除图例
    strip.text = element_text(size = 30, face = "bold"),
    strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2)
  ) +
  border(color = "black", size = 1.5) +
  labs() # 添加说明
# caption = "Y轴使用伪对数变换 (σ=10)"
fig2_CH4
# 保存图形
ggsave("plotCH4/qCH4_Landuse_log.png", 
       plot = fig2_CH4, 
       device = "png", 
       width = 18, 
       height = 7, 
       dpi = 600, 
       bg = "transparent")

###qCH4
# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data1 %>%
  group_by(Landuse) %>%
  anova_test(qCH4 ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)
###CH4
# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data1 %>%
  group_by(Landuse) %>%
  anova_test(CH4 ~ Diversity) %>%
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
data1=read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv", sep=",", header = T) # 这里的O处理名称变“C”
names(data1)
str(data1)

# 选择数据进行标准化
data2 <- data1 %>% dplyr::select("Name","Turnover","CUE", "NUE","MBC","RichnessB", "MBN","RichnessF","SOC","GNM","Water","ShannonB","ShannonF")
rownames(data2) <- data2$Name
data2 <- data2 %>% dplyr::select(-"Name")
data1_z <- as.data.frame(scale(data2))
data1_z$Name <- rownames(data1_z)
str(data1_z)

# 具有Landuse的标准化数据库
data3 <- data1 %>% dplyr::select("Name","Landuse","Diversity2","Diversity1","Diversity","Plot","CH4","qCH4","qCH4_2")
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
data4_heat <- data4 %>% dplyr::select("CH4","qCH4",
                                      "ShannonB","ShannonF",
                                      "Turnover","CUE","NUE",
                                      "MBC","MBN",)
data4_heat <- data1 %>% dplyr::select("CH4","qCH4",
                                      "ShannonB","ShannonF",
                                      "Turnover","CUE","NUE",
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
data4 <- data4 %>% rename(Diversity2 = Diversity)
data4 <- data4 %>% rename(Diversity = Diversity1)
str(data4)

# # 方法3-用factor对diversity进行转换
data4$Diversity <- factor(data4$Diversity, levels = c("C", "D0", "D2","D4","D8"), ordered= T)
data4$Landuse <- factor(data4$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)

#### CH4的模型####
LMM_CH4<-lmer(CH4~Diversity1+(1|Landuse),data=data4)
shapiro.test(resid(LMM_CH4))
summary(LMM_CH4)
tab_model(LMM_CH4)
anova(LMM_CH4,ddf="Kenward-Roger")
r_squared <- r.squaredGLMM(LMM_CH4)
print(r_squared)
AIC(LMM_CH4)
# plot(check_collinearity(LMM_CH4))

# 
LMM_CH4_1<-lmer(CH4~ShannonB+ShannonF+(1|Landuse),data=data4)
shapiro.test(resid(LMM_CH4_1))
summary(LMM_CH4_1)
tab_model(LMM_CH4_1)
anova(LMM_CH4_1,ddf="Kenward-Roger")
r_squared_1 <- r.squaredGLMM(LMM_CH4_1)
print(r_squared_1)
AIC(LMM_CH4_1)
plot(check_collinearity(LMM_CH4_1))

# 高度共线性，模型不能使用
LMM_CH4_2<-lmer(CH4~ShannonB+ShannonF+MBC+(1|Landuse),data=data4)
shapiro.test(resid(LMM_CH4_2))
summary(LMM_CH4_2)
tab_model(LMM_CH4_2)
anova(LMM_CH4_2,ddf="Kenward-Roger")
r_squared_2 <- r.squaredGLMM(LMM_CH4_2)
print(r_squared_2)
AIC(LMM_CH4_2)
plot(check_collinearity(LMM_CH4_2))

# 
LMM_CH4_4<-lmer(CH4~ShannonB+ShannonF+MBC+CUE+NUE+Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_CH4_4))
summary(LMM_CH4_4)
tab_model(LMM_CH4_4)
anova(LMM_CH4_4,ddf="Kenward-Roger")
r_squared_4 <- r.squaredGLMM(LMM_CH4_4)
print(r_squared_4)
AIC(LMM_CH4_4)
plot(check_collinearity(LMM_CH4_4))

#这个CH4的模型最好
LMM_CH4_3<-lmer(CH4~ShannonF+CUE+Turnover+CUE:Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_CH4_3))
summary(LMM_CH4_3)
tab_model(LMM_CH4_3)
anova(LMM_CH4_3,ddf="Kenward-Roger")
r_squared_3 <- r.squaredGLMM(LMM_CH4_3)
print(r_squared_3)
AIC(LMM_CH4_3)
plot(check_collinearity(LMM_CH4_3))

#MBC的影响过大，可能导致其他因素不显著
LMM_CH4<-lmer(CH4~MBC+ShannonF+CUE+Turnover+CUE:Turnover+NUE:Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_CH4))
summary(LMM_CH4)
tab_model(LMM_CH4)
anova(LMM_CH4,ddf="Kenward-Roger")
r_squared_3 <- r.squaredGLMM(LMM_CH4)
print(r_squared_3)
AIC(LMM_CH4)
plot(check_collinearity(LMM_CH4))

#
LMM_CH4_5<-lmer(CH4~ShannonB+ShannonF+MBC+NUE+CUE*Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_CH4_5))
summary(LMM_CH4_5)
tab_model(LMM_CH4_5)
anova(LMM_CH4_5,ddf="Kenward-Roger")
r_squared_5 <- r.squaredGLMM(LMM_CH4_5)
print(r_squared_5)
AIC(LMM_CH4_5)
plot(check_collinearity(LMM_CH4_5))

#### qCH4的模型####
LMM_qCH4_1<-lmer(log(qCH4_2)~Diversity1+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qCH4_1))
summary(LMM_qCH4_1)
tab_model(LMM_qCH4_1)
anova(LMM_qCH4_1,ddf="Kenward-Roger")
r_squared_q1 <- r.squaredGLMM(LMM_qCH4_1)
print(r_squared_q1)
AIC(LMM_qCH4_1)
# plot(check_collinearity(LMM_qCH4_1))

# 模型共线性
LMM_qCH4_2<-lmer(log(qCH4_2)~ShannonB+ShannonF+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qCH4_2))
summary(LMM_qCH4_2)
tab_model(LMM_qCH4_2)
anova(LMM_qCH4_2,ddf="Kenward-Roger")
r_squared_q2 <- r.squaredGLMM(LMM_qCH4_2)
print(r_squared_q2)
AIC(LMM_qCH4_2)
plot(check_collinearity(LMM_qCH4_2))

#这个模型最好
LMM_qCH4_3<-lmer(log(qCH4_2)~ShannonF+Turnover+turnoverCUE+CUE+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qCH4_3))
summary(LMM_qCH4_3)
tab_model(LMM_qCH4_3)
anova(LMM_qCH4_3,ddf="Kenward-Roger")
r_squared_3 <- r.squaredGLMM(LMM_qCH4_3)
print(r_squared_3)
AIC(LMM_qCH4_3)
plot(check_collinearity(LMM_qCH4_3))

# 考虑了NUE * tunover交互作用
LMM_qCH4_5<-lmer(qCH4~ShannonB+ShannonF+CUE+NUE+Turnover+Turnover*NUE+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qCH4_5))
summary(LMM_qCH4_5)
tab_model(LMM_qCH4_5)
anova(LMM_qCH4_5,ddf="Kenward-Roger")
r_squared_q5 <- r.squaredGLMM(LMM_qCH4_5)
print(r_squared_q5)
AIC(LMM_qCH4_5)
plot(check_collinearity(LMM_qCH4_5))

#
LMM_qCH4_4<-lmer(qCH4~ShannonB+ShannonF+CUE+NUE+Turnover+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qCH4_4)) 
summary(LMM_qCH4_4) 
tab_model(LMM_qCH4_4)
anova(LMM_qCH4_4,ddf="Kenward-Roger")
r_squared_q4 <- r.squaredGLMM(LMM_qCH4_4) 
print(r_squared_q4)  
AIC(LMM_qCH4_4) 
plot(check_collinearity(LMM_qCH4_4))

#
# LMM_qCH4_6<-lmer(qCH4~ShannonB+ShannonF+CUE+NUE*Turnover+(1|Landuse),data=data4)
# shapiro.test(resid(LMM_qCH4_6))
# summary(LMM_qCH4_6)
# tab_model(LMM_qCH4_6)
# anova(LMM_qCH4_6,ddf="Kenward-Roger")
# r_squared_q6 <- r.squaredGLMM(LMM_qCH4_6)
# print(r_squared_q6)
# AIC(LMM_qCH4_6)
# plot(check_collinearity(LMM_qCH4_6))

#
LMM_qCH4_7<-lmer(CUE~ShannonB+ShannonF+NUE+Turnover+Turnover*NUE+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qCH4_7))
summary(LMM_qCH4_7)
tab_model(LMM_qCH4_7)
anova(LMM_qCH4_7,ddf="Kenward-Roger")
r_squared_q7 <- r.squaredGLMM(LMM_qCH4_7)
print(r_squared_q7)
AIC(LMM_qCH4_7)
plot(check_collinearity(LMM_qCH4_7))


# 多个qco2模型比较，LMM_qCO2_5 IS THE BEST MODEL, SO IT WAS USED IN THE FOLLOWING PARTIAL ANALYSIS
AIC(LMM_qCH4_1,LMM_qCH4_2,LMM_qCH4_3,LMM_qCH4_4,LMM_qCH4_5,LMM_qCH4_6) # 模型5是最好的模型
r.squaredGLMM(LMM_qCH4_1) # 模型1的R2相对模型5低很多
r.squaredGLMM(LMM_qCH4_2)
r.squaredGLMM(LMM_qCH4_3)## this model is the best one
r.squaredGLMM(LMM_qCH4_4)
r.squaredGLMM(LMM_qCH4_5)
r.squaredGLMM(LMM_qCH4_6)




#### 分析模型4获得的各个参数与qCH4的关系 ####
##分析turnover * CUE变量对CH4的影响


# 加载包
library(ggpubr)
library(ggplot2)

library(data.table)
head(data4)
LMM_data_1<-data.table(qCH4=c(resid(LMM_qCH4_3)+
                                fixef(LMM_qCH4_3)[1]+
                                (fixef(LMM_qCH4_3)["turnoverCUE"]*LMM_qCH4_3@frame[,"turnoverCUE"])),
                       turnoverCUE=c(LMM_qCH4_3@frame[,"turnoverCUE"]+mean(data4$turnoverCUE)),
                       Landuse= LMM_qCH4_3@frame[,"Landuse"],
                        ShannonF = LMM_qCH4_3@frame[,"ShannonF"])

LMM_data_1[,Pre_qCH4:=exp(qCH4)]

# qCH4 vs turnoverCUE
fig_CH4_turnCUE <- ggscatter(LMM_data_1,
                             x = "turnoverCUE",
                             y = "Pre_qCH4",
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
fig_CH4_turnCUE
ggsave("qCH4_turnoverCUE.png", plot = fig_CH4_turnCUE, device = "png", width = 6, height = 7, dpi = 600, bg = "transparent")




##分析turnover变量对CH4的影响
LMM_data_2<-data.table(qCH4=c(resid(LMM_qCH4_3)+
                                fixef(LMM_qCH4_3)[1]+
                                (fixef(LMM_qCH4_3)["Turnover"]*LMM_qCH4_3@frame[,"Turnover"])),
                       Turnover=c(LMM_qCH4_3@frame[,"Turnover"]+mean(data4$Turnover)),
                       Landuse= LMM_qCH4_3@frame[,"Landuse"],
                       ShannonB= LMM_qN2O_4@frame[,"ShannonB"])
# )
LMM_data_2[,Pre_qCH4:=exp(qCH4)]

# qCH4 vs turnover
fig_CH4_turn <- ggscatter(LMM_data_2,
                          x = "Turnover",
                          y = "Pre_qCH4",
                          # add = "reg.line",
                          #conf.int = TRUE, 
                          #cor.coef = TRUE,
                          size = 5,
                          palette = "uchicago",
                          color = "steelblue",
                          cor.coef.coord = c(-1, 140),
                          cor.coef.size =6)+ 
  #scale_y_continuous(breaks = seq(-600, 100, by = 300), limits = c(-600, 100))+
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
fig_CH4_turn
#ggsave("qCH4_turnover.jpg", plot = fig_CH4_turn, device = "jpeg", width = 6, height = 7, dpi = 300)
ggsave("qCH4_turnover.png", plot = fig_CH4_turn, device = "png", width = 6, height = 7, dpi = 600, bg = "transparent")

##分析CUE变量对CH4的影响
LMM_data_3<-data.table(qCH4=c(resid(LMM_qCH4_3)+
                                fixef(LMM_qCH4_3)[1]+
                                (fixef(LMM_qCH4_3)["CUE"]*LMM_qCH4_3@frame[,"CUE"])),
                       CUE=c(LMM_qCH4_3@frame[,"CUE"]+mean(data4$CUE)),
                       Landuse= LMM_qCH4_3@frame[,"Landuse"])
                       # ShannonB= LMM_qCH4_3@frame[,"ShannonB"])
LMM_data_3[,Pre_qCH4:=exp(qCH4)]

# qCH4 vs CUE
fig_CH4_CUE <- ggscatter(LMM_data_3,
                         x = "CUE",
                         y = "Pre_qCH4",
                         # add = "reg.line",
                         #conf.int = TRUE, 
                         #cor.coef = TRUE,
                         palette = "uchicago",
                         size = 5,
                         color = "steelblue",
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
fig_CH4_CUE
#ggsave("qco2_CUE.jpg", plot = fig_CO2_CUE, device = "jpeg", width = 6, height = 7, dpi = 300)
ggsave("qCH4_CUE.png", plot = fig_CH4_CUE, device = "png", width = 6, height = 7, dpi = 300, bg = "transparent")


##分析ShannonF变量对CH4的影响
LMM_data_4<-data.table(qCH4=c(resid(LMM_qCH4_3)+
                                fixef(LMM_qCH4_3)[1]+
                                (fixef(LMM_qCH4_3)["ShannonF"]*LMM_qCH4_3@frame[,"ShannonF"])),
                       ShannonF=c(LMM_qCH4_3@frame[,"ShannonF"]+mean(data4$ShannonF)),
                       Landuse= LMM_qCH4_3@frame[,"Landuse"])
# Diversity= LMM_qCH4_3@frame[,"Diversity"])
LMM_data_4[,Pre_qCH4:=exp(qCH4)]

# qCH4 vs ShannonF
fig_CH4_ShannonF <- ggscatter(LMM_data_4,
                              x = "ShannonF",
                              y = "Pre_qCH4",
                              # add = "reg.line",
                              #conf.int = TRUE, 
                              #cor.coef = TRUE,
                              palette = "uchicago",
                              size = 5,
                              color = "steelblue",
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
fig_CH4_ShannonF
#ggsave("qco2_NUE.jpg", plot = fig_CO2_NUE, device = "jpeg", width = 6, height = 7, dpi = 300)
ggsave("qCH4_ShannonF.png", plot = fig_CH4_ShannonF, device = "png", width = 6, height = 7, dpi = 300, bg = "transparent")



##分析ShannonB变量对CUE的影响
LMM_data_4<-data.table(CUE=c(resid(LMM_qCH4_7)+
                                fixef(LMM_qCH4_7)[1]+
                                (fixef(LMM_qCH4_7)["ShannonB"]*LMM_qCH4_7@frame[,"ShannonB"])),
                       ShannonB=c(LMM_qCH4_7@frame[,"ShannonB"]+mean(data4$ShannonB)),
                       Landuse= LMM_qCH4_7@frame[,"Landuse"])
# Diversity= LMM_qCH4_7@frame[,"Diversity"])
LMM_data_4[,Pre_CUE:=exp(CUE)]

# CUE vs ShannonB
fig_CUE_ShannonB <- ggscatter(LMM_data_4,
                              x = "ShannonB",
                              y = "Pre_CUE",
                              # add = "reg.line",
                              #conf.int = TRUE, 
                              #cor.coef = TRUE,
                              palette = "uchicago",
                              size = 5,
                              color = "steelblue",
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
fig_CUE_ShannonB
#ggsave("qco2_NUE.jpg", plot = fig_CO2_NUE, device = "jpeg", width = 6, height = 7, dpi = 300)
ggsave("CUE_ShannonB.png", plot = fig_CUE_ShannonB, device = "png", width = 6, height = 7, dpi = 300, bg = "transparent")



library(cowplot)
figure_2 <- plot_grid(fig_N2O_turnNUE, fig_N2O_turn, fig_N2O_GNM, fig_N2O_ShannonB, fig_N2O_ShannonF, ncol = 5,align = "h"  )
print(figure_2)
ggsave("qN2O_vs_pred.jpg", plot = figure_2, width = 17, height = 7,device = "jpeg", dpi = 300)


#### 随机森林，微生物门对qch4进行预测####
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
data_asv=read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv", sep=",", header = T)
names(data_asv)
str(data_asv)
## 分析微生物门对qCH4的影响
set.seed(123)
RF_model_taxa <- randomForest(qCH4~Chloroflexi+
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
RF_model_taxaCH4 <- rfPermute(qCH4~Chloroflexi+
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
print(RF_model_taxaCH4)    
imp.000 <- importance(RF_model_taxaCH4,scale = TRUE)            
imp.000
# 提取rfPermute模型的R²
rfPermute_r2 <- tail(RF_model_taxaCH4$rf$rsq, 1)
cat("rfPermute模型OOB R² =", round(rfPermute_r2, 4), "\n")


#### 线性混合模型分析微生物对 qCH4的影响####




# 导入数据
dir()
data_asv=read.csv("E:/Huangxiaoyi/CH4data/CH4_variable_importance.csv", sep=",", header = T)
names(data_asv)
str(data_asv)

# 
# # 选择数据进行标准化
# data_asv1 <- data_asv %>% dplyr::select("Planctomycetota ",
#                                         "Mortierellomycota",
#                                         "Proteobacteria",
#                                         "Chloroflexi",
#                                         "Rozellomycota",
#                                         "Ascomycota",
#                                         "Chytridiomycota",
#                                         "Myxococcota",
#                                         "Gemmatimonadota",
#                                         "Actinobacteriota",
#                                         "Deinococcota",
#                                         "Verrucomicrobiota",
#                                         "Basidiomycota",
#                                         "Actinobacteriota",
#                                         "Bacteroidota",
#                                         "Name")
# rownames(data_asv1) <- data_asv1$Name
# data_asv1 <- data_asv1 %>% dplyr::select(-"Name")
# data1_z2 <- as.data.frame(scale(data_asv1))
# data1_z2$Name <- rownames(data_asv1)
# str(data1_z2)
# 
# 
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
# 设置工作空间
setwd("E:/黄小轶数据/CH4data")
dir()
data_rf_taxa=read.csv("E:/黄小轶数据/CH4data/CH4_variable_importance.csv", sep=",", header = T)
names(data_rf_taxa)
str(data_rf_taxa)
data_rf_taxa$Taxa <- factor(data_rf_taxa$Taxa, levels = c("Planctomycetota",
                                                          "Mortierellomycota",
                                                          "Proteobacteria",
                                                          "Chloroflexi",
                                                          "Rozellomycota",
                                                          "Ascomycota",
                                                          "Chytridiomycota",
                                                          "Myxococcota",
                                                          "Gemmatimonadota",
                                                          "Actinobacteriota",
                                                          "Deinococcota",
                                                          "Verrucomicrobiota",
                                                          "Basidiomycota",
                                                          "Acidobacteriota",
                                                          "Bacteroidota"
), 
ordered= T)
# 每个微生物随机森林对qCH4影响的图
rf_taxa_qCH4 <- ggbarplot(data_rf_taxa, 
                          x = "Taxa", 
                          y = "X.IncMSE",
                          fill = "#8a508f",
                          orientation = "horiz",
                          palette = "uchicago") +
  scale_y_continuous(position = "right") +  # Y轴在左侧
  # scale_x_discrete(position = "bottom") +
  #scale_y_continuous(expand = c(0, 0))+# 将柱状图水平显示
  theme_bw()+
  theme(
    #panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 18,color = "black"),
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),  # 强制移除 x 轴标题
    axis.title.y = element_blank()  # 强制移除 y 轴标题
  )
rf_taxa_qCH4
ggsave("plotCH4/rf_taxa_qCH4.png", plot = rf_taxa_qCH4, device = "png", width = 5, height = 6, dpi = 600, bg = "transparent")


### 随机森林的partial analysis

#1# Bacteroidota
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
ggsave("plotCH4/fig_Bacteroidota.jpg", plot = Bacteroidota, device = "jpeg", width = 4, height = 3, dpi = 600)

#2# Acidobacteriota
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
ggsave("plotCH4/fig_Acidobacteriota.jpg", plot = Acidobacteriota, device = "jpeg", width = 4, height = 3, dpi = 600)

#3# Basidiomycota
Basidiomycota <- partial(RF_model_taxa, pred.var = "Basidiomycota", plot = TRUE, rug = TRUE,
                       main = "Partial Dependence of medv on rm",
                       xlab = "Average Number of Rooms per Dwelling (rm)",
                       ylab = "Predicted medv")
Basidiomycota

Basidiomycota <- partial(RF_model_taxa, 
                       pred.var = "Basidiomycota", 
                       plot = FALSE)
head(Basidiomycota)

Basidiomycota <- ggplot(Basidiomycota, aes(x = Basidiomycota, y = yhat)) +
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
Basidiomycota
ggsave("plotCH4/fig_Basidiomycota.jpg", plot = Basidiomycota, device = "jpeg", width = 4, height = 3, dpi = 600)

#4# Verrucomicrobiota
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
ggsave("plotCH4/fig_Verrucomicrobiota.jpg", plot = Verrucomicrobiota, device = "jpeg", width = 4, height = 3, dpi = 600)

#5# Deinococcota
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
ggsave("plotCH4/fig_Deinococcota.jpg", plot = Deinococcota, device = "jpeg", width = 4, height = 3, dpi = 600)


#6# Acidobacteriota
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
ggsave("plotCH4/fig_Acidobacteriota.jpg", plot = Acidobacteriota, device = "jpeg", width = 4, height = 3, dpi = 600)


#### 微生物多样性绘图 ####

# 设置工作空间
setwd("E:/黄小轶数据/CH4data")
dir()
# 导入数据 
library(scales)
data_div=read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv", sep=",", header = T)
names(data_div)
str(data_div)

#设置分类变量
data_div$Diversity <- factor(data_div$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data_div$Landuse <- factor(data_div$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data_div)

# ggpubr绘制图  #
head(data_div)
# Bacteria

Bac_diversityCH4 <- ggbarplot(data_div,
                           x = "Diversity",
                           y = "ShannonB",
                           #color = "Diversity",
                           add = "mean_se",
                           palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                           fill = "Diversity",
                           facet.by = "Landuse")+
   scale_y_continuous(limits = c(0, 10.5))+
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
Bac_diversityCH4

ggsave("plotCH4/Bac_diversityCH4.png", 
       plot = Bac_diversityCH4, 
       device = "png", 
       width = 10, 
       height = 4, 
       dpi = 600, 
       bg = "transparent")

# 对每个 Landuse 类型分别进行ANOVA
anova_results <- data_div %>%
  group_by(Landuse) %>%
  anova_test(ShannonB ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)

# Fungi 真菌

Fun_diversityCH4 <- ggbarplot(data_div,
                           x = "Diversity",
                           y = "ShannonF",
                           #color = "Diversity",
                           add = "mean_sd",
                           palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                           fill = "Diversity",
                           facet.by = "Landuse")+
  scale_y_continuous(limits = c(0, 10),labels = number_format(accuracy = 0.1))+
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
Fun_diversityCH4

ggsave("plotCH4/Fun_diversityCH4.png", 
       plot = Fun_diversityCH4, 
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


# 设置工作空间
setwd("E:/Huangxiaoyi/CH4data")
dir()


#### 各变量与梯度关系的图 ####
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(rcartocolor)

# 导入数据 
data1=read.csv("E:/Huangxiaoyi/CH4data/CH4_Dataset.csv", sep=",", header = T)
names(data1)
str(data1)

#设置分类变量
#data1$Plot <- as.factor(data1$Plot)
data1$Diversity <- factor(data1$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data1$Landuse <- factor(data1$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
str(data1)

## ShannonB by Diversity
figShannon_Dilu <- ggboxplot(data1,
                      x = "Diversity",
                      y = "ShannonB",
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
figShannon_Dilu
ggsave("plotCH4/Shannon_Dilu.png", plot = Shannon_Dilu, device = "png", width = 6, height = 6, dpi = 490, bg = "transparent")


## ShannonF by Diversity
figShanF_Diver <- ggboxplot(data1,
                             x = "Diversity",
                             y = "ShannonF",
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
figShanF_Diver
ggsave("plotCH4/figShanF_Diver.png", plot = figShanF_Diver, device = "png", width = 6, height = 6, dpi = 490, bg = "transparent")

# Turnover by Diversity
figTurnov_Diver <- ggboxplot(data1,
                             x = "Diversity",
                             y = "Turnover",
                             combine= TRUE,
                             add = "jitter",
                             fill = "Diversity", 
                             #scales = "free",
                             palette = "uchicago",
                             facet.by = c("Landuse"))+
  #scale_y_continuous(limits = c(-500, 50))+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
figTurnov_Diver
ggsave("figTurnov_Diver.png", plot = figTurnov_Diver, device = "png", width = 6, height = 6, dpi = 300, bg = "transparent")


#####MBC与多样性梯度的关系########
# MBC by Diversity
figMBC_Diver <- ggboxplot(data1,
                             x = "Diversity",
                             y = "MBC",
                             combine= TRUE,
                             add = "jitter",
                             fill = "Diversity", 
                             #scales = "free",
                             palette = "uchicago",
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
ggsave("figMBC_Diver.png", plot = figMBC_Diver, device = "png", width = 6, height = 6, dpi = 300, bg = "transparent")

########评估CH4混合效应模型因素权重关系#######

# 设置工作空间
setwd("E:/黄小轶数据/CH4data")
dir()
library(readxl)
data1 <- read_excel("E:/黄小轶数据/CH4data/CH4混合效应模型结果.xlsx")

library(tidyverse)
library(ggplot2)

# 示例：自定义顺序（替换为你的实际变量名）
data1$variable <- factor(data1$variable, levels = c("TurnoverCUE","Turnover","CUE", "ShannonF"), ordered= T)

figure_value <- ggplot(data1,aes(value,variable,col=subgroup))+
  geom_point(size=6,shape=15)+
  geom_errorbar(aes(xmin=value-sd, xmax=value+sd), size= 1, width=0.02)+
  geom_vline(aes(xintercept=0),linetype=2,col="black")+ 
  scale_x_continuous(limits = c(-0.4,0.5))+
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
ggsave("plotCH4/figure_valueCH4.png", plot = figure_value, device = "png", width = 4, height = 6, dpi = 600, bg = "transparent")

data2 <- read_excel("E:/黄小轶数据/CH4data/CH4_Relative effect of estimates (%).xlsx")

fig_estimates <- ggplot(data = data2,aes(Year, proporation, fill=subgroup))+
  geom_bar(stat="identity", width = 0.7,size=0.5)+# 柱子宽度和边框粗细
  scale_fill_manual(values =c("#730220", "#194a7a" ),) +  # 使用自定义配色
  theme_classic(base_size = 18)+
  scale_y_continuous(name = "Relative effect of estimates (%)")+
  theme(# 将所有线条（包括坐标轴、网格线等）设为黑色
    line = element_line(color = "black"),
    # 将坐标轴文本设为黑色
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x =element_blank())+
  theme(legend.position="none")+
  labs(x=NULL) 
fig_estimates
ggsave("plotCH4/fig_estimatesCH4.png", plot = fig_estimates, device = "png", width =2.2, height = 6, dpi = 600, bg = "transparent")

# 点线图显著性标记
sig_labels <- data.frame(
  variable = levels(data1$variable),
  x_pos = 0.35,
  label = c("", "*", "", "***") # 根据实际结果填写
)

figure_value <- figure_value +
  geom_text(data = sig_labels, aes(x = x_pos, y = variable, label = label), 
            size = 8, color = "red")
figure_value

# 保存图形
ggsave("plotCH4/figure_value_sig.png", figure_value, width = 4, height = 6,dpi = 600, bg = "transparent")



#########热图#####
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
###CH4##DATASET:
data1<-read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv",header=T,na.strings=c("NA"))

names(data1)

# 检查目标列是否为数值型
str(data1[c(10,12:13,15,16,19:22)])

corr<- corr.test(data1[c(10,12:13,15,16,20:22)],
                 data1[c(10,12:13,15,16,20:22)],use = 'pairwise',
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
  qCH4 = r1[, 1],
  MBC = r1[, 2],  # Growth与物种的相关系数
  MBN = r1[, 3],  # Respiration与物种的相关系数
  CUE = r1[, 4],  # Turnover_rate与物种的相关系数
  NUE = r1[, 5],
  Turnover = r1[, 6],
  ShannonB = r1[, 7],
  ShannonF = r1[, 8],
  
  qCH4_p = p1[, 1],
  MBC_p = p1[, 2],  # Growth的p值
  MBN_p = p1[, 3],  # Respiration的p值
  CUE_p = p1[, 4], 
  NUE_p = p1[, 5],
  Turnover_p = p1[, 6],
  ShannonB_p = p1[, 7],
  ShannonF_p = p1[, 8]
  
)
# 设置工作空间
setwd("E:/黄小轶数据/CH4data")
dir()
# 将结果写入到Excel文件
write.xlsx(result, "correlations_CH4.xlsx", rowNames = TRUE)

library(reshape2)
library(ggplot2)

##先画O层的heatmap：
# 读取数据
cor_data <- read.xlsx("correlations_CH4.xlsx",  na.strings = c("NA"))
var_order <- cor_data$Variable  # 保留原始顺序
var_order <- var_order[!duplicated(var_order)]  # 去重并保留原始顺序
# 假设列 2–4 是相关系数，列 5–7 是星号
r_long <- reshape2::melt(cor_data[ ,2:10], id.vars = "Variable", variable.name = "Function", value.name = "r")
p_long <- reshape2::melt(cor_data[, c(2, 11:18)], id.vars = "Variable", variable.name = "Function", value.name = "stars")


# 去掉 Function 名里的 "_p"
p_long$Function <- gsub("_p", "", p_long$Function)

# 合并相关系数和星号
cor_long <- merge(r_long, p_long, by = c("Variable", "Function"))

# 可选：设定因子顺序（根据你原始数据的顺序）
cor_long$Variable <- factor(cor_long$Variable, levels = var_order)
cor_long$Function <- factor(cor_long$Function, levels = c("qCH4","MBC","MBN","CUE","NUE",  "Turnover","ShannonB", "ShannonF"  
))

# 画图
hp_CH4  <- ggplot(cor_long, aes(x = Variable, y = Function, fill = r)) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = stars), size = 5, color = "black") +
  scale_fill_gradient2(low = "#f6c63c", high = "#692f7c", mid = "white", midpoint = 0,limits=c(-1,1),
                       ) +
  theme_minimal() +
  scale_x_discrete(position = 'top') +
  theme(
    text = element_text(size = 18),
    axis.text.x = element_text(angle = 60, hjust = 0.06,size = 18,color = "black"),
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    strip.background = element_blank(),
    plot.margin = unit(c(0.2, 1, 0.2, 0.2), 'cm')
  )

print(hp_CH4 )
ggsave("plotCH4/CH4hp1.jpg", plot = hp_CH4 , device = "jpeg", width = 8, height = 7, dpi = 600)





# 上述OK
######################################################################################################
########################################################################################################




# 加载必要的包
library(ggplot2)
library(ggpubr)  # 用于 ggboxplot（如果不想用，可用纯 ggplot2 替代）
# 导入数据
data1 <- read.csv("E:/Huangxiaoyi/CH4data/CH4_Dataset.csv", sep = ",", header = TRUE)

#data1$Plot <- as.factor(data1$Plot)
data1$Diversity <- factor(data1$Diversity, levels = c("O","D0", "D2","D4","D8"), ordered= T)
data1$Landuse <- factor(data1$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)
# 移除包含 NA 值的行
data1 <- na.omit(data1)  # 移除所有包含 NA 的行
str(data1)
library(ggplot2)
library(scales) # 用于对数变换

# 自定义变换函数（压缩负值对数变换）
signed_log_trans <- function() {
  trans_new(
    "signed_log",
    transform = function(x) sign(x) * log10(abs(x) + 1),
    inverse = function(x) sign(x) * (10^abs(x) - 1)
  )
}

# 绘制图形
ggplot(data1, aes(x = Diversity, y = CH4, fill = Diversity)) +
  geom_boxplot() +
  facet_wrap(~Landuse, scales = "free_x") +
  
  
  
  # 关键步骤：对y轴进行压缩变换
  scale_y_continuous(
    trans = "signed_log",
    breaks = c(-462, -460, -458, -10, 0, 5, 10),
    labels = c("-462","-460","-458",  "-10", "0", "5", "10")
  ) +
  
  # 添加截断标记
  
  labs(y = "CH4 Flux (Log-scaled)") +
  theme_bw()

##############一般线性混合模型
# 读入数据 
data1=read.csv("E:/Huangxiaoyi/CH4数据csv/CH4-Dataset.csv", sep=",", header = T) # 这里的O处理名称变“C”
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
data3 <- data1 %>% dplyr::select("Name","Landuse","Diversity2","Diversity1","Diversity","Plot","CH4","qCH4")
data1_z <- merge(data3,data1_z,by = "Name")
names(data1_z)
str(data1_z)

#创建NUE和turnover的交互项
data1_z$turnoverNUE <- data1_z$Turnover*data1_z$NUE
data1_z$turnoverCUE <- data1_z$Turnover*data1_z$CUE
str(data1_z)

#"Name","Turnover","CUE", "NUE","MBC","MBN","RichnessB", "RichnessF","SOC","GNM","ShannonB","ShannonF"
#CUE,NUE,RS,GNM,GrowthC,GrowthN,MAP,MAT,Soil_pH,Silt,Soil_C_N,Soil_N_P,Soil_DOC_DON,
#DOM_Shannon_index,Bacterial_Shannon_index,Fungal_Shannon_index,
#DOM_PCoA1,Bacterial_PCoA1,Fungal_PCoA1


# ## 模型一：SOC全模型，去除共线性（MAT+MAP+PCoA1_B），最终拟加入的10个变量 ####
model_CH4_Q <- lm(CH4~Turnover*NUE+CUE+NUE+MBN+GNM+MBC+SOC+ShannonB+ShannonF+Turnover*CUE,
                  data=data1_z)
summary(model_CH4_Q)

##共线性检查
plot(check_collinearity(model_CH4_Q))###VIF<10


# ##############################
# # 模型一：N2O全模型，包括所有拟加入的13个变量 ####
# model_CH4_Q <- lm(CH4~Turnover:NUE+CUE+MBC+MBN+SOC+GNM+ShannonB+ShannonF,
#                   data=data1_z)
# summary(model_CH4_Q)
# #共线性检查
# plot(check_collinearity(model_CH4_Q))###VIF<10
# ##发现Diversity1，ShannonB，ShannonF,且三者之间相关性最强.去除Diversity1,R为 0.8068

# 查看模型公式
formula(model_CH4_Q)

# 重新拟合模型（确保包含预测变量）
model_CH4_Q <- lm(CH4 ~ Turnover + NUE + SOC + GNM + MBC + MBN + CUE + ShannonB + ShannonF +Turnover*NUE+Turnover*CUE , data = data1_z)


##最优模型选择
options(na.action = "na.fail")
dre_model_CH4_Q<- dredge(model_CH4_Q, 
                         
                         extra = c("R^2", F = function(x)
                           summary(x)$fstatistic[[1]]))
sub_model_CH4_Q <- subset(dre_model_CH4_Q, delta < 2)
summary(sub_model_CH4_Q)

##提取模型的参数
mean_R2=mean(sub_model_CH4_Q$`R^2`)
mean_AICc=mean(sub_model_CH4_Q$`AICc`)
mean_delta_AICc=mean(sub_model_CH4_Q$`delta`)
mean_R2
mean_AICc
mean_delta_AICc

##模型平均值
aveg_model_CH4_Q<-model.avg(dre_model_CH4_Q, subset = delta < 2)
summary(aveg_model_CH4_Q)
sw(aveg_model_CH4_Q)

library(broom)
broom::tidy(aveg_model_CH4_Q)



# 模型一的系数提取并绘制图 ####
a1=summary(aveg_model_CH4_Q)
a2 <- as.data.frame(a1$coefmat.full)
a3 <- a2[-1,c(1,2)]
a3$variables <- row.names(a3)
names(a3)[1] <- "coefficients"
names(a3)[2] <- "sd"
a3$proporation <- abs(a3$coefficients)/sum(abs(a3$coefficients))*100
a3
a3$sd <- a3$sd*1.96
a3
str(a3)

##导入分组文件
library(tidyverse)
subgroup <- read.csv("E:/Huangxiaoyi/N2O数据csv/250409_N2O_Dateset_v1_Yang.csv", header = T, sep = ",")

##合并
comb_date <- a3 %>% left_join (subgroup,
                               by = c('variables' = 'variables'))
comb_date <- comb_date %>% mutate(Year = "2006")
str(comb_date)

#CUE,NUE,RS,GNM,GrowthC,GrowthN,MAP,MAT,Soil_pH,Silt,Soil_C_N,Soil_N_P,Soil_DOC_DON,
#DOM_Shannon_index,Bacterial_Shannon_index,Fungal_Shannon_index,
#DOM_PCoA1,Bacterial_PCoA1,Fungal_PCoA1

##点图制作
comb_date$variable2 <- factor(comb_date$variable2,
                              levels=c("Turnover","CUE", "NUE","MBC","MBN","RichnessB", "RichnessF","SOC","GNM","ShannonB","ShannonF"))
comb_date %>%
  dplyr::arrange(subgroup,desc(coefficients)) %>%
  dplyr::mutate(variables = factor(variables, levels = rev(variables), ordered = T)) -> comb_date2

#dplyr::mutate(variables = factor(variables, levels = rev(variables), ordered = T)) -> comb_date2
###################

######################


figure3a <- ggplot(comb_date2,aes(coefficients,variables ,col=subgroup))+
  geom_point(size=4,shape=15)+
  geom_errorbar(aes(xmin=coefficients-sd, xmax=coefficients+sd), linewidth= 0.5, width=.01)+
  geom_vline(aes(xintercept=0),linetype=2,col="black")+ 
  scale_color_manual(values = c("Microbial community" = "#5CB0C3", 
                                "Soil properties" = "#F5BE8F")) + 
  scale_x_continuous(limits = c(-1.2,1.2))+
  scale_y_discrete(position = 'right')+
  labs(y=NULL)+
  labs(x="Parameter estimates")+
  theme_classic()+
  theme(legend.position="none",
        axis.line.y.right = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "#000000"),
        axis.title.x = element_text(size = 12))
figure3a
ggsave("GrowthN28-1.pdf", figure3a, width = 7.49, 
       height = 8.01, units = "cm") ## 6.49 x 8.01 
ggsave("GrowthN28-1.png", figure3a, width = 7.49, 
       height = 8.01, units = "cm") ## 6.49 x 8.01 
##分组制图
# comb_date$subgroup <- factor(comb_date$subgroup,levels=c("Climate",
#                                                          "Soil properties",
#                                                          "DOM quality",
#                                                          "Microbial community"))


comb_date2 %>%
  dplyr::group_by(subgroup) %>%
  dplyr::summarise(sum = sum(proporation)) %>%
  dplyr::arrange(desc(sum)) %>%
  dplyr::mutate(cumsum = cumsum(sum)) %>%
  dplyr::mutate(Year = "2006") %>%
  dplyr::mutate(subgroup = str_replace(subgroup, pattern = " ", replacement = "\n"))-> comb_date2.txt


figure3b <- ggplot(data = comb_date2,
                   aes(Year, proporation, fill=subgroup))+
  geom_bar(stat="identity", width = 0.5)+
  geom_text(data = comb_date2.txt, aes(x = Year, y = cumsum - 0.5*sum, label = subgroup)) +
  scale_fill_manual(values = c("Microbial community" = "#5CB0C3", 
                               "Soil properties" = "#F5BE8F")) +  # 多组之后，增加一个颜色即可
  theme_classic()+
  scale_y_continuous(name = "Relative effect of estimates (%)")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x =element_blank())+
  theme(legend.position="none")+
  annotate(geom = "text", label = "Adj. R^2=0.45", x= 1,  y = 102) + 
  labs(x=NULL) 
figure3b
ggsave("GrowthN28-2.pdf", figure3b, width = 3.49, 
       height = 8.01, units = "cm") ## 1.49 x 8.01 
ggsave("GrowthN28-2-2.png", figure3b, width = 3.49, 
       height = 8.01, units = "cm") ## 1.49 x 8.01 

# combine
library(patchwork)

p_combine <- figure3b + figure3a


ggsave("p_combine.pdf", p_combine, width = 7.5,  height = 5)


