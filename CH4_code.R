#### CH4 with time ####


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
    theme(axis.text.x = element_text(size = 26),
          axis.text.y = element_text(size = 28),
          axis.title = element_blank(),
          plot.title = element_blank(),   # 移除分面标题
          legend.position = "none",    # 移除图例
          strip.text = element_text(size = 30, face = "bold"),
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

####CH4 by Landuse####

# 读入数据 
data1=read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv", sep=",", header = T) # 这里的O处理名称变“C”
names(data1)
str(data1)

library(ggpubr)
library(scales)     # 用于伪对数变换

library(rstatix)    # 用于参数检验
library(dplyr)      # 用于参数检验

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

## qCH4 by Landuse

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
LMM_qCH4_5<-lmer(CUE~ShannonB+ShannonF+NUE+Turnover+Turnover*NUE+(1|Landuse),data=data4)
shapiro.test(resid(LMM_qCH4_5))
summary(LMM_qCH4_5)
tab_model(LMM_qCH4_5)
anova(LMM_qCH4_5,ddf="Kenward-Roger")
r_squared_q5 <- r.squaredGLMM(LMM_qCH4_5)
print(r_squared_q5)
AIC(LMM_qCH4_5)
plot(check_collinearity(LMM_qCH4_5))


# 多个qco2模型比较，LMM_qCO2_3 IS THE BEST MODEL, SO IT WAS USED IN THE FOLLOWING PARTIAL ANALYSIS
AIC(LMM_qCH4_1,LMM_qCH4_2,LMM_qCH4_3,LMM_qCH4_4,LMM_qCH4_5) # 模型3是最好的模型
r.squaredGLMM(LMM_qCH4_1) # 模型1的R2相对模型5低很多
r.squaredGLMM(LMM_qCH4_2)
r.squaredGLMM(LMM_qCH4_3)## this model is the best one
r.squaredGLMM(LMM_qCH4_4)
r.squaredGLMM(LMM_qCH4_5)

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

#### 随机森林结果影响的绘图 ####
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


#6# Actinobacteriota
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
ggsave("plotCH4/fig_Actinobacteriota.jpg", plot = Actinobacteriota, device = "jpeg", width = 4, height = 3, dpi = 600)


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
  MBC = r1[, 2],  
  MBN = r1[, 3],  
  CUE = r1[, 4],  
  NUE = r1[, 5],
  Turnover = r1[, 6],
  ShannonB = r1[, 7],
  ShannonF = r1[, 8],
  
  qCH4_p = p1[, 1],
  MBC_p = p1[, 2],  
  MBN_p = p1[, 3],  
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

####结构方程模型CH4####
rm(list = ls()) 

# 设置到CH4data文件夹
setwd("E:/黄小轶数据/CH4data/")

# 然后读取文件
data <- read.csv("CH4_Dataset.csv")
dir()
library(readxl)
library(semPlot)
library(ggdag)
library(dplyr)
library(piecewiseSEM)
library(tidyverse)
library(nlme)
library(lme4)
library(brms)
library(ape)
library(caper)
library(car) 

data1=read.csv("E:/黄小轶数据/CH4data/CH4_Dataset.csv", sep=",", header = T) # 这里的O处理名称变“C”
names(data1)
str(data1)

# 选择数据进行标准化
data2 <- data1 %>% dplyr::select("link","ShannonB", "ShannonF","Turnover","GNM","CUE", "NUE")
rownames(data2) <- data2$link
data2 <- data2 %>% dplyr::select(-"link")
data1_z <- as.data.frame(scale(data2))
data1_z$link <- rownames(data1_z)
str(data1_z)

# 具有Landuse的标准化数据库
data3 <- data1 %>% dplyr::select("link","Landuse","Diversity2","Diversity","qCH4")
data4 <- merge(data3,data1_z,by = "link")
names(data4)
str(data4)

## 构建CH4-SEM
## 构建SEM模型
names(data4)
str(data4)
qCH4_sem<-psem(
  lme(qCH4~ShannonF+CUE+Turnover+Turnover:CUE, random = ~ 1 | Landuse,data= data4),
  lme(CUE~Turnover, random = ~ 1 | Landuse,data= data4),
  lme(Turnover~ShannonB+ShannonF, random = ~ 1 | Landuse,data= data4),
  data=data4)

summary(qCH4_sem)
basisSet(qCH4_sem)
dSep(qCH4_sem)#conditioning=T,显示所有自变量
fisherC(qCH4_sem)#
coefs(qCH4_sem)#intercept=T,给出截距
rsquared(qCH4_sem)#默认方法是delta,可改为method="theoretical"则是用的日本人的方法计算R2了
AIC(qCH4_sem)
plot(qCH4_sem,node_attrs = list(shape = "rectangle", color = "Bule",
                                fillcolor = "Red"))

###SEM权重分析图####

# 创建示例数据
data1 <- data.frame(
  variableA = factor(c(   "ShannonB", "ShannonF","CUE","Turnover","TurnCUE"),
                     levels = c( "ShannonB", "ShannonF","CUE","Turnover","TurnCUE" )),
  EffectCH4 = c( 0.10758, -0.90472,-0.35, 0.326 ,-0.26)
)



# 添加填充颜色列 - 直接指定颜色
data1$fill_color <- ifelse(data1$variableA %in% c("ShannonB", "ShannonF"), "#bee2ff", "#b5ea8c")
# 横过来带颜色渐变的条形图
fig_sem <- ggplot(data1, aes(x = variableA, y = EffectCH4, fill = EffectCH4)) +
  geom_bar(stat = "identity", 
           fill = ifelse(data1$variableA %in% c("ShannonB", "ShannonF"), "#bee2ff", "#b5ea8c"),
           color = "black",         # 所有边框都为黑色
           size = 1.5,  # 增加边框粗细以更明显
           width = 0.6) +
  scale_fill_identity() +           # 直接使用fill_color列中的颜色值
  # scale_color_manual(values = c("#43b0f1" = "#43b0f1", "#a4f4a1" = "#a4f4a1"),  # 修正这里：使用正确的颜色值
  #                    labels = c("Microbial diversity", "Microbial physiology traits"),  # 添加标签
  #                    ) +  # 添加图例标题
  # guides(color = guide_legend(override.aes = list(fill = c("#43b0f1", "#a4f4a1")))) +  # 让边框图例中间填满颜色
  labs(x = "Variables", y = "Standardized effect") +
  # coord_flip() +  # 调换坐标轴，让条形竖直排列
  theme_bw() +
  theme(axis.text.x = element_text(size = 30, face = "bold", color = "black"),  # x轴文字加粗黑色
        axis.text.y = element_text(size = 30,  color = "black"),  # y轴文字加粗黑色
        axis.title.x = element_blank(),  # 去掉x轴标题
        axis.title.y = element_text(size = 30, face = "bold", color = "black"),  # y轴标题样式
        axis.ticks = element_line(size = 1.5,  color = "black"),  # 坐标轴刻度黑色
        axis.ticks.length = unit(0.3, "cm"),  # 增加刻度线长度
        axis.line = element_line(size = 1.5, color = "black"),   # 坐标轴线粗细为2
        # legend.key.size = unit(1.5, "cm"),
        # legend.text = element_text(size = 20,  face = "bold", color = "black"),  # 图例文字黑色
        # legend.title = element_text(size = 14, face = "bold", color = "black"),  # 图例标题黑色
        # legend.title = element_blank(),
        # legend.position = "right",
        # plot.background = element_rect(fill = "transparent", color = NA),
        # panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),  # 去掉主要网格线
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 1.5, fill = NA))+  # 面板边框加粗加黑)  # 去掉次要网格线)
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1.5) +
  scale_y_continuous(limits = c(-1, 0.4), breaks = seq(-1, 1, 0.4))

fig_sem

ggsave("E:/黄小轶数据/CH4data/plotCH4/figsem_CH4-1.png", 
       plot = fig_sem, 
       device = "png", 
       width = 13, 
       height = 10, 
       dpi = 600, 
       bg = "transparent")



