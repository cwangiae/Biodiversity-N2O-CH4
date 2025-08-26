
#### N2O with time ####

library(ggplot2)
library(ggthemes)
library(ggpubr)
library(rcartocolor)

# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()

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
  theme(axis.text.x = element_text(size = 26, color = "black"),
        axis.text.y = element_text(size = 28, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 30, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", size = 0.4))+ 
  border(color = "black", size = 1.5)

Time_N2O
ggsave("PlotN2O/N2O_Time.jpg", plot = Time_N2O, device = "jpeg", width = 18, height = 7, dpi = 600)



# 设置工作空间
setwd("E:/黄小轶数据/N2Odata")
dir()
####N2O by Landuse####
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



# 多个qco2模型比较，LMM_qN2O_4 IS THE BEST MODEL, SO IT WAS USED IN THE FOLLOWING PARTIAL ANALYSIS
AIC(LMM_qN2O_1,LMM_qN2O_2,LMM_qN2O_3,LMM_qN2O_4,LMM_qN2O_5,LMM_qN2O_6,LMM_qN2O_7) # 模型4是最好的模型
r.squaredGLMM(LMM_qN2O_1) # 模型1的R2相对模型2低很多
r.squaredGLMM(LMM_qN2O_2)
r.squaredGLMM(LMM_qN2O_3)
r.squaredGLMM(LMM_qN2O_4)## this model is the best one
r.squaredGLMM(LMM_qN2O_5)
r.squaredGLMM(LMM_qN2O_6)
r.squaredGLMM(LMM_qN2O_7)

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

#### 随机森林结果影响的绘图 ####
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

library(ggpubr)
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
  theme(axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
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

####参数检验####
library(rstatix)
library(dplyr)
library(ggpubr)


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
  theme(axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
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


####结构方程模型####

rm(list = ls()) 

setwd("E:/黄小轶数据/N2Odata/")
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

data1=read.csv("E:/黄小轶数据/N2Odata/N2O_Dateset.csv", sep=",", header = T) # 这里的O处理名称变“C”
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
data3 <- data1 %>% dplyr::select("link","Landuse","Diversity2","Diversity","qN2O")
data4 <- merge(data3,data1_z,by = "link")
names(data4)
str(data4)


## 构建N2OSEM
## 构建SEM模型
qN2O_sem<-psem(
  lme(log(qN2O)~ShannonB+ShannonF+Turnover+GNM+NUE:Turnover, random = ~ 1 | Landuse,data= data4),
  lme(GNM~ShannonB, random = ~ 1 | Landuse,data= data4),
  lme(Turnover~ShannonB+ShannonF, random = ~ 1 | Landuse,data= data4),
  lme(NUE~Turnover+GNM, random = ~ 1 | Landuse,data= data4),
  data=data4)

summary(qN2O_sem)
basisSet(qN2O_sem)
dSep(qN2O_sem)#conditioning=T,显示所有自变量
fisherC(qN2O_sem)#
coefs(qN2O_sem)#intercept=T,给出截距
rsquared(qN2O_sem)#默认方法是delta,可改为method="theoretical"则是用的日本人的方法计算R2了
AIC(qN2O_sem)
plot(qN2O_sem,node_attrs = list(shape = "rectangle", color = "Bule",
                                fillcolor = "Green"))


###SEM权重分析图####
# 导入数据 
library(ggplot2)
library(ggthemes)
library(ggpubr)
data1=read.csv("E:/黄小轶数据/N2Odata/sem_N2O.csv", sep=",", header = T)
names(data1)
str(data1)
# 创建示例数据
data1 <- data.frame(
  variableA = factor(c( "ShannonB","ShannonF",  "GNM",  "Turnover","TurnNUE"),
                     levels = c( "ShannonB","ShannonF", "GNM", "Turnover", "TurnNUE")),
  EffectN2O = c( 0.933, -0.6236, -0.32, 0.38,-0.47)
)
# 添加填充颜色列 - 直接指定颜色
data1$fill_color <- ifelse(data1$variableA %in% c("ShannonB", "ShannonF"), "#bee2ff", "#b5ea8c")
# 横过来带颜色渐变的条形图
fig_sem <- ggplot(data1, aes(x = variableA, y = EffectN2O, fill = EffectN2O)) +
  geom_bar(stat = "identity", 
           fill = ifelse(data1$variableA %in% c("ShannonB", "ShannonF"), "#bee2ff", "#b5ea8c"),
           color = "black",         # 所有边框都为黑色
           size = 1.5,  # 增加边框粗细以更明显
           width = 0.6) +
  scale_fill_identity() +           # 直接使用fill_color列中的颜色值
  # scale_fill_gradient2(low = "#bd2323",      # 负值颜色（红色）
  #                      mid = "white",        # 中性颜色
  #                      high = "#256e35",     # 正值颜色（绿色）
  #                      midpoint = 0,         # 中性点
  #                      # name = "Effect Size"
  #                      guide = "none") +     # 去除图例
  # scale_color_manual(values = c("#43b0f1", "#a4f4a1"),  # 简化颜色设置
  #                    guide = "none") +       # 去除边框图例
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
  scale_y_continuous(limits = c(-0.7, 1), breaks = seq(-0.7, 1, 0.2))

fig_sem

ggsave("E:/黄小轶数据/N2Odata/PlotN2O/figsem_N2O-1.png", 
       plot = fig_sem, 
       device = "png", 
       width = 13, 
       height = 10, 
       dpi = 600, 
       bg = "transparent")



