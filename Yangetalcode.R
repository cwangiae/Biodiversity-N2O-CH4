#These codes are prepared for the article "Yang, Y.X., Qu, L.R., Yu J., Huang, X.Y., Liu, Y., Qu, F.Y., Wang, J., Yang, T.T., Bai, E., Wang, C. Microbial turnover mediates nonlinear response of soil N2O and CH4 fluxes to diversity loss. Global Change Biology."
#Datasets and code are available in Github

########Model analysis##########

#### LMEM analysis ####
##R packages will be used:
library(lme4)
library(lmerTest)
library(MuMIn)
library(LMERConvenienceFunctions)
library(tidyverse)
library(sjPlot) 
library(readxl)

####r2 function for evaluating models:
r2.mixed<-function(mF){
  mFX<-model.matrix(mF)
  VarF <- var(as.vector(fixef(mF) %*% t(mFX)))
  VarR<-sum(as.numeric(VarCorr(mF))) 
  VarResid<-attr(VarCorr(mF), "sc")^2
  fR2<-VarF/(VarF + VarR + VarResid)
  rfR2<-(VarF + VarR)/(VarF + VarR + VarResid)
  list(fR2=fR2,rfR2=rfR2)
}


####vif function for evaluating models:

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

#DATASETS:
data <- read_excel("YangetalDATA.xlsx", sheet = "DATA")
data_sel<-data%>% dplyr::select("Name","Turnover","CUE", "NUE","ShannonB","ShannonF","GNM")
rownames(data_sel) <- data_sel$Name
data_sel <- data_sel %>% dplyr::select(-"Name")
data_scale <- as.data.frame(scale(data_sel))
data_scale$Name <- rownames(data_scale)
data_sel2 <- data %>% dplyr::select("Name","Landuse","Diversity1","Plot","qCH4_2","qN2O")
dat <- merge(data_sel2,data_scale,by = "Name")
dat$turnoverNUE <- dat$Turnover*dat$NUE
dat$turnoverCUE <- dat$Turnover*dat$CUE

#### CH4 model
#Testing the effect of microbial pysiological traits on CH4 flux:
m1<-lmer(log(qCH4_2)~ShannonB+ShannonF+CUE+NUE+Turnover+turnoverCUE+turnoverNUE+(1|Landuse),data=dat)
summary(m1)
plot(m1)
AIC(m1)
r2.mixed(m1)
vif.mer(m1)

#Removing turnoverNUE  as it is high correlated with turnoverCUE (R2 >0.7):
m2<-lmer(log(qCH4_2)~ShannonB+ShannonF+CUE+NUE+Turnover+turnoverCUE+(1|Landuse),data=dat)
summary(m2) 
plot(m2)
AIC(m2) 
r2.mixed(m2)
vif.mer(m2)

#Removing NUE and ShannonB from the model because of its limited effects on CH4 flux:
m3<-lmer(log(qCH4_2)~ShannonF+Turnover+CUE+turnoverCUE+(1|Landuse),data=dat)
summary(m3) 
plot(m3)
AIC(m3) 
r2.mixed(m3)
vif.mer(m3)

#Removing CUE from the model because of its limited effects on CH4 flux:
m4<-lmer(log(qCH4_2)~ShannonF+Turnover+turnoverCUE+(1|Landuse),data=dat)
summary(m4) 
plot(m4)
AIC(m4) 
r2.mixed(m4)
vif.mer(m4)
#We found conditional R2 of m4 < conditional R2 of m3, so CUE retained in the final model.
##Removing turnoverCUE from the model because of its limited effects on CH4 flux:
m5<-lmer(log(qCH4_2)~ShannonF+Turnover+CUE+(1|Landuse),data=dat)
summary(m5) 
plot(m5)
AIC(m5) 
r2.mixed(m5)
vif.mer(m5)

#We found marginal R2 of m4 < marginal R2 of m3, so turnoverCUE retained in the final model.
#Both CUE and turnoverCUE were retained based on R2 comparisons.
#Overall, m3 was selected as the final model for CH4 flux.

#### N2O model
#Testing the effect of microbial pysiological traits on N2O flux:
Nm1<-lmer(log(qN2O)~ShannonB+ShannonF+CUE+NUE+Turnover+GNM+turnoverNUE+(1|Landuse),data=dat)
summary(Nm1)
plot(Nm1)
AIC(Nm1)
r2.mixed(Nm1)
vif.mer(Nm1)

##Removing NUE from the model because of its limited effects on N2O flux:
Nm2<-lmer(log(qN2O)~ShannonB+ShannonF+CUE+Turnover+GNM+turnoverNUE+(1|Landuse),data=dat)
summary(Nm2)
plot(Nm2)
AIC(Nm2)
r2.mixed(Nm2)
vif.mer(Nm2)

###Removing CUE as it is high correlated with Turnover (R2 > 0.7):
Nm3<-lmer(log(qN2O)~ShannonB+ShannonF+Turnover+GNM+turnoverNUE+(1|Landuse),data=dat)
summary(Nm3)
plot(Nm3)
AIC(Nm3)
r2.mixed(Nm3)
vif.mer(Nm3)

#We found Nm3  failed to converge.
#Overall, Nm1 was selected as the final model for N2O flux.

#### Random forest analysis ####
##R packages will be used:
library(rfPermute)
library(ggplot2)
library(ggpubr)
library(pdp)
library(MASS)
library(A3)
library(caret)
library(readxl)

#DATASETS:
data <- read_excel("YangetalDATA.xlsx", sheet = "DATA")

#### CH4 model

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
                              data = data,
                              importance=TRUE,            
                              ntree=1000,            
                              nrep=100,           
                              num.cores = 1)            
print(RF_model_taxaCH4) 
imp.000 <- importance(RF_model_taxaCH4,scale = TRUE)            
imp.000
rfPermute_r2 <- tail(RF_model_taxaCH4$rf$rsq, 1)
cat("model R² =", round(rfPermute_r2, 4), "\n")

#### N2O model

set.seed(123)     
RF_model_taxaN2O <- rfPermute(qN2O~Chloroflexi+
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
                            data = data,
                            importance=TRUE,            
                            ntree=1000,            
                            nrep=100,           
                            num.cores = 1)            

print(RF_model_taxaN2O)    
imp.000 <- importance(RF_model_taxaN2O,scale = TRUE)            
imp.000
rfPermute_r2 <- tail(RF_model_taxaN2O$rf$rsq, 1)
cat("model R² =", round(rfPermute_r2, 4), "\n")

#### SEM analysis ####
##R packages will be used:
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

#DATASETS:
data <- read_excel("YangetalDATA.xlsx", sheet = "DATA")
data_sel<-data%>% dplyr::select("Name","Turnover","CUE", "NUE","ShannonB","ShannonF","GNM")
rownames(data_sel) <- data_sel$Name
data_sel <- data_sel %>% dplyr::select(-"Name")
data_scale <- as.data.frame(scale(data_sel))
data_scale$Name <- rownames(data_scale)
data_sel2 <- data %>% dplyr::select("Name","Landuse","Diversity1","qCH4","qN2O")
dat <- merge(data_sel2,data_scale,by = "Name")
dat$turnoverNUE <- dat$Turnover*dat$NUE
dat$turnoverCUE <- dat$Turnover*dat$CUE

#### CH4 model
CH4_sem<-psem(
  lme(qCH4~ShannonF+CUE+Turnover+turnoverCUE, random = ~ 1 | Landuse,data= dat),
  lme(CUE~Turnover, random = ~ 1 | Landuse,data= dat),
  lme(Turnover~ShannonB+ShannonF, random = ~ 1 | Landuse,data= dat),
  data=dat)

summary(CH4_sem)
basisSet(CH4_sem)
dSep(CH4_sem)
fisherC(CH4_sem)
coefs(CH4_sem)
rsquared(CH4_sem)#默认方法是delta,可改为method="theoretical"则是用的日本人的方法计算R2了
AIC(CH4_sem)
plot(CH4_sem,node_attrs = list(shape = "rectangle", color = "Bule",
                                fillcolor = "Red"))

#### N2O model
N2O_sem<-psem(
  lme(log(qN2O)~ShannonB+ShannonF+Turnover+GNM+turnoverNUE, random = ~ 1 | Landuse,data= dat),
  lme(GNM~ShannonB, random = ~ 1 | Landuse,data= dat),
  lme(Turnover~ShannonB+ShannonF, random = ~ 1 | Landuse,data= dat),
  lme(NUE~Turnover+GNM, random = ~ 1 | Landuse,data= dat),
  data=dat)

summary(N2O_sem)
basisSet(N2O_sem)
dSep(N2O_sem)
fisherC(N2O_sem)
coefs(N2O_sem)
rsquared(N2O_sem)#默认方法是delta,可改为method="theoretical"则是用的日本人的方法计算R2了
AIC(N2O_sem)
plot(N2O_sem,node_attrs = list(shape = "rectangle", color = "Bule",
                                fillcolor = "Green"))
########Figure plot##########

#### Fig. 1 ####

library(ggplot2)
library(ggpubr)
library(scales) 
library(readxl)

data1 <- read_excel("YangetalDATA.xlsx", sheet = "Fig. 1")

data1$Dilution <- factor(data1$Dilution, 
                                 levels = c("O", "D0", "D2", "D4", "D8"), 
                                 ordered = TRUE)
data1$Landuse <- factor(data1$Landuse, 
                                levels = c("Forest", "Grassland", "Cropland"), 
                                ordered = TRUE)
data1 <- na.omit(data1)

#CH4 Plot
dilution_colors <- c("#fcb735", "#bd2323", "#256e35", "#175b91", "#8a508f")
plot_pseudo_log <- function(data) {
  ggline(data, 
         x = "Time", 
         y = "CH4", 
         add = "mean_se", 
         color = "Dilution",
         facet.by = c("Landuse"),
         palette = dilution_colors,
         size = 1.2,
         numeric.x.axis = TRUE) +
    scale_x_continuous(breaks = c(0,30,60,90,120))+  
    scale_y_continuous(
      trans = pseudo_log_trans(sigma = 0.1, base = 10),
      breaks = c(-500, -100, -10, 0, 10, 100, 200),
      labels = function(x) ifelse(x == 0, "0", x)
    ) +
    theme(axis.text.x = element_text(size = 26),
          axis.text.y = element_text(size = 28),
          axis.title = element_blank(),
          plot.title = element_blank(),   
          legend.position = "none",    
          strip.text = element_text(size = 30, face = "bold"),
          strip.background = element_rect(fill = "grey", color = "black", size = 0.4))+ 
    border(color = "black", size = 1.5) 
  
}

CH4_plot <- plot_pseudo_log(data1)
CH4_plot

#N2O Plot
N2O_plot <- ggline(data1, x = "Time", y = "N2O", 
                   add = "mean_se", 
                   color = "Dilution",
                   facet.by = c("Landuse"),
                   #scales = "free",
                   palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ), 
                   size = 1.2,
                   numeric.x.axis = TRUE)+ 
  scale_x_continuous(breaks = c(0,30,60,90,120))+
  theme(axis.text.x = element_text(size = 26, color = "black"),
        axis.text.y = element_text(size = 28, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 30, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", size = 0.4))+ 
  border(color = "black", size = 1.5)
N2O_plot

#### Fig. 2 ####

library(ggpubr)
library(scales)     
library(rstatix)    
library(dplyr)     
library(readxl)

data <- read_excel("YangetalDATA.xlsx", sheet = "DATA")

data$Diversity <- factor(data$Diversity, levels = c("O", "D0", "D2","D4","D8"), ordered= T)
data$Landuse <- factor(data$Landuse, levels = c("Forest", "Grassland", "Cropland"), ordered= T)


#CH4 Plot
fig2_CH4 <- ggboxplot(data,
                      x = "Diversity",
                      y = "CH4",
                      combine = TRUE,
                      add = "jitter",
                      fill = "Diversity",
                      palette = c("#fcb735","#bd2323","#256e35","#175b91", "#8a508f"),
                      facet.by = c("Landuse")) +
  
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 10), 
    breaks = c(-1000, -100, -10, 0, 10, 100, 1000), 
    labels = function(x) round(x, 1) 
  ) +
  theme(
    axis.text.x = element_text(size = 28),
    axis.text.y = element_text(size = 28),
    axis.title = element_blank(),
    legend.position = "none",    
    strip.text = element_text(size = 30, face = "bold"),
    strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2)
  ) +
  border(color = "black", size = 1.5) +
  labs() 
fig2_CH4

anova_results <- data%>%
  group_by(Landuse) %>%
  anova_test(CH4 ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)


fig2_qCH4 <- ggboxplot(data,
                       x = "Diversity",
                       y = "qCH4",
                       combine = TRUE,
                       add = "jitter",
                       fill = "Diversity",
                       palette = c("#fcb735","#bd2323","#256e35","#175b91", "#8a508f"),
                       facet.by = c("Landuse")) +
  
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 10),
    breaks = c(-1000, -100, -10, 0, 10, 100, 1000), 
    labels = function(x) round(x, 1) 
  ) +
  theme(
    axis.text.x = element_text(size = 28),
    axis.text.y = element_text(size = 28),
    axis.title = element_blank(),
    legend.position = "none",    
    strip.text = element_text(size = 30, face = "bold"),
    strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2)
  ) +
  border(color = "black", size = 1.5) +
  labs() 
fig2_qCH4

anova_results <- data %>%
  group_by(Landuse) %>%
  anova_test(qCH4 ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)

#N2O Plot
fig2_N2O <- ggboxplot(data,
                      x = "Diversity",
                      y = "N2O",
                      combine= TRUE,
                      add = "jitter",
                      fill = "Diversity", 
                      #scales = "free",
                      palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                      facet.by = c("Landuse"))+
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",   
        strip.text = element_text(size = 30, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
fig2_N2O

anova_results <- data %>%
  group_by(Landuse) %>%
  anova_test(N2O ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)

fig2_qN2O <- ggboxplot(data,
                       x = "Diversity",
                       y = "qN2O",
                       combine= TRUE,
                       add = "jitter",
                       fill = "Diversity", 
                       palette = c("#fcb735","#bd2323","#256e35","#175b91",  "#8a508f" ),
                       facet.by = c("Landuse"))+ 
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",    
        strip.text = element_text(size = 30, face = "bold"),
        strip.background = element_rect(fill = "grey", color = "black", linewidth = 0.2))+
  border(color = "black", size = 1.5)
fig2_qN2O

anova_results <- data %>%
  group_by(Landuse) %>%
  anova_test(qN2O ~ Diversity) %>%
  adjust_pvalue(method = "bonferroni")
print(anova_results)

#### Fig. 3 ####
library(readxl)
library(tidyverse)
library(ggplot2)

#CH4 plot

data <- read_excel("YangetalDATA.xlsx", sheet = "Fig. 3_CH4")
data$variable <- factor(data$variable, levels = c("TurnoverCUE","Turnover","CUE", "ShannonF"), ordered= T)

fig3_CH4_parameter <- ggplot(data,aes(value,variable,col=subgroup))+
  geom_point(size=6,shape=15)+
  geom_errorbar(aes(xmin=value-sd, xmax=value+sd), size= 1, width=0.02)+
  geom_vline(aes(xintercept=0),linetype=2,col="black")+ 
  scale_x_continuous(limits = c(-0.4,0.5))+
  scale_y_discrete(position = 'right')+
  scale_color_manual(values = c("#730220", "#194a7a" ),) +  
  labs(y=NULL)+
  labs(x=NULL)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 15,color = "black"),
        axis.text.y = element_text(size = 15,color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, face = "bold"),
        legend.position="none")

fig3_CH4_relative <- ggplot(data = data,aes(Year, proporation, fill=subgroup))+
  geom_bar(stat="identity", width = 0.7,size=0.5)+
  scale_fill_manual(values =c("#730220", "#194a7a" ),) +  
  theme_classic(base_size = 18)+
  scale_y_continuous(name = "Relative effect of estimates (%)")+
  theme(line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x =element_blank())+
  theme(legend.position="none")+
  labs(x=NULL) 
fig3_CH4_parameter
fig3_CH4_relative

#N2O plot

data <- read_excel("YangetalDATA.xlsx", sheet = "Fig. 3_N2O")
data$variable <- factor(data$variable, levels = c("TurnoverNUE","Turnover","CUE", "NUE","GNM","ShannonF","ShannonB"), ordered= T)

fig3_N2O_parameter <- ggplot(data,aes(value,variable,col=subgroup))+
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

fig3_N2O_relative <- ggplot(data = data,aes(Year, proporation, fill=subgroup))+
  geom_bar(stat="identity", width = 0.7,size=0.5)+
  scale_fill_manual(values =c("#730220", "#194a7a" ),) +  
  theme_classic(base_size = 18)+
  scale_y_continuous(name = "Relative effect of estimates (%)")+
  theme(line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x =element_blank())+
  theme(legend.position="none")+
  labs(x=NULL) 
fig3_N2O_parameter
fig3_N2O_relative

#### Fig. 4 ####

library(randomForest)
library(pdp)
data <- read_excel("YangetalDATA.xlsx", sheet = "DATA")

#CH4 plot
set.seed(123)
RF_model_CH4 <- randomForest(qCH4~Chloroflexi+
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
                              data = data,
                              ntree=1000,
                              importance=TRUE,
                              proximity=TRUE)
imp_df <- importance(RF_model_CH4)
data_rf_CH4 <- data.frame(
  Taxa = rownames(imp_df),
  X.IncMSE = imp_df[, "%IncMSE"] 
)
data_rf_CH4$Taxa <- factor(data_rf_CH4$Taxa, levels = c("Planctomycetota",
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

Importance_CH4 <- ggbarplot(data_rf_CH4, 
                          x = "Taxa", 
                          y = "X.IncMSE",
                          fill = "#8a508f",
                          orientation = "horiz",
                          palette = "uchicago") +
  scale_y_continuous(position = "right") + 
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),  
    axis.title.y = element_blank() ) 
Importance_CH4

#1# Bacteroidota
Bacteroidota <- partial(RF_model_CH4, pred.var = "Bacteroidota", plot = TRUE, rug = TRUE,
                        main = "Partial Dependence of medv on rm",
                        xlab = "Average Number of Rooms per Dwelling (rm)",
                        ylab = "Predicted medv")
#2# Acidobacteriota
Acidobacteriota <- partial(RF_model_CH4, pred.var = "Acidobacteriota", plot = TRUE, rug = TRUE,
                           main = "Partial Dependence of medv on rm",
                           xlab = "Average Number of Rooms per Dwelling (rm)",
                           ylab = "Predicted medv")
#3# Basidiomycota
Basidiomycota <- partial(RF_model_CH4, pred.var = "Basidiomycota", plot = TRUE, rug = TRUE,
                         main = "Partial Dependence of medv on rm",
                         xlab = "Average Number of Rooms per Dwelling (rm)",
                         ylab = "Predicted medv")
#4# Verrucomicrobiota
Verrucomicrobiota <- partial(RF_model_CH4, pred.var = "Verrucomicrobiota", plot = TRUE, rug = TRUE,
                             main = "Partial Dependence of medv on rm",
                             xlab = "Average Number of Rooms per Dwelling (rm)",
                             ylab = "Predicted medv")
Verrucomicrobiota
#5# Deinococcota
Deinococcota <- partial(RF_model_CH4, pred.var = "Deinococcota", plot = TRUE, rug = TRUE,
                        main = "Partial Dependence of medv on rm",
                        xlab = "Average Number of Rooms per Dwelling (rm)",
                        ylab = "Predicted medv")
Deinococcota
#6# Actinobacteriota
Actinobacteriota <- partial(RF_model_CH4, pred.var = "Actinobacteriota", plot = TRUE, rug = TRUE,
                            main = "Partial Dependence of medv on rm",
                            xlab = "Average Number of Rooms per Dwelling (rm)",
                            ylab = "Predicted medv")
Actinobacteriota

#N2O plot
set.seed(123)
RF_model_N2O <- randomForest(qN2O~Chloroflexi+
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
                              data = data,
                              ntree=1000,
                              importance=TRUE,
                              proximity=TRUE)
imp_df <- importance(RF_model_N2O)
data_rf_N2O <- data.frame(
  Taxa = rownames(imp_df),
  X.IncMSE = imp_df[, "%IncMSE"] 
)
data_rf_N2O$Taxa <- factor(data_rf_N2O$Taxa, levels = c("Rozellomycota",
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

Importance_N2O <- ggbarplot(data_rf_N2O, 
                          x = "Taxa", 
                          y = "X.IncMSE",
                          fill = "#8a508f",
                          orientation = "horiz",
                          palette = "uchicago") +
  scale_y_continuous(position = "right") +  
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 18,color = "black"),
    axis.text.y = element_text(size = 18,color = "black"),
    axis.title.x = element_blank(),  
    axis.title.y = element_blank()  
  )


Importance_N2O

#1# Mortierellomycota
Mortierellomycota <- partial(RF_model_N2O, pred.var = "Mortierellomycota", plot = TRUE, rug = TRUE,
                             main = "Partial Dependence of medv on rm",
                             xlab = "Average Number of Rooms per Dwelling (rm)",
                             ylab = "Predicted medv")

#2# Proteobacteria
Proteobacteria <- partial(RF_model_N2O, pred.var = "Proteobacteria", plot = TRUE, rug = TRUE,
                          main = "Partial Dependence of medv on rm",
                          xlab = "Average Number of Rooms per Dwelling (rm)",
                          ylab = "Predicted medv")
#3# Acidobacteriota
Acidobacteriota <- partial(RF_model_N2O, pred.var = "Acidobacteriota", plot = TRUE, rug = TRUE,
                           main = "Partial Dependence of medv on rm",
                           xlab = "Average Number of Rooms per Dwelling (rm)",
                           ylab = "Predicted medv")
#4# Planctomycetota
Planctomycetota <- partial(RF_model_N2O, pred.var = "Planctomycetota", plot = TRUE, rug = TRUE,
                           main = "Partial Dependence of medv on rm",
                           xlab = "Average Number of Rooms per Dwelling (rm)",
                           ylab = "Predicted medv")
#5# Actinobacteriota
Actinobacteriota <- partial(RF_model_N2O, pred.var = "Actinobacteriota", plot = TRUE, rug = TRUE,
                            main = "Partial Dependence of medv on rm",
                            xlab = "Average Number of Rooms per Dwelling (rm)",
                            ylab = "Predicted medv")
#6# Verrucomicrobiota
Verrucomicrobiota <- partial(RF_model_N2O, pred.var = "Verrucomicrobiota", plot = TRUE, rug = TRUE,
                             main = "Partial Dependence of medv on rm",
                             xlab = "Average Number of Rooms per Dwelling (rm)",
                             ylab = "Predicted medv")
#7# Deinococcota
Deinococcota <- partial(RF_model_N2O, pred.var = "Deinococcota", plot = TRUE, rug = TRUE,
                        main = "Partial Dependence of medv on rm",
                        xlab = "Average Number of Rooms per Dwelling (rm)",
                        ylab = "Predicted medv")
#8# Bacteroidota
Bacteroidota <- partial(RF_model_N2O, pred.var = "Bacteroidota", plot = TRUE, rug = TRUE,
                        main = "Partial Dependence of medv on rm",
                        xlab = "Average Number of Rooms per Dwelling (rm)",
                        ylab = "Predicted medv")
########End code##########