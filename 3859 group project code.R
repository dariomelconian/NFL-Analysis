#Importing the data and installing the packages:
data=read.csv(file.choose())
install.packages("Hmisc")
library("Hmisc")
install.packages("kableExtra")
library("kableExtra")
install.packages("olsrr")
library("olsrr")
install.packages("sjPlot")
library("sjPlot")
install.packages("tidyverse")
library("tidyverse")
#Preparing the data:
data=subset(data,X.G>2)
data=data[,1:28]
data=data[complete.cases(data),]
data$TD=(data$less_TD+data$more_TD)/2
#Calculating the correlations:
correlations=rcorr(as.matrix(data.frame(data$TD,data$X.G,data$DB,data$X2ATT,
                                        data$INPK,data$X2SK,data$X2RUN,
                                        data$less_DP,data$less_DB.,data$less_SK,
                                        data$less_ATT,data$less_COM,data$less_COM.,
                                        data$less_INT,data$less_NFL,
                                        data$more_DP,data$more_DB.,data$more_SK,
                                        data$more_ATT,data$less_COM,data$more_COM.,
                                        data$more_INT,data$more_NFL)))
correlation_values=round(as.data.frame(correlations[1]),4)
correlation_values=cbind(Variable = rownames(correlation_values),
                         correlation_values[1])
correlation_values=correlation_values[-c(1,20),]
correlation_values$Variable=c("X.G","DB","X2ATT","INPK","X2SK",
                              "X2RUN","Less_DP","Less_DB_perc",
                              "Less_SK","Less_ATT","Less_COM",
                              "Less_COM_perc","Less_INT","Less_NFL",
                              "More_DP","More_DB_perc","More_SK",
                              "More_ATT","More_COM_perc","More_INT",
                              "More_NFL")
rownames(correlation_values)=1:nrow(correlation_values)
colnames(correlation_values)=c("Variable","Correlation with TD")
#kable_styling(kable(correlation_values[1]), full_width = T)
correlation_pvalues=round(as.data.frame(correlations[3]),4)
correlation_pvalues=cbind(Variable = rownames(correlation_pvalues),
                          correlation_pvalues[1])
correlation_pvalues=correlation_pvalues[-c(1,20),]
correlation_pvalues$Variable=c("X.G","DB","X2ATT","INPK","X2SK",
                               "X2RUN","Less_DP","Less_DB_perc",
                               "Less_SK","Less_ATT","Less_COM",
                               "Less_COM_perc","Less_INT","Less_NFL",
                               "More_DP","More_DB_perc","More_SK",
                               "More_ATT","More_COM_perc","More_INT",
                               "More_NFL")
rownames(correlation_pvalues)=1:nrow(correlation_pvalues)
colnames(correlation_pvalues)=c("Variable","P-value")
cor=merge(correlation_values,correlation_pvalues,by="Variable")
kable_styling(kable(cor, align="c"),full_width = T, font_size = 11)
#Fitting a multiple regression model:
model=lm(TD~X.G+DB+less_DP+less_SK+less_ATT+less_COM+
           less_COM.+less_INT+less_NFL+more_SK+
           more_ATT+more_COM.+more_INT+more_NFL,data=data)
summary(model)
tab_model(model,show.stat=TRUE)
#Stepwise regression:
tab_model(ols_step_forward_p(model)$model,show.stat=TRUE)
model=lm(TD~DB+less_NFL+more_NFL+more_SK+more_COM.+
           less_COM.+more_INT,data=data)
summary(model)
#Residual plot:
ggplot(data,aes(x=fitted(model),y=rstandard(model)))+geom_point(color="#006EA1")+
  xlab("Fitted Values")+
  ylab("Standardized Residuals")+
  ggtitle("Standardized Residuals Versus Fitted Values")
#Scatterplots:
ggplot(data,aes(x=DB,y=TD))+geom_point(color="#006EA1")+
  xlab("DB")+
  ylab("TD")+
  ggtitle("DB Versus TD")
ggplot(data,aes(x=less_NFL,y=TD))+geom_point(color="#006EA1")+
  xlab("Less_NFL")+
  ylab("TD")+
  ggtitle("Less_NFL Versus TD")
#Model with the squared terms:
model=lm(TD~DB+I(DB^2)+less_NFL+I(less_NFL^2)+more_NFL+
           more_SK+more_COM_perc+
           less_COM_perc+more_INT,data=data)
summary(model)
tab_model(model,show.stat=TRUE)
#Residual Plot
ggplot(data,aes(x=fitted(model),y=rstandard(model)))+geom_point(color="#006EA1")+
  xlab("Fitted Values")+
  ylab("Standardized Residuals")+
  ggtitle("Standardized Residuals Versus Fitted Values")
#Removing outliers and influencial observations:
data$resid=resid(model)
sd2=2*sd(resid(model))
data$outs=ifelse(abs(data$resid)>sd2, 1, 0)
data2=data[!data$outs,]
cooksd=cooks.distance(model)
influential=as.numeric(names(cooksd)[(cooksd > (4/nrow(data)))])
data2=data2[-influential,]
#Final model:
model=lm(TD~DB+I(DB^2)+less_NFL+I(less_NFL^2)+more_NFL+
           more_SK+more_COM.+
           less_COM.+more_INT,data=data2)
tab_model(model,show.stat=TRUE)
ggplot(data2,aes(x=fitted(model),y=rstandard(model)))+geom_point(color="#006EA1")+
  xlab("Fitted Values")+
  ylab("Standardized Residuals")+
  ggtitle("Residuals Versus Fitted Values")
ggplot(data2, aes(x=rstandard(model))) + 
  geom_histogram(color="black", fill = "#006EA1") +
  xlab("Standardized Residuals")+
  ylab("Frequancy")+
  ggtitle("Histogram of Standardized Residuals")
ggplot(data2, aes(sample=rstandard(model))) + 
  stat_qq(color="#006EA1") + 
  stat_qq_line(color="#006EA1") +
  xlab("Theoretical Normal Distribution Quantiles")+
  ylab("Standardized Residuals Quantiles")+
  ggtitle("Normal Q-Q Plot of Standardized Residuals")


library(lmtest)
bptest(model)
shapiro.test(resid(model))

plot(resid(model) ~ fitted(model), col = "red", pch = 20, xlab = "Fitted", ylab = "Residual", main = "Fitted vs. Residual")

abline(h = 0, col = "green", lwd = 2)

