#R-codes

library(faraway)
library(readxl)
Rplot_data <- read_excel("C:/Users/prabha/Desktop/ISEN 616/Project/for_R_Half_normal_plot.xlsx")
View(Rplot_data)
x = halfnorm(Rplot_data$MainEffect, nlab= 11, labs = as.character(Rplot_data$Run),
             ylab = "Sorted Data",)
library(ISLR)
library(leaps)
library(olsrr)
library(readxl)
project_data  <- read_excel("C:/Users/prabha/Desktop/ISEN 616/Project/prabha_project.xlsx")
View(project_data )

##Dispersion
##step 1
regfit_l=lm(lnssq~l+lw+lL+lW+ld+lF,data=project_data)
ols_step_both_p(regfit_l)
regfit_w=lm(lnssq~w+lw+wL+wW+wd+wF,data=project_data)
ols_step_both_p(regfit_w)
regfit_L=lm(lnssq~L+lL+wL+LW+Ld+LF,data=project_data)
ols_step_both_p(regfit_L)
regfit_W=lm(lnssq~W+lW+wW+LW+Wd+WF,data=project_data)
ols_step_both_p(regfit_W)
regfit_d=lm(lnssq~d+ld+wd+Ld+Wd+dF,data=project_data)
ols_step_both_p(regfit_d)
regfit_F=lm(lnssq~F+lF+wF+LF+WF+dF,data=project_data)
ols_step_both_p(regfit_F)

##step2:
regfit_all=lm(lnssq~l+w+L+W+d+F+WF+dF+wd+wL+lw,data=project_data)
ols_step_both_p(regfit_all)
summary(regfitall)

regfit_all_=lm(lnssq~WF+dF,data=project_data)
ols_step_both_p(regfit_all_)
summary(regfit_all_)

##LOcation effects

library(faraway)
library(readxl)
Rplot_data <- read_excel("C:/Users/prabha/Desktop/ISEN 616/Project/for_R_Half_normal_plot.xlsx")
View(Rplot_data)
x = halfnorm(Rplot_data$Dispersion, nlab= 11, labs = as.character(Rplot_data$Run),
             ylab = "Sorted Data",)
##step 1
regfit_l1=lm(Ybar~l+lw+lL+lW+ld+lF,data=project_data)
ols_step_both_p(regfit_l1)
regfit_w1=lm(Ybar~w+lw+wL+wW+wd+wF,data=project_data)
ols_step_both_p(regfit_w1)
regfit_L1=lm(Ybar~L+lL+wL+LW+Ld+LF,data=project_data)
ols_step_both_p(regfit_L1)
regfit_W1=lm(Ybar~W+lW+wW+LW+Wd+WF,data=project_data)
ols_step_both_p(regfit_W1)
regfit_d1=lm(Ybar~d+ld+wd+Ld+Wd+dF,data=project_data)
ols_step_both_p(regfit_d1)
regfit_F1=lm(Ybar~F+lF+wF+LF+WF+dF,data=project_data)
ols_step_both_p(regfit_F1)

##step2:
regfit_all1=lm(Ybar~l+w+L+W+d+F+lF+wd+lw+lL,data=project_data)
ols_step_both_p(regfit_all1)

##step3:
regfit_all2=lm(Ybar~l+W+lF+lW+lL+lw+ld+wW+LW+Wd+WF,data=project_data)
ols_step_both_p(regfit_all2)

##Step4:
regfit_all3=lm(Ybar~l+W+lW+lL+wW+LW,data=project_data)
ols_step_both_p(regfit_all3)
summary(regfit_all3)

regfit_all3=lm(Ybar~l+w+L+W+d+F+lW,data=project_data)
ols_step_both_p(regfit_all3)
k