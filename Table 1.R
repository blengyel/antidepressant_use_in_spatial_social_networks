# This script generates Table 1 for the paper Lengyel et al. (2024) Antidpressant use and spatial social networks. Science Advances

rm(list=ls())

library(foreign)
library(stargazer)
library(sandwich)
library(lmtest)
library(regclass)

library(ggplot2)
library(wooldridge)
library(dplyr)
library(tibble)
library(lemon)
library(ggstance)

library(haven)

# data preparation and IV creation

d=read.table("id_master_v3_agegroups.csv", header=T)

d$b11=0
d$b11[d$antidep_dot2011>0]=1
length(d$b11[d$b11==1])
sd(d$b11)

# We generate the IV variable that cannot be produced from the published data since the town identifiers are droped.
        # d1=read.table("id_master_v3.csv", header=T)
        # nuts4=read_dta("usage_rate_jaras.dta")
        # d1=merge(d1,nuts4, by.x = "cityid1", by.y = "user_citycode", all.x = T, all.y=F)
        # 
        # iv_d=glm(b11 ~ distance_psych, data=d1, family="binomial")
        # summary(iv_d)
        # iv_ur=glm(b11 ~ log(usage_rate), data=d1, family="binomial")
        # summary(iv_ur)
        # iv=glm(b11 ~ log(usage_rate) + distance_psych, data=d1, family="binomial")
        # summary(iv)
        # 
        # stargazer(iv_d,iv_ur, iv, type="latex",
        #           column.labels = "Probability of Antidepressant",
        #           omit="county_id")
        # 
        # d1$iv=predict.glm(iv, newdata=d, type="response")
        # d1=d1[,c("iv")]
        # write.table(d1,"IV.csv")

d1=read.table("IV.csv", header = T)
d=cbind(d,d1)
  d$iv=d$x

# Table 1 

library(mosaic)
d$log_antidep_dot2011=log(d$antidep_dot2011+1)
d$z_log_antidep_dot2011 = zscore(d$log_antidep_dot2011)


f1<- lm(log(antidep_dot2013+1) ~ z_log_antidep_dot2011
        + m1
        + agegroup #z_age
        + z_ln_income_pop
        + z_unemp_pop 
        + as.character(county_id),
        data = d[d$b11==1,])

f2<- lm(log(antidep_dot2013+1) ~ z_log_antidep_dot2011
        + m1
        + agegroup #z_age
        + z_ln_income_pop
        + z_unemp_pop 
        + z_H_norm
        + z_cc_in_ERnorm 
        + z_ln_d + z_n50 
        + as.character(county_id),
        data = d[d$b11==1,])

f3<- lm(log(antidep_dot2013+1) ~ z_log_antidep_dot2011
        + m1
        + agegroup #z_age
        + z_ln_income_pop
        + z_unemp_pop 
        + z_H_norm
        + z_cc_in_ERnorm 
        + z_ln_d + z_n50 
        + iv
        + as.character(county_id),
        data = d[d$b11==1,])

f4<- lm(log(antidep_dot2013+1) ~ z_log_antidep_dot2011
        + z_H_norm*m1
        + z_H_norm*agegroup #z_age
        + z_H_norm*z_ln_income_pop
        + z_cc_in_ERnorm 
        + z_ln_d + z_n50 
        + z_unemp_pop 
        + iv
        + as.character(county_id),
        data = d[d$b11==1,])

f5<- lm(log(antidep_dot2015+1) ~ z_log_antidep_dot2011
        + z_H_norm*m1
        + z_H_norm*agegroup #z_age
        + z_H_norm*z_ln_income_pop
        + z_cc_in_ERnorm 
        + z_ln_d + z_n50 
        + z_unemp_pop 
        + iv
        + as.character(county_id),
        data = d[d$b11==1,])

f6<- lm(log(antidep_dot2015+1) ~ z_log_antidep_dot2011
        + z_H_norm_in20*m1
        + z_H_norm_in20*agegroup #z_age
        + z_H_norm_in20*z_ln_income_pop 
        + cc_in_ERnorm
        + z_unemp_pop 
        + iv
        + z_ln_d + z_n50 
        + as.character(county_id),
        data = d[d$b11==1,])


stargazer(f1, f2, f3, f4, f5, f6, type="text", style="aer", 
          # column.labels = c("Probability of Antidepressant", "Days of Therapy, all obs", "Days of Therapy, "),
          omit="county_id")