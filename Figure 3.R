# This script generates Figure 3 for the paper Lengyel et al. (2024) Antidpressant use and spatial social networks. Science Advances


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


# Fig 3A. Coefficient plot - containing estimates from the standardized 1 model

d=read.table("id_master_v3_agegroups.csv", header = T)

m_logit<- glm(b11 ~ z_H_norm + z_cc_in_ERnorm + z_ln_d 
                + z_n50 + m1 
                + agegroup # z_age 
                + z_ln_income_pop 
                + z_unemp_pop 
                + as.character(county_id), 
                data = d, family="binomial")
  summary(m_logit)

m_logit_rob1<- glm(b1 ~ z_H_norm + z_cc_in_ERnorm + z_ln_d 
              + z_n50 + m1 
              + agegroup # z_age 
              + z_ln_income_pop 
              + z_unemp_pop 
              + b_max
              + as.character(county_id), 
              data = d, family="binomial")
summary(m_logit_rob1)

m_logit_rob2<- glm(b1 ~ z_H_norm + z_cc_in_ERnorm + z_ln_d 
                + z_n50 + m1 
                + agegroup # z_age 
                + z_ln_income_pop 
                + z_unemp_pop 
                + distance_psych
                + as.character(county_id), 
                data = d, family="binomial")
summary(m_logit_rob2)


# Heteroskedasticity-robust SE

#t=coeftest(m_logit, vcov = vcovHC(m_logit, "HC1"))
t=summary(m_logit)$coefficients
var1_m <- data.frame(Variable = "Spatial Diversity",
                     Coefficient = t[2,1],
                     SE = t[2,2])
var2_m <- data.frame(Variable = "Local Cohesion",
                      Coefficient = t[3,1],
                      SE = t[3,2])
var3_m <- data.frame(Variable = "Degree",
                      Coefficient = t[4,1],
                      SE = t[4,2])
var4_m <- data.frame(Variable = "Share of friends within 50km",
                      Coefficient = t[5,1],
                      SE = t[5,2])

allModelFrame <- data.frame(rbind(
  var1_m, var2_m, var3_m, var4_m))


allModelFrame$col = "grey"
allModelFrame$col[1:4] = "darkblue"
allModelFrame$fcol <- factor(allModelFrame$col)
allModelFrame$ypos=c(15, 12, 9, 6)
allModelFrame$shape=16

  
cairo_pdf("Fig3A_coeff_plot.pdf", width=8, height=12)
ggplot(allModelFrame) + 
  geom_vline(xintercept = 0, linetype="solid", size=4, colour="gray80") +
  geom_hline(yintercept = 7, linetype="dotted", size=4, colour="gray80") +
  geom_hline(yintercept = 10, linetype="dotted", size=4, colour="gray80") +
  geom_hline(yintercept = 13, linetype="dotted", size=4, colour="gray80") +
  # geom_hline(yintercept = 16, linetype="dotted", size=4, colour="gray80") +
  # geom_hline(yintercept = 19, linetype="dotted", size=4, colour="gray80") +
  # geom_hline(yintercept = 22, linetype="dotted", size=4, colour="gray80") +
  # geom_hline(yintercept = 25, linetype="dotted", size=4, colour="gray80") +
  geom_point(aes(x = Coefficient, y = ypos), shape = allModelFrame$shape, 
             show.legend = FALSE, size=12, colour=allModelFrame$fcol) +
  geom_linerangeh(aes(xmin = Coefficient - 1.96*SE, y = ypos, 
                      xmax = Coefficient + 1.96*SE), show.legend = FALSE,
                  lwd = 3,  colour=allModelFrame$fcol) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size=50)) + 
  theme(axis.text.x =element_text(size=30)) +
  theme(axis.text.y =element_text(size=30)) +
  theme(axis.title.x =element_text(size=30)) +
  labs(x="Coefficients of\nAntidepdessant Use", y="") +
  scale_y_continuous(labels=c("Spatial Diversity",
                              "Local Cohesion",
                              "Degree",
                              "Share of Friends\nwithin 50km"),
                     # "Male",
                     # "Age",
                     # "Average income in hometown",
                     # "Unemplyment rate in hometown"), 
                     breaks=c(14.5, 11.5, 8.5, 5.5))
dev.off()


# Fig 3B. Interplot
#install.packages("haven")
library(haven)

d=read.table("id_master_v3_agegroups.csv", header=T)

library(interplot)

m<- lm(b1 ~ z_H_norm * z_cc_in_ERnorm + z_ln_d 
              + z_n50 + m1 
              + agegroup #z_age 
              + z_ln_income_pop 
              + z_unemp_pop 
              + as.character(county_id), 
              data = d)
summary(m)

cairo_pdf("Fig3B_interplot.pdf", width=12, height=12)
  interplot(m = m, var1 = "z_cc_in_ERnorm", var2 = "z_H_norm", hist=FALSE, point =T, 
            steps = 30, 
            esize = 2, 
            ercolor = "black")+
    # Change the background
    # theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    # Add a horizontal line at y = 0
    geom_hline(yintercept = 0, linetype = "dashed") +
    # Add labels for X and Y axes
    xlab(expression(italic(H[i]))) +
    ylab(expression(paste('Coefficient of ', italic(C[i]), ' on ', italic(A[i]),' by levels of ', italic(H[i]))))+
    theme(axis.title=element_text(size=60),text = element_text(size=60))+
    theme(plot.title = element_text(face="bold")) + 
    scale_x_continuous(breaks=c(-2,0,2,4))
dev.off()
