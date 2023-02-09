
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(tidyverse)
library(dplyr)
library(gdata)
library(corrplot)
library(readxl)

setwd("~/R/Oxysterol Correlation Analysis") 

#load dataset


Compiled_Sputum_and_Plasma <- read_excel("AP_Ozone_Sterol_Healthy_Asthma_data.xlsx")
View(Compiled_Sputum_and_Plasma)

#subset datasets by sample  type

prePlas <-  Compiled_Sputum_and_Plasma[ which(Compiled_Sputum_and_Plasma$`Time_Point`=='Pre'
                                              & Compiled_Sputum_and_Plasma$`Sample_Type` == 'P'), ]

postSput <-  Compiled_Sputum_and_Plasma[ which(Compiled_Sputum_and_Plasma$`Time_Point`=='Post'
                                               & Compiled_Sputum_and_Plasma$`Sample_Type` == 'S'), ]

#separate endpints by disease status - NAS non-asthmastics and AS is asthmatics

prePlasNAS <-  prePlas[ which(prePlas$`Disease_Status`=='NAS'),]

prePlasAS <-  prePlas[ which(prePlas$`Disease_Status`=='AS'),]

postSputNAS <-  postSput[ which(postSput$`Disease_Status`=='NAS'),]

postSputAS <-  postSput[ which(postSput$`Disease_Status`=='AS'),]


#select subset of analytes - plasma sterols at baseline and sputum endpoints post ozone exposure

#aggregate data
prePlasSter <- prePlas%>%
  select(starts_with("Sterol."))

postSputLungCyt <- postSput%>%
  select(starts_with(c("LungFxn.", "Cytokine.")))

#non-asthmatic data
prePlasSterNAS <- prePlasNAS%>%
  select(starts_with("Sterol."))

postSputLungCytNAS <- postSputNAS%>%
  select(starts_with(c("LungFxn.", "Cytokine.")))

#asthmatic data
prePlasSterAS <- prePlasAS%>%
  select(starts_with("Sterol."))

postSputLungCytAS <- postSputAS%>%
  select(starts_with(c("LungFxn.", "Cytokine.")))


#change class of columns from character to numeric
#aggregate
prePlasSterNum <- sapply(prePlasSter, as.numeric)
postSputLungCytNum <- sapply(postSputLungCyt, as.numeric)

#non-asthmatics
prePlasSterNumNAS <- sapply(prePlasSterNAS, as.numeric)
postSputLungCytNumNAS <- sapply(postSputLungCytNAS, as.numeric)

#asthmatics
prePlasSterNumAS <- sapply(prePlasSterAS, as.numeric)
postSputLungCytNumAS <- sapply(postSputLungCytAS, as.numeric)

#Remove prefix from column names
#aggregate
colnames(prePlasSterNum) <- gsub("Sterol.","" , colnames(prePlasSterNum))
colnames(postSputLungCytNum) <- gsub(c("LungFxn."),"" , colnames(postSputLungCytNum))
colnames(postSputLungCytNum) <- gsub(c("Cytokine."),"" , colnames(postSputLungCytNum))

#non-asthmatics
colnames(prePlasSterNumNAS) <- gsub("Sterol.","" , colnames(prePlasSterNumNAS))
colnames(postSputLungCytNumNAS) <- gsub(c("LungFxn."),"" , colnames(postSputLungCytNumNAS))
colnames(postSputLungCytNumNAS) <- gsub(c("Cytokine."),"" , colnames(postSputLungCytNumNAS))

#asthmatics
colnames(prePlasSterNumAS) <- gsub("Sterol.","" , colnames(prePlasSterNumAS))
colnames(postSputLungCytNumAS) <- gsub(c("LungFxn."),"" , colnames(postSputLungCytNumAS))
colnames(postSputLungCytNumAS) <- gsub(c("Cytokine."),"" , colnames(postSputLungCytNumAS))

#separate endpints by disease status

#Spearman rank correlation

library(psych)
cor.matrix1 <- corr.test(prePlasSterNum, postSputLungCytNum, adjust = "none", method = "spearman")
corrplot(cor.matrix1$r, order="original", tl.col = "black",
         p.mat = cor.matrix1$p, sig.level = 0.05, insig = "blank")

cor.matrix2 <- corr.test(prePlasSterNumNAS, postSputLungCytNumNAS, adjust = "none", method = "spearman")
corrplot(cor.matrix2$r, order="original", tl.col = "black",
         p.mat = cor.matrix2$p, sig.level = 0.05, insig = "blank")

cor.matrix3 <- corr.test(prePlasSterNumAS, postSputLungCytNumAS, adjust = "none", method = "spearman")
corrplot(cor.matrix3$r, order="original", tl.col = "black",
         p.mat = cor.matrix3$p, sig.level = 0.05, insig = "blank")
         
         
