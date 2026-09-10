setwd("~/The University of Manchester Dropbox/Norman van Rhijn/WT:Araf Ipflufenoquin/Competitive fitness/Analysis after third run")

library(reshape2)
library(tidyverse)
library(pals)
library(vegan)
library(readxl)

RPMI <- read_excel("Norm_data_R.xlsx", sheet = "RPMI")
AMM <- read_excel("Norm_data_R.xlsx", sheet = "AMM")
ClinvsEnv <- read_excel("Norm_data_R.xlsx", sheet = "ClinvsEnv")
MAT <- read_excel("Norm_data_R.xlsx", sheet = "MAT")

Invivo <- read_excel("Norm_data_R.xlsx", sheet = "Invivo")
IACPA <- read_excel("Norm_data_R.xlsx", sheet = "IACPA")

#RPMI
RPMI.summary <- RPMI %>%
  group_by(`Cyp51a genotype`) %>%
  summarise(
    sd = sd(`Relative fitness`, na.rm = TRUE),
    se=sd(`Relative fitness`)/sqrt(n()),
    `Relative fitness` = mean(`Relative fitness`)
  )

ggplot(RPMI, aes(x=`Cyp51a genotype`, y=`Relative fitness`))  +
  geom_jitter(shape=21, size=2, fill="lightgrey", position = position_jitter(0.2)) +
  geom_errorbar(aes(ymin=`Relative fitness`-sd, ymax=`Relative fitness`+sd), width=.2,
                position=position_dodge(0.05), data=RPMI.summary) +
  geom_point(shape=21, size = 3, data=RPMI.summary, fill="black") +
  theme_bw() +
  theme(legend.position = "", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
#AMM
AMM.summary <- AMM %>%
  group_by(`Cyp51a genotype`) %>%
  summarise(
    sd = sd(`Relative fitness`, na.rm = TRUE),
    se=sd(`Relative fitness`)/sqrt(n()),
    `Relative fitness` = mean(`Relative fitness`)
  )

ggplot(AMM, aes(x=`Cyp51a genotype`, y=`Relative fitness`))  +
  geom_jitter(shape=21, size=2, fill="lightgrey", position = position_jitter(0.2)) +
  geom_errorbar(aes(ymin=`Relative fitness`-sd, ymax=`Relative fitness`+sd), width=.2,
                position=position_dodge(0.05), data=AMM.summary) +
  geom_point(shape=21, size = 3, data=AMM.summary, fill="black") +
  theme_bw() +
  theme(legend.position = "", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Clinical Environemtn
ClinEnv.summary <- ClinvsEnv %>%
  group_by(`Source`) %>%
  summarise(
    sd = sd(`Relative fitness`, na.rm = TRUE),
    se=sd(`Relative fitness`)/sqrt(n()),
    `Relative fitness` = mean(`Relative fitness`)
  )

ggplot(ClinvsEnv, aes(x=`Source`, y=`Relative fitness`))  +
  geom_jitter(shape=21, size=2, fill="lightgrey", position = position_jitter(0.2)) +
  geom_errorbar(aes(ymin=`Relative fitness`-sd, ymax=`Relative fitness`+sd), width=.2,
                position=position_dodge(0.05), data=ClinEnv.summary) +
  geom_point(shape=21, size = 3, data=ClinEnv.summary, fill="black") +
  theme_bw() +
  theme(legend.position = "", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#MAT
MAT.summary <- MAT %>%
  group_by(`Mating type`) %>%
  summarise(
    sd = sd(`Relative fitness`, na.rm = TRUE),
    se=sd(`Relative fitness`)/sqrt(n()),
    `Relative fitness` = mean(`Relative fitness`)
  )

ggplot(MAT, aes(x=`Mating type`, y=`Relative fitness`))  +
  geom_jitter(shape=21, size=2, fill="lightgrey", position = position_jitter(0.2)) +
  geom_errorbar(aes(ymin=`Relative fitness`-sd, ymax=`Relative fitness`+sd), width=.2,
                position=position_dodge(0.05), data=MAT.summary) +
  geom_point(shape=21, size = 3, data=MAT.summary, fill="black") +
  theme_bw() +
  theme(legend.position = "", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Invivo
Invivo.summary <- Invivo %>%
  group_by(`Azole susceptibility`) %>%
  summarise(
    sd = sd(`Relative fitness`, na.rm = TRUE),
    se=sd(`Relative fitness`)/sqrt(n()),
    `Relative fitness` = mean(`Relative fitness`)
  )

ggplot(Invivo, aes(x=`Azole susceptibility`, y=`Relative fitness`))  +
  geom_jitter(shape=21, size=2, fill="lightgrey", position = position_jitter(0.2)) +
  geom_errorbar(aes(ymin=`Relative fitness`-sd, ymax=`Relative fitness`+sd), width=.2,
                position=position_dodge(0.05), data=Invivo.summary) +
  geom_point(shape=21, size = 3, data=Invivo.summary, fill="black") +
  theme_bw() +
  theme(legend.position = "", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#IA vs CPA
IACPA.summary <- IACPA %>%
  group_by(`Clinical Source`) %>%
  summarise(
    sd = sd(`Relative fitness`, na.rm = TRUE),
    se=sd(`Relative fitness`)/sqrt(n()),
    `Relative fitness` = mean(`Relative fitness`)
  )

ggplot(IACPA, aes(x=`Clinical Source`, y=`Relative fitness`))  +
  geom_jitter(shape=21, size=2, fill="lightgrey", position = position_jitter(0.2)) +
  geom_errorbar(aes(ymin=`Relative fitness`-sd, ymax=`Relative fitness`+sd), width=.2,
                position=position_dodge(0.05), data=IACPA.summary) +
  geom_point(shape=21, size = 3, data=IACPA.summary, fill="black") +
  theme_bw() +
  theme(legend.position = "", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
