# Plotting Gao Resilience scores

# Clean up
rm(list=ls())

### load/install libraries ###
# Data handling and table reshaping
library("tidyverse")
library("reshape2")
library("devtools")
library("MASS")
library("dplyr")
#library("Tool.R")
# Plotting
library("ggplot2")
library("RColorBrewer")
library(wesanderson)
library(viridis)
library(gridExtra)
library(ggpubr)

############ Test 1 #######################

GaoFile <- "Gao_SE_s.csv"
Gaoindx <- read.csv(GaoFile, header = TRUE)

Gaoindx$Case <- as.factor(Gaoindx$Case)
#GaoF <- filter(Gaoindx, Case == 1)
#GaoF <- filter(Gaoindx, Case == 2)
GaoF <- Gaoindx
x <- seq(0.01, 6.95, by=0.01) 
n <- length(x)
c_coefft <- 0.019
b_coefft <- 0.8931
a_coefft <- 5.3384
H_ref <- 5.32
s_ref <- 6.97
H <- x*x*c_coefft-b_coefft*x+a_coefft
gao_ref_pts <- data.frame(matrix(0, ncol = 2, nrow = n))
colnames(gao_ref_pts)[1:2] <- c("s","H")
gao_ref_pts$s <- x
gao_ref_pts$H <- H

ymin <- 1e-1
xmin <- 1e-1
ymax <- 1e+2
xmax <- 1e+3

ggplot(data = GaoF, aes(x = S, y = H)) +
  geom_point(data = GaoF, aes(size=Flevel, colour = Case, shape = Case)) +
  #scale_color_manual(values=wes_palette(n=3, name="GrandBudapest"))
 #geom_path(size = 0.2) + #so connected up through time (colour based on Year)
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_shape_manual(values=c(17, 18)) +
  scale_color_manual(values=c("darkgoldenrod2", "cadetblue4")) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  theme_bw() +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=12,face="bold"), 
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=16,face="bold"),
        legend.title=element_text(size=14,face="bold"))

############ Test 2 #######################

# All systems

GaoFile <- "Gao_per_system_Hist.csv"
Gaoindx <- read.csv(GaoFile, header = TRUE)

Gaoindx$Year <- as.factor(Gaoindx$Year)
GaoF <- Gaoindx
x <- seq(0.01, 6.95, by=0.01) 
n <- length(x)
c_coefft <- 0.019
b_coefft <- 0.8931
a_coefft <- 5.3384
H_ref <- 5.32
s_ref <- 6.97
H <- x*x*c_coefft-b_coefft*x+a_coefft
gao_ref_pts <- data.frame(matrix(0, ncol = 2, nrow = n))
colnames(gao_ref_pts)[1:2] <- c("s","H")
gao_ref_pts$s <- x
gao_ref_pts$H <- H

ymin <- 1e-1
xmin <- 1e-1
ymax <- 1e+3
xmax <- 1e+3

ggplot(data = GaoF, aes(x = Gao_S, y = Gao_H)) +
  #geom_point(data = GaoF, aes(size=Year), colour = "cadetblue4") +
  geom_point(data = GaoF, aes(color=Year)) +
  #  geom_path(size = 0.2) + #so connected up through time (colour based on Year)
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #theme_bw() +
  facet_wrap (Place~.) +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=10,face="bold"), 
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=12),
        strip.text=element_text(size=12))

############ Test 3 #######################

# All systems

GaoFile <- "Gao_per_system_Scenarios.csv"
Gaoindx <- read.csv(GaoFile, header = TRUE)

Gaoindx$Year <- as.factor(Gaoindx$Year)
GaoF <- Gaoindx
x <- seq(0.01, 6.95, by=0.01) 
n <- length(x)
c_coefft <- 0.019
b_coefft <- 0.8931
a_coefft <- 5.3384
H_ref <- 5.32
s_ref <- 6.97
H <- x*x*c_coefft-b_coefft*x+a_coefft
gao_ref_pts <- data.frame(matrix(0, ncol = 2, nrow = n))
colnames(gao_ref_pts)[1:2] <- c("s","H")
gao_ref_pts$s <- x
gao_ref_pts$H <- H

ymin <- 1e-1
xmin <- 1e-1
ymax <- 1e+3
xmax <- 1e+3

scenarios <- c("climate_change","marine_heat_wave","reduce_F")
places <- c("SE_Aust","East_Bering_Sea","Kerala_India","Nth_Central_Chile")

thisPlace <- places[[1]]
dftmp <- filter(GaoF, Place == thisPlace)
df1 <- filter(dftmp, Scenario == scenarios[[1]])
p1 <- ggplot(data = df1, aes(x = Gao_S, y = Gao_H)) +
  geom_point(aes(color=Year)) +
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "South east Australia", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none", axis.title.x=element_text(color="white"))

df2 <- filter(dftmp, Scenario == scenarios[[2]])
p2 <- ggplot(data = df2, aes(x = Gao_S, y = Gao_H)) +
  geom_point(aes(color=Year)) +
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold",color="white"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none",axis.text.y=element_blank(),
        axis.title.y=element_blank(), axis.title.x=element_text(color="black"))

df3 <- filter(dftmp, Scenario == scenarios[[3]])
p3 <- ggplot(data = df3, aes(x = Gao_S, y = Gao_H)) +
  #geom_point(data = GaoF, aes(size=Year), colour = "cadetblue4") +
  geom_point(aes(color=Year)) +
  #  geom_path(size = 0.2) + #so connected up through time (colour based on Year)
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold",color="white"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none", axis.text.y=element_blank(),
        axis.title.y=element_blank(), axis.title.x=element_text(color="white"))

gt <- arrangeGrob(p1, p2, p3, ncol = 3, nrow = 1)
p <- as_ggplot(gt)                                 # transform to a ggplot
p

thisPlace <- places[[2]]
dftmp <- filter(GaoF, Place == thisPlace)
df1 <- filter(dftmp, Scenario == scenarios[[1]])
p1 <- ggplot(data = df1, aes(x = Gao_S, y = Gao_H)) +
  geom_point(aes(color=Year)) +
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Eastern Bering Sea", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none", axis.title.x=element_text(color="white"))

df2 <- filter(dftmp, Scenario == scenarios[[2]])
p2 <- ggplot(data = df2, aes(x = Gao_S, y = Gao_H)) +
  geom_point(aes(color=Year)) +
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold",color="white"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none",axis.text.y=element_blank(),
        axis.title.y=element_blank(), axis.title.x=element_text(color="black"))

df3 <- filter(dftmp, Scenario == scenarios[[3]])
p3 <- ggplot(data = df3, aes(x = Gao_S, y = Gao_H)) +
  #geom_point(data = GaoF, aes(size=Year), colour = "cadetblue4") +
  geom_point(aes(color=Year)) +
  #  geom_path(size = 0.2) + #so connected up through time (colour based on Year)
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold",color="white"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none", axis.text.y=element_blank(),
        axis.title.y=element_blank(), axis.title.x=element_text(color="white"))

gt <- arrangeGrob(p1, p2, p3, ncol = 3, nrow = 1)
p <- as_ggplot(gt)                                 # transform to a ggplot
p

thisPlace <- places[[3]]
dftmp <- filter(GaoF, Place == thisPlace)
df1 <- filter(dftmp, Scenario == scenarios[[1]])
p1 <- ggplot(data = df1, aes(x = Gao_S, y = Gao_H)) +
  geom_point(aes(color=Year)) +
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Kerala, India", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none", axis.title.x=element_text(color="white"))

df2 <- filter(dftmp, Scenario == scenarios[[2]])
p2 <- ggplot(data = df2, aes(x = Gao_S, y = Gao_H)) +
  geom_point(aes(color=Year)) +
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold",color="white"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none",axis.text.y=element_blank(),
        axis.title.y=element_blank(), axis.title.x=element_text(color="black"))

df3 <- filter(dftmp, Scenario == scenarios[[3]])
p3 <- ggplot(data = df3, aes(x = Gao_S, y = Gao_H)) +
  #geom_point(data = GaoF, aes(size=Year), colour = "cadetblue4") +
  geom_point(aes(color=Year)) +
  #  geom_path(size = 0.2) + #so connected up through time (colour based on Year)
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold",color="white"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none", axis.text.y=element_blank(),
        axis.title.y=element_blank(), axis.title.x=element_text(color="white"))

gt <- arrangeGrob(p1, p2, p3, ncol = 3, nrow = 1)
p <- as_ggplot(gt)                                 # transform to a ggplot
p

thisPlace <- places[[4]]
dftmp <- filter(GaoF, Place == thisPlace)
df1 <- filter(dftmp, Scenario == scenarios[[1]])
p1 <- ggplot(data = df1, aes(x = Gao_S, y = Gao_H)) +
  geom_point(aes(color=Year)) +
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "North central Chile", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none", axis.title.x=element_text(color="white"))

df2 <- filter(dftmp, Scenario == scenarios[[2]])
p2 <- ggplot(data = df2, aes(x = Gao_S, y = Gao_H)) +
  geom_point(aes(color=Year)) +
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold",color="white"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none",axis.text.y=element_blank(),
        axis.title.y=element_blank(), axis.title.x=element_text(color="black"))

df3 <- filter(dftmp, Scenario == scenarios[[3]])
p3 <- ggplot(data = df3, aes(x = Gao_S, y = Gao_H)) +
  #geom_point(data = GaoF, aes(size=Year), colour = "cadetblue4") +
  geom_point(aes(color=Year)) +
  #  geom_path(size = 0.2) + #so connected up through time (colour based on Year)
  geom_line(data = gao_ref_pts, aes(x = s, y = H)) + # refrence line details
  geom_vline(xintercept = s_ref, lty=2) +
  geom_hline(yintercept = H_ref, lty=2) +
  scale_x_log10(limits=c(xmin, xmax)) + scale_y_log10(limits=c(ymin, ymax)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Gao index", x = "Density <s>", y = "Flow Heterogeneity H", color='', size='Fishing Pressure') +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=28,face="bold"),
        title=element_text(size=28,face="bold",color="white"),
        legend.title=element_text(size=24,face="bold"),
        legend.text=element_text(size=24),
        legend.position="none", axis.text.y=element_blank(),
        axis.title.y=element_blank(), axis.title.x=element_text(color="white"))

gt <- arrangeGrob(p1, p2, p3, ncol = 3, nrow = 1)
p <- as_ggplot(gt)                                 # transform to a ggplot
p
