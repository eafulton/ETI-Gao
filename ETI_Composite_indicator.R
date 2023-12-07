# Clean up before you start
rm(list=ls())

# Load libraries
library("tidyverse")
library("reshape2")
library("devtools")
library("MASS")
library("dplyr")
#library("Tool.R")
# Plotting
library("ggplot2")
library("RColorBrewer")
#library("ggbiplot")
library("plot3D")
library("plotly")
# PCA and Clustering
library("factoextra")
library("FactoMineR")
library("corrplot")
library("ape")
library("plot3D")
library("heatmaply")
library("data.table")

dfScoreCase <- c(2.5, 3.7, 4.6, 5.5, 6.4, 7.3, 8.2, 9.1, 10.0)
dfCase <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

colors <- data.frame(val=1:11,col=brewer.pal(11,'RdYlGn'),labels=c("Collapse","Shocks Likely","Shocks Possible","Low Integrity","Med Integrity","Med-High Integrity","High Integrity","Robust Integrity","Close to Pristine","Pristine","Pristine"))

xmin <- 0
xmax <- 6

############ Test 1 #######################
datFile <- "ETI_Composite_indicator.csv"
datScore<- read.csv(datFile, header = T)

# Plot results
plot_indx <- "ETI_indx_continuous_example_cases.png"
ggplot(datScore, aes (x = Fmsyscalar, y = Value)) +
  #geom_point(size = 5, aes(colour=factor(Score), shape=factor(Case))) + 
  geom_jitter(size = 5, aes(colour=factor(Score), shape=factor(Case))) +
  expand_limits(y=0) + expand_limits(y=10) +
  scale_colour_manual(breaks=colors$val, values=colors$col, labels=colors$labels) +
  scale_shape_manual(values = c(17, 16)) +
  xlim(xmin, xmax) +
  #theme_bw() +
  guides(color = guide_legend(reverse=TRUE)) +
  labs(
    #title = "Basic Ecosystem Traits & Health Index (BI)",
    x = "Exploitation scalar - FMMSY multiplier",
    y = "Ecosystem Traits Index (ETI)",
    colour = "ETI rating",
    shape="Case"
  ) + 
  theme(axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=18), 
        axis.title=element_text(size=18,face="bold")) 
ggsave(file=plot_indx)

############ Test 2 #######################

# All systems
datFile <- "ETI_per_system.csv"
datScore<- read.csv(datFile, header = T)

ggplot(datScore, aes (x = Year, y = ETI)) +
  geom_point(size = 3, aes(colour=factor(Score), shape=factor(Place))) + 
  #geom_jitter(size = 3, aes(colour=factor(Score), shape=factor(Place))) +
  expand_limits(y=0) + expand_limits(y=10) +
  scale_colour_manual(breaks=colors$val, values=colors$col, labels=colors$labels) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  guides(color = guide_legend(reverse=TRUE)) +
  labs(
    #title = "Basic Ecosystem Traits & Health Index (BI)",
    x = "Year",
    y = "Ecosystem Traits Index (ETI)",
    colour = "ETI rating",
    shape="Location"
  ) + 
  theme(axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=18), 
        axis.title=element_text(size=18,face="bold")) 

ggplot(datScore, aes (x = Year, y = ETI)) +
  geom_point(size = 3, aes(colour=factor(Score))) + 
  #geom_jitter(size = 3, aes(colour=factor(Score), shape=factor(Place))) +
  expand_limits(y=0) + expand_limits(y=10) +
  scale_colour_manual(breaks=colors$val, values=colors$col, labels=colors$labels) +
  facet_wrap (Place~.) +
  guides(color = guide_legend(reverse=TRUE)) +
  labs(
    #title = "Basic Ecosystem Traits & Health Index (BI)",
    x = "Year",
    y = "Ecosystem Traits Index (ETI)",
    colour = "ETI rating"
  ) + 
  theme(axis.text=element_text(size=10,face="bold"), 
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=12),
        strip.text=element_text(size=12))


############ Test 3 #######################

# All systems
datFile <- "ETI_per_system_Scenarios.csv"
datScore<- read.csv(datFile, header = T)

ggplot(datScore, aes (x = Year, y = ETI)) +
  geom_point(size = 3, aes(colour=factor(Score), shape=factor(Scenario))) + 
  #geom_jitter(size = 3, aes(colour=factor(Score), shape=factor(Scenario))) +
  expand_limits(y=0) + expand_limits(y=10) +
  scale_colour_manual(breaks=colors$val, values=colors$col, labels=colors$labels) +
  facet_wrap (Place~.) +
  guides(color = guide_legend(reverse=TRUE)) +
  labs(
    #title = "Basic Ecosystem Traits & Health Index (BI)",
    x = "Year",
    y = "Ecosystem Traits Index (ETI)",
    colour = "ETI rating"
  ) + 
  theme(axis.text=element_text(size=10,face="bold"), 
        axis.title=element_text(size=14,face="bold"),
        title=element_text(size=14,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=12),
        strip.text=element_text(size=12))

############ Thailand #######################

# All systems
datFile <- "ETI_Thailand.csv"
datScore<- read.csv(datFile, header = T)

ggplot(datScore, aes (x = Year, y = ETI)) +
  geom_point(size = 3, aes(colour=factor(Score))) + 
  expand_limits(y=0) + expand_limits(y=10) +
  scale_colour_manual(breaks=colors$val, values=colors$col, labels=colors$labels) +
  guides(color = guide_legend(reverse=TRUE)) +
  labs(
    #title = "Basic Ecosystem Traits & Health Index (BI)",
    x = "Year",
    y = "Ecosystem Traits Index (ETI)",
    colour = "ETI rating"
  ) + 
  theme(axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=18), 
        axis.title=element_text(size=18,face="bold")) 

