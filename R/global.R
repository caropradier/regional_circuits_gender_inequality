library(tidyverse)
library(plotly)
library(ggthemes)
library(scales)
library(wesanderson)
library(ggrepel)
library(viridis)
library(geomtextpath)
library(countrycode)
library(gridExtra)
library(stats)
library(ggbump)
library(GGally)
library(openxlsx)
library(cowplot)
library(ggridges)
library(plotly)
library(patchwork)
library(wesanderson)
library(RColorBrewer)

results_list <-  readRDS("www/results_list.RDS")

#fig1
test_size_fig1 <- 10

#fig3
mycolors = c("Social Sciences" = "#33A02C", 
             "Humanities" = "#B2DF8A", 
             "Medical and Health Sciences" = "#E31A1C", 
             "Multidisciplinary" = "#FDBF6F", 
             "Agricultural Sciences" = "#6A3D9A", 
             "Natural Sciences" = "#1F78B4", 
             "Engineering and Technology" = "#A6CEE3" )


m_w <- median(results_list$plot_3_table$women)
m_l <- median(results_list$plot_3_table$latam)


paleta_1 <- wes_palette("Zissou1")[]
paleta_2 <- wes_palette("Darjeeling1")[]