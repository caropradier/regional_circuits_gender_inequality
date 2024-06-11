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


abstract <- "The Latin-American scientific community has achieved significant 
    progress towards gender parity, with nearly equal representation 
    of women and men scientists. Despite this achievement, women continue
    to be underrepresented in scholarly publications originating from the region.
    Throughout the 20th century, Latin America established its academic circuit, 
    focusing on research topics of regional significance. However, in recent years, 
    there has been a shift towards integration into the global academic circuit. 
    Through an analysis of scientific publications, this article explores the relationship 
    between gender inequalities in science and the integration of Latin-American researchers 
    into the regional and global academic circuits between 1993 and 2022. Despite the increase 
    in women's participation in research over the period, gender disparities persist: women are
    more likely to engage in the regional circuit, while men are more active within the global circuit.
    This trend is attributed to a thematic alignment between women's research interests and issues specific
    to Latin America. In turn, regional topics receive less citations, which affects career development.
    Our results also reveal that the mechanisms contributing to gender differences in symbolic capital
    accumulation vary between circuits. Women's work achieves equal or greater recognition compared to 
    men's within the regional circuit, but consistently garners less attention in the global circuit. 
    Our findings suggest that policies aimed at strengthening the regional academic circuit would encourage
    scientists to address locally relevant topics while simultaneously fostering gender equality in science."