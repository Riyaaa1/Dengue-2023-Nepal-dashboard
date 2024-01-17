# Loading essential libraries
setwd("/home/riya/wd")
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(plotly)
library(htmlwidgets)


plot_choropleth <- function(data){
  ggplot(data, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = Total.cases), color = 'black', linewidth = 0.1) +
    coord_fixed(1.3) +
    guides(fill = guide_colorbar(title = "Total cases"))+
    aes(text = paste0(id)) +
    scale_fill_gradient(high="#084594",low="#c6dbef",guide="colorbar") +
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank())
    
}