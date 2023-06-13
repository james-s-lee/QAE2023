
library(devtools)
library(sf)
library(purrr)
library(dplyr)
library(DT)
library(rgdal)
library(lattice)
library(latticeExtra)
library(lubridate)
library(ggplot2)
library(ggfortify)
library(ggrepel)
library(showtext)   
library(leaflet)
library(leaflet.extras)
library(raster)
library(shiny)
library(shinydashboard)
library(mapview)
library(mapedit)
library(grid)
library(MASS)


require(showtext)
font_add_google(name='Nanum Gothic', regular.wt=400, bold.wt=700)
showtext_auto()
showtext_opts(dpi=112)



#setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
grid <- st_read("./sigun_grid/seoul.shp") 
bnd <- st_read("./sigun_bnd/seoul.shp")  
load("./apt_price.rdata")      
load("./kde_high.rdata")                
load("./kde_hot.rdata")            



pcnt_10 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by = .1))[1]) 
pcnt_90 <- as.numeric(quantile(apt_price$py, probs = seq(.1, .9, by = .1))[9]) 
load("./circle_marker.rdata")                            
circle.colors <- sample(x=c("red","green","blue"), size=1000, replace=TRUE)    



grid <- as(grid, "Spatial") 
grid <- as(grid, "sfc") 
grid <- grid[which(sapply(st_contains(st_sf(grid), apt_price), length)>0)]



ui <- dashboardPage(
  
  dashboardHeader(title = "서울 부동산 대시보드"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      
      menuItem("부동산R114", icon = icon("new-window", lib = "glyphicon"),
               href = "https://www.r114.com/"))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        h3("2022년 6월 ~ 2023년 6월 서울 아파트 매매 실거래 지도"),
        fluidRow(
          
          column(12, selectModUI("selectmap"), div(style = "height:50px")),
        ), 
        
        fluidRow(
          
          column(3, sliderInput("range_time", "건축연도", sep = "", min = 1960, 
                                max = 2023, value = c(1990, 2023)),
                 sliderInput("range_area", "전용면적", sep = "", min = 0, 
                             max = 350, value = c(0, 100)),
                 
          ),
          column(9, DT::dataTableOutput("table"))
          
        ),
        
        fluidRow(
          column(12, h4("거래금액 추세", align = "left"), 
                 plotOutput("regression", height=300)), 
        )
        
        
      )
      
      
    )
  )
)