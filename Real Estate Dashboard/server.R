
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


server <- function(input, output, session){
  
  all = reactive({
    all = subset(apt_price,
                 con_year >= input$range_time[1] & 
                   con_year <= input$range_time[2] & 
                   area >= input$range_area[1] & 
                   area <= input$range_area[2])
    return(all)})
  
  g_sel <- callModule(selectMod, "selectmap",
                      leaflet() %>% 
                        addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>% 
                        addRasterImage(raster_high, 
                                       colors = colorNumeric(c("blue", "green", "yellow","red"), 
                                                             values(raster_high), na.color = "transparent"), 
                                       opacity = 0.4, group = "최고가") %>%
                        addRasterImage(raster_hot, 
                                       colors = colorNumeric(c("blue", "green", "yellow","red"), 
                                                             values(raster_hot), na.color = "transparent"), 
                                       opacity = 0.4, group = "급등지") %>%
                        addLayersControl(baseGroups = c("최고가", "급등지"), 
                                         options = layersControlOptions(collapsed = FALSE)) %>%
                        addPolygons(data=bnd, weight = 3, stroke = T, 
                                    color = "black", fillOpacity = 0) %>%
                        addCircleMarkers(data = apt_price, lng =unlist(map(apt_price$geometry,1)), 
                                         lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE, 
                                         fillOpacity = 0.6, fillColor = circle.colors, weight=apt_price$py, 
                                         popup=~paste0('지번주소: ',juso_jibun,
                                                       '<br> 전용면적: ', area, "m²",
                                                       '<br> 거래금액: ', price, "만원"), 
                                         clusterOptions = markerClusterOptions(iconCreateFunction=JS(avg.formula))) %>% 
                        leafem::addFeatures(st_sf(grid), layerId = ~seq_len(length(grid)), color = 'grey'))
  
  rv <- reactiveValues(intersect=NULL, selectgrid=NULL)
  observe({
    gs <- g_sel() 
    rv$selectgrid <- st_sf(grid[as.numeric(gs[which(gs$selected==TRUE),"id"])])
    if(length(rv$selectgrid) > 0){
      rv$intersect <- st_intersects(rv$selectgrid, all())
      rv$sel       <- st_drop_geometry(apt_price[apt_price[unlist(rv$intersect[1:10]),],])
    } else {
      rv$intersect <- NULL
    }
  })
  
  output$regression <- renderPlot({
    if (nrow(rv$intersect) == 0) 
      return(NULL)
    all <- aggregate(all()$py, by=list(all()$ym),mean)
    sel <- aggregate(rv$sel$py, by=list(rv$sel$ym),mean)
    fit_all <- lm(all$x ~ all$Group.1)   
    fit_sel <- lm(sel$x ~ sel$Group.1)   
    coef_all <- round(summary(fit_all)$coefficients[2], 1) * 365  
    coef_sel <- round(summary(fit_sel)$coefficients[2], 1) * 365 
    grob_1 <- grobTree(textGrob(paste0("서울시 전체 변화량: ",              
                                       coef_all, "만원"), x=0.05,  y=0.84, hjust=0,
                                gp=gpar(col="blue", fontsize=13)))
    grob_2 <- grobTree(textGrob(paste0("선택 지역 변화량: ", 
                                       coef_sel, "만원"), x=0.05,  y=0.95, hjust=0,
                                gp=gpar(col="red", fontsize=16, fontface="bold")))
    gg <- ggplot(sel, aes(x=Group.1, y=x, group=1)) +
      geom_smooth(color= "red",size=1.5, se=F) + xlab("년월")+ ylab("거래금액") +
      theme(axis.text.x=element_text(angle=90)) +
      stat_smooth(method='lm', linetype = "dashed", se=F) +
      theme_bw()
    gg + geom_smooth(data=all, aes(x=Group.1, y=x, group=1, se=F), 
                     color="blue", size=1, se=F) +
      annotation_custom(grob_1) + 
      annotation_custom(grob_2)
  })
  
  output$table <- DT::renderDataTable({
    df <- dplyr::select(rv$sel, ymd, addr_1, apt_nm, con_year, price, area, floor, py) %>% 
      arrange(desc(py))
    colnames(df) <- c("날짜", "주소", "아파트명", "연식", "거래금액", "전용면적", "층", "평당거래가")
    df
  }, extensions = 'Buttons',
  options = list(dom = 'Bfrtip', scrollY = 300, scrollCollapse = T, 
                 paging = TRUE, buttons = c('excel')))
  
  
  
  
}