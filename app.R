rm(list=ls())

library(leaflet)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(data.table)

#GLOBAL
rbPal <- colorRampPalette(c('blue','red'))

rampa_prov<-rbPal(10)
rampa_reg<-rbPal(5)

#READ ESTIMATES AT NATIONAL, REGION AND PROVINCE LEVEL (THESE WILL BE MERGED WITH THE SHAPEFILES)
province_results=readRDS("province_results.rds")
region_results=readRDS("region_results.rds")




#READ ALL RESULTS BY SEX, AGE, AREA AND WEEK
results_to_plot=readRDS("results_to_plot.rds")

results_to_plot$Deaths=as.numeric(results_to_plot$Deaths)

results_to_plot$period_n=1
results_to_plot$period_n[results_to_plot$period=="5Feb-11Feb"]<-2
results_to_plot$period_n[results_to_plot$period=="12Feb-18Feb"]<-3
results_to_plot$period_n[results_to_plot$period=="19Feb-25Feb"]<-4
results_to_plot$period_n[results_to_plot$period=="26Feb-3Mar"]<-5
results_to_plot$period_n[results_to_plot$period=="4Mar-10Mar"]<-6
results_to_plot$period_n[results_to_plot$period=="11Mar-17Mar"]<-7
results_to_plot$period_n[results_to_plot$period=="18Mar-24Mar"]<-8
results_to_plot$period_n[results_to_plot$period=="25Mar-31Mar"]<-9
results_to_plot$period_n[results_to_plot$period=="1Apr-7Apr"]<-10
results_to_plot$period_n[results_to_plot$period=="8Apr-14Apr"]<-11
results_to_plot$period_n[results_to_plot$period=="15Apr-21Apr"]<-12
results_to_plot$period_n[results_to_plot$period=="22Apr-28Apr"]<-13
results_to_plot$period_n[results_to_plot$period=="29Apr-5May"]<-14
results_to_plot$period_n[results_to_plot$period=="6May-12May"]<-15
results_to_plot$period_n[results_to_plot$period=="13May-15May"]<-16


#CALCULATE DAILY DEATHS AND EXCESS MORTALITY BY WEEK

results_to_plot$d_deaths=results_to_plot$Deaths/7
results_to_plot$d_deaths[results_to_plot$period_n==1]=results_to_plot$Deaths[results_to_plot$period_n==1]/4
results_to_plot$d_deaths[results_to_plot$period_n==16]=results_to_plot$Deaths[results_to_plot$period_n==16]/3

results_to_plot$d_excess=results_to_plot$Excess/7
results_to_plot$d_excess[results_to_plot$period_n==1]=results_to_plot$Excess[results_to_plot$period_n==1]/4
results_to_plot$d_excess[results_to_plot$period_n==16]=results_to_plot$Excess[results_to_plot$period_n==16]/3

results_to_plot$d_excess_low=results_to_plot$Excess95eCIlow/7
results_to_plot$d_excess_low[results_to_plot$period_n==1]=results_to_plot$Excess95eCIlow[results_to_plot$period_n==1]/4
results_to_plot$d_excess_low[results_to_plot$period_n==16]=results_to_plot$Excess95eCIlow[results_to_plot$period_n==16]/3

results_to_plot$d_excess_high=results_to_plot$Excess95eCIhigh/7
results_to_plot$d_excess_high[results_to_plot$period_n==1]=results_to_plot$Excess95eCIhigh[results_to_plot$period_n==1]/4
results_to_plot$d_excess_high[results_to_plot$period_n==16]=results_to_plot$Excess95eCIhigh[results_to_plot$period_n==16]/3


##GENERATE ALL THE VALUES FOR DROP-DOWN MENUS
period_opt=unique(results_to_plot$period)

results_to_plot=subset(results_to_plot,period!="15Feb-15May")

sex_opt=unique(results_to_plot$Sex)
age_opt=unique(results_to_plot$Age)
area_opt=c("Italy","Macro areas","Regions","Provinces")
macro_opt=c("North","Central","South","Islands")
region_opt=unique(region_results$Region)
province_opt=unique(province_results$Province)


region_map=readRDS("region_map2.rds")
province_map=readRDS("province_map2.rds")




region_map$id=as.character(1:nrow(region_map@data))
province_map$id=as.character((1:nrow(province_map@data))+21)

#UI
ui <- navbarPage("Covid-19 mortality", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                          # includeScript("gomap.js")
                        ),    
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("myMap", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 60, left = "auto", right = "auto", bottom = "auto",
                                      width = 400, height = "auto",
                                      
                                      h2("Select population"),
                                      
                                      selectInput("agegroup", "Age category", age_opt,selected="All ages"),
                                      selectInput("sex", "Sex", sex_opt, selected = "Total"),
                                      selectInput("period", "Period", period_opt, selected = "15Feb-15May"),

                                      #plotOutput("excess_plot", height = 280),
                                      h2("Select area to map"),
                                      selectInput("mapping", "Map", c("Regions","Provinces"), selected = "Provinces"),
                                      sliderInput(inputId = "opacity_slider", label = "Map opacity", min = 0, max = 1, value = 1),
                                      span(tags$i(h4("Click on the map to get additional\narea specific informations")), style="color:#8B0000")                                      
                                      
                                      
                        ))),
           tabPanel("Time trend",
                    fluidPage(theme = shinytheme("flatly")),
                    tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                          # includeScript("gomap.js")
                        ),
                    column(offset=4,width=8,br(),br(),br(),plotOutput("excess_plot",height="600px")),
                    
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 60, left = "auto", right = "auto", bottom = "auto",
                                      width = "auto", height = "auto",
                                      
                                      h2("Select population"),
                                      
                                      selectInput("agegroup2", "Age category", age_opt,selected="All ages"),
                                      selectInput("sex2", "Sex", sex_opt, selected = "Total"),
                                      selectInput("outcome", "Outcome",c("Daily deaths","Daily excess","Weekly excess (%)"), selected = "Weekly excess (%)"),
                                      selectInput("area", "Area", area_opt, selected = "Italy"),
                                      h6("The following changes according to the Area selected",style="color:#006d2c"),
                                      uiOutput("ui")
                        )))

#SERVER
server=function(input, output, session) {
  

  output$ui <- renderUI({

    
    switch(input$area,
           "Italy" = selectInput("area_to_plot", "Italy","Italy",selected="Italy"),
           "Macro areas" = selectInput("area_to_plot", "Macro area",macro_opt,selected="North"),
           "Regions" = selectInput("area_to_plot", "Regions",region_opt,selected="Piemonte"),
           "Provinces" = selectInput("area_to_plot", "Provinces",province_opt,selected="Torino")
    )
  })
    
 plot_df <- reactive({
   
   req(input$area_to_plot)
    results_to_plot %>% dplyr::filter(Sex==input$sex2 & Age==input$agegroup2 & area_to_select==input$area_to_plot) 
   })
 
 
   
 plot_results=reactive({if(input$outcome=="Daily deaths") return(
     qplot(data=plot_df(), x = period_n, y = d_deaths,col="red3",size=I(4)) + geom_point(size = 4,col="red3") +
       ylab("Daily deaths") + theme_bw() + 
       scale_y_continuous() +
       theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=15),
                               axis.text.y = element_text(angle = 90, hjust = 1, vjust = 0.5,size=15),
                               axis.title=element_text(size=14,face="bold"),
                               legend.position="none") + scale_x_discrete(name="Week",limits=1:16,
                                                                          labels=c("1 - 4 feb",
                                                                                   "5 - 11 feb",
                                                                                   "12 - 18 feb",
                                                                                   "19 - 25 feb",
                                                                                   "26 feb - 3 mar",
                                                                                   "4 - 10 mar",
                                                                                   "11 - 17 mar",
                                                                                   "18 - 24 mar",
                                                                                   "25 - 31 mar",
                                                                                   "1 - 7 apr",
                                                                                   "8 - 14 apr",
                                                                                   "15 - 21 apr",
                                                                                   "22 - 28 apr",
                                                                                   "29 apr - 5 may",
                                                                                   "6 - 12 may",
                                                                                   "13 - 15 may"))) 
   else if(input$outcome=="Daily Excess") return(
     qplot(data=plot_df(), x = period_n, y = d_excess,col="red3",size=I(4)) + geom_errorbar(aes(ymin=plot_df()$d_excess_low, ymax=plot_df()$d_excess_high,col="red3")) +
       ylab("Excess (daily deaths)") + theme_bw() + 
       scale_y_continuous() +
       theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=15),
                               axis.text.y = element_text(angle = 90, hjust = 1, vjust = 0.5,size=15),
                               axis.title=element_text(size=14,face="bold"),
                               legend.position="none") + scale_x_discrete(name="Week",limits=1:16,
                                                                          labels=c("1 - 4 feb",
                                                                                   "5 - 11 feb",
                                                                                   "12 - 18 feb",
                                                                                   "19 - 25 feb",
                                                                                   "26 feb - 3 mar",
                                                                                   "4 - 10 mar",
                                                                                   "11 - 17 mar",
                                                                                   "18 - 24 mar",
                                                                                   "25 - 31 mar",
                                                                                   "1 - 7 apr",
                                                                                   "8 - 14 apr",
                                                                                   "15 - 21 apr",
                                                                                   "22 - 28 apr",
                                                                                   "29 apr - 5 may",
                                                                                   "6 - 12 may",
                                                                                   "13 - 15 may")))
   else return(qplot(data=plot_df(), x = period_n, y = ExcessPer,col="red3",size=I(4)) + geom_errorbar(aes(ymin=plot_df()$ExcessPer95eCIlow, ymax=plot_df()$ExcessPer95eCIhigh,col="red3")) +
                 ylab("Excess (%)") + theme_bw() + 
                 scale_y_continuous() +
                 theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=15),
                                         axis.text.y = element_text(angle = 90, hjust = 1, vjust = 0.5,size=15),
                                         axis.title=element_text(size=14,face="bold"),
                                         legend.position="none") + scale_x_discrete(name="Week",limits=1:16,
                                                                                    labels=c("1 - 4 feb",
                                                                                             "5 - 11 feb",
                                                                                             "12 - 18 feb",
                                                                                             "19 - 25 feb",
                                                                                             "26 feb - 3 mar",
                                                                                             "4 - 10 mar",
                                                                                             "11 - 17 mar",
                                                                                             "18 - 24 mar",
                                                                                             "25 - 31 mar",
                                                                                             "1 - 7 apr",
                                                                                             "8 - 14 apr",
                                                                                             "15 - 21 apr",
                                                                                             "22 - 28 apr",
                                                                                             "29 apr - 5 may",
                                                                                             "6 - 12 may",
                                                                                             "13 - 15 may")))
   
   })
 
   output$excess_plot <- renderPlot({plot_results()
     
   })
   
    select_region <- reactive({
       region_data=region_results %>% dplyr::filter(Sex==input$sex & Age==input$agegroup & period==input$period)
    region_data
       })
    
    select_province <- reactive({
      province_data=province_results %>% dplyr::filter(Sex==input$sex & Age==input$agegroup & period==input$period)
    province_data})
    
    
    province_map2=reactive({merge(province_map,select_province(),by="Province")})
    region_map2= reactive({merge(region_map,select_region(),by="Region")})
    
    
   province_cut=reactive({
     prov_cut=quantile(province_map2()$ExcessPer,seq(0,1,0.1))
     prov_cut}) 
   region_cut=reactive({
     reg_cut=quantile(region_map2()$ExcessPer,seq(0,1,0.2))
     reg_cut}) 
   
   
    
   

    pal_prov <-reactive({
      pal_prov2=colorNumeric(
      rampa_prov,
      domain = province_map2()$Excess_Per
    )
      pal_prov2}) 
    pal_reg <- reactive({pal_reg2=colorNumeric(
      rampa_reg,
      domain = region_map2()$Excess_Per
    )
    pal_reg2})
  
     content_prov <- reactive({paste(
       
       "<br><b>ID</b>:",province_map2()$Province,"</br>",
       "<br><b>Age</b>: ",province_map2()$Age,"</br>",
       "<br><b>Sex</b>: ",province_map2()$Sex,"</br>",
       "<br><b>Period</b>: ",province_map2()$period,"</br>",
       "<br><b>Deaths</b>: ",province_map2()$Deaths,"</br>",
       "<br><b>Excess  (CI 95%)</b>: ",province_map2()$Excess,"(",province_map2()$Excess95eCIlow," - ",province_map2()$Excess95eCIhigh,")","</br>",
       "<br><b>Percentage Excess  (CI 95%)</b>: ",province_map2()$ExcessPer,"(",province_map2()$ExcessPer95eCIlow," - "
       ,province_map2()$ExcessPer95eCIhigh,")","</br>")
     })
     content_reg <- reactive({paste(
       
       "<br><b>ID</b>:",region_map2()$Region,"</br>",
       "<br><b>Age</b>: ",region_map2()$Age,"</br>",
       "<br><b>Sex</b>: ",region_map2()$Sex,"</br>",
       "<br><b>Period</b>: ",region_map2()$period,"</br>",
       "<br><b>Deaths</b>: ",region_map2()$Deaths,"</br>",
       "<br><b>Excess  (CI 95%)</b>: ",region_map2()$Excess,"(",region_map2()$Excess95eCIlow," - ",region_map2()$Excess95eCIhigh,")","</br>",
       "<br><b>Percentage Excess  (CI 95%)</b>: ",region_map2()$ExcessPer,"(",region_map2()$ExcessPer95eCIlow," - "
       ,region_map2()$ExcessPer95eCIhigh,")","</br>")  
     })
    
finalMap <- reactive ({
  if(input$mapping=="Provinces") return(leaflet(province_map2()) %>% addTiles() %>%
                                       addPolygons(data=province_map2(),col=province_map2()$color,weight=2,fillOpacity = input$opacity_slider,layerId=~id,popup=content_prov()) %>% 
                                         addLegend("topright", pal = pal_prov(), values=province_cut(),title = "Excess (%)",opacity = 1))
                                       
  else return (leaflet(region_map2()) %>% addTiles() %>%
                 addPolygons(data=region_map2(),col=region_map2()$color,weight=2,fillOpacity = input$opacity_slider,layerId=~id,popup=content_reg()) %>% 
                 addLegend("topright", pal = pal_reg(), values=region_cut(),title = "Excess (%)",opacity = 1))
                 
                 # setView(lng = 12.5113300, lat=41.8919300, zoom = 8.4))
})
output$myMap = renderLeaflet(finalMap())
  
  }

shinyApp(ui,server)

