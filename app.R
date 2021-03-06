rm(list=ls())

library(leaflet)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(lubridate)
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
        sliderInput(inputId = "opacity_slider", label = "Map opacity", min = 0, max = 1, value = 0.5),
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
      selectInput("outcome", "Outcome",c("Daily deaths","Daily excess","Daily excess (%)"), selected = "Daily excess (%)"),
      selectInput("area", "Area", area_opt, selected = "Italy"),
      h6("The following changes according to the Area selected",style="color:#006d2c"),
      uiOutput("ui")
    )),
  tabPanel("About the app",
    tags$div(tags$br(),
      "This app reports interactively all the results from the study \"Excess mortality during the COVID-19 outbreak in Italy: a two-stage interrupted time series analysis\".",
      tags$br(),
      span(tags$i(h4("The manuscript is still under submission and not available online")), style="color:#8B0000"),
      tags$br(),
      tags$h4("Code"),
      "Code and input data used to generate the analysis are available at this ",tags$a(href="https://github.com/gasparrini/ItalyCOVIDdeath", "link."),tags$br(),
      "Code and input data used to generate this app are available at this ",tags$a(href="https://github.com/johnnyfreak82/Covid-19-italy", "link"),
      tags$br(),
      tags$h4("Sources"),
      tags$b("Mortality data: "), tags$a(href="https://www.istat.it/it/archivio/240401", "ISTAT"),tags$br(),
      tags$b("Temperature data: "), tags$a(href="https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview", "COPERNICUS ERA5 REANALYSIS"),tags$br(),
      tags$b("Influenza activity data: "), tags$a(href="https://old.iss.it/site/RMI/influnet/pagine/rapportoInflunet.aspx", "INFLUNET PROJECT"),
      tags$br(),
      tags$h4("Authors"),
      "Matteo Scortichini, Department of Epidemiology, Lazio Regional Health Service",
      tags$br(),
      "Antonio Gasparrini, London School of Hygiene & Tropical Medicine",
      tags$br(),
      tags$h4("Contacts"),
      "antonio.gasparrini@lshtm.ac.uk",tags$br(),
      "m.scortichini@deplazio.it",tags$br(),
      tags$br(),
      tags$h4("Last update: 2020/07/06")
    )
  )
)

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
  
  
  plot_results=reactive({
    
    # DEFINE OUTCOME
    out <- switch(input$outcome, "Daily deaths"=plot_df()$d_deaths, 
      "Daily excess"=plot_df()$d_excess, "Daily excess (%)"=plot_df()$ExcessPer)
    cilow <- switch(input$outcome, "Daily deaths"=NULL, 
      "Daily excess"=plot_df()$d_excess_low, "Daily excess (%)"=plot_df()$ExcessPer95eCIlow)
    cihigh <- switch(input$outcome, "Daily deaths"=NULL, 
      "Daily excess"=plot_df()$d_excess_high, "Daily excess (%)"=plot_df()$ExcessPer95eCIhigh)
    
    # DEFINE LABELS
    seqpost <- seq(dmy(01022020), dmy(15052020), 1)
    cutdate <- unique(c(dmy(01022020)-1,
      seqpost[seqpost %in% tapply(seqpost, week(seqpost), last)]))
    labperiod1 <- sapply(seq(length(cutdate)-1), function(i)
      paste(paste0(day(cutdate[i]+1), format(cutdate[i]+1, format="%b")),
        paste0(day(cutdate[i+1]), format(cutdate[i+1], format="%b")), sep="-"))
    
    # START PLOT
    plot <- ggplot(plot_df(), aes(x=period_n, y=out))
    
    # ADD CI IF NEEDED
    if(!is.null(cilow)) plot <- plot +
      geom_ribbon(aes(ymin=cilow, ymax=cihigh), fill=grey(0.9))
    if(input$outcome!="Daily deaths") plot <- plot + geom_hline(yintercept=0)
    
    # FINISH PLOT
    plot + geom_line(col="red3") + geom_point(size=4, col="red3") +
      ylab(input$outcome) + theme_bw() +
      scale_y_continuous() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5,size=12),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5,size=15),
        axis.title=element_text(size=14, face="bold"), legend.position="none",
        aspect.ratio=3/4) +
      scale_x_discrete(name="Week", limits=1:16, labels=labperiod1)
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

