## app.R ##
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(ggthemes)
library(lubridate)
library(data.table)
library(tidyr)
library(markdown)
library(leaflet)
library(shinydashboard)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(wesanderson)
library(ggthemes)
library(gridExtra)
library(xtable)
ui <- navbarPage("Kickstarter",
                 
                 tabPanel("World map", plotlyOutput("plot1"),leafletOutput("worldmap_plotID")),
                 navbarMenu("Statistic",
                            tabPanel("Linear Regression",htmlOutput("sumtable11"),verbatimTextOutput("sum1")),
                            tabPanel("Logistical regression", htmlOutput("sumtable"),htmlOutput("sumtext"),verbatimTextOutput("sum"),plotOutput("dplot"))
                           
                 ),
                 
                 navbarMenu("Data  visualisation",
                            tabPanel("number of case for Catergories", plotlyOutput("nocases"),plotlyOutput("plotstate")),
                            tabPanel("State of Kickstarter by Country", plotlyOutput("fac_country")),
                            tabPanel("Project by subcategory",plotlyOutput("project_by_sub")),
                            tabPanel("Total Amount Pledged",plotlyOutput("pledgedd"), plotlyOutput("amount")),
                            tabPanel("Number of project Launched", plotlyOutput("succbyyea"),plotlyOutput("yea"))
                            
                          
                            
                            
                 ),
                 navbarMenu("United States",
                            #sub panel under United States
                            ####successful rate analysis########
                            tabPanel("Successful Rate",
                                     fluidPage(
                                       plotlyOutput("US_scatterplot_ID"),
                                       plotlyOutput("US_success_rate_ID")
                                       
                                     )),
                            
                            tabPanel("Fund Amount",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons(inputId="state_ID", label="Outcome of the listed project",
                                                      choices= c("All"="All","Failed"="failed", "Successful"="successful",
                                                                 "Canceled"="canceled","Live"="live",
                                                                 "Undefined"="undefined","Suspended"="suspended"
                                                      ), selected = "successful"
                                         ),
                                         selectInput(inputId="category_under_goal_ID", label="Choose a category:",
                                                     choices = c("All"="All",unique((ks18)%>%filter(region=="United States")%>%select(main_category))), 
                                                     selected ="All"),
                                         sliderInput(inputId = "goal_range_ID",
                                                     label = "Upper limit of desired amount of funding",
                                                     min=0,max=300000,value=10000),
                                         sliderInput(inputId = "min_goal_range_ID",
                                                     label = "Lower limit of desired amount of funding",
                                                     min=0,max=300000,value=0),
                                         sliderInput(inputId = "binwidth_ID",
                                                     label = "Binwidth for better visualization",
                                                     min=0,max=10000,value=500),
                                         verbatimTextOutput("summary_ID")
                                       ),
                                       mainPanel(
                                         plotlyOutput("US_goal_ID"),
                                         fluidPage(
                                           DT::dataTableOutput("US_tableID"))
                                       )))
                            
                            ,
                            
                            ###category(include subcategory) distribution tab that returns a bar plot
                            tabPanel("Category Distribution",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons(inputId="category_state_ID", label="Outcome of the listed project",
                                                      choices= c("All"="All","Failed"="failed", "Success"="successful",
                                                                 "Cancelled"="canceled","Live"="live",
                                                                 "Undefined"="undefined","Suspended"="suspended"
                                                      ), selected = "successful"
                                         ),width=2
                                         
                                       ),
                                       mainPanel(
                                         plotlyOutput("US_category_ID"),width= 10
                                       )),
                                     DT::dataTableOutput("US_category_tableID")
                            ), 
                            
                            
                            
                            ####backers analysis########
                            tabPanel("Backers Distribution",
                                     sidebarLayout(
                                       plotlyOutput("US_backers_ID"),
                                       plotlyOutput("US_backers_by_category_ID")
                                     )),
                            
                            ####almost made it project: raised divided by goal#########################################
                         
                            
                            ##extremely successful project###########################
                            tabPanel("Extremely Succesful Project",
                                     titlePanel('These projects rasied more than they asked for...'),
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput(inputId="prop_success_max_ID", label="Choose lower bound of percentage overfunded",
                                                     min=1, max=2000, value=1.2),
                                         #choice to filter goal less than $10
                                         selectInput("sucessful_showone_ID", "Display options",
                                                     choices = c("Hide goal less than $100","Show all")
                                                     
                                         )),
                                       mainPanel(
                                         fluidPage(
                                           DT::dataTableOutput("US_successful_ID")
                                         )
                                       )
                                     )
                                     
                            )
                           ),
                 navbarMenu("Rest of the world",
                            tabPanel("Successful Rate",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId="Rest_success_rate_ID", 
                                                     label=h3("Select country for bar plot"), 
                                                     choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                                     selected ="All"),
                                         selectInput(inputId="Rest_scatter_ID", 
                                                     label=h3("Select country for scatter plot"), 
                                                     choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                                     selected ="All"),width = 2),
                                       mainPanel(
                                         fluidPage(
                                           plotlyOutput("Rest_US_success_rate_ID"),
                                           plotlyOutput("Rest_US_scatterplot_ID"))
                                         
                                       ))),
                            tabPanel("Fund Amount",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons(inputId="Rest_state_ID", label="Outcome of the project",
                                                      choices= c("All"="All","Failed"="failed", "Success"="successful",
                                                                 "Cancelled"="canceled","Live"="live",
                                                                 "Undefined"="undefined","Suspended"="suspended"
                                                      ), selected = "successful"
                                         ),
                                         sliderInput(inputId = "Rest_goal_range_ID",
                                                     label = "Upper limit of desired amount of funding",
                                                     min=0,max=300000,value=10000),
                                         sliderInput(inputId = "Rest_min_goal_range_ID",
                                                     label = "Lower limit of desired amount of funding",
                                                     min=0,max=300000,value=0),
                                         sliderInput(inputId = "Rest_binwidth_ID",
                                                     label = "Binwidth for better visualization",
                                                     min=0,max=10000,value=500),
                                         selectInput(inputId="Rest_region_ID", 
                                                     label=h3("Select country"), 
                                                     choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                                     selected ="All"),
                                         selectInput(inputId="Rest_category_under_goal_ID", label="Choose a category:",
                                                     choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(main_category))), 
                                                     selected ="All"),
                                         verbatimTextOutput("Rest_summary_ID")
                                       ),
                                       mainPanel(
                                         plotlyOutput("Rest_US_goal_ID"),
                                         fluidPage(
                                           DT::dataTableOutput("Rest_US_tableID"))
                                       )
                                     )
                            ),
                            ###main cateogry distribution tab that returns bar plot
                            tabPanel("Category Distribution",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons(inputId="Rest_main_category_state_ID", label="State of the project",
                                                      choices= c("All"="All","Failed"="failed", "Success"="successful",
                                                                 "Cancelled"="canceled","Live"="live",
                                                                 "Undefined"="undefined","Suspended"="suspended"
                                                      ), selected = "successful"
                                         ),
                                         selectInput(inputId="Rest_main_category_region_ID", 
                                                     label="Select country", 
                                                     choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                                     selected ="All"),
                                         
                                         width=2 ),
                                       mainPanel(
                                         plotlyOutput("Rest_main_US_category_ID"), width= 10
                                       )),
                                     DT::dataTableOutput("Rest_main_US_category_tableID")
                            ),
                            ####backers analysis################################
                            tabPanel("Backers Distribution",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("Rest_backers_region_ID", "Top: Choose a country:",
                                                     choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                                     selected ="All"),
                                         selectInput("Rest_backers_category_region_ID", "Bottom: Choose a country for category:",
                                                     choices = c("All"="All",unique((ks18)%>%filter(region!="United States")%>%select(region))), 
                                                     selected ="All"),width=2 ),
                                       mainPanel(
                                         fluidPage(
                                           plotlyOutput("Rest_US_backers_ID"),
                                           plotlyOutput("Rest_backers_by_category_ID")
                                           
                                         ),width=10
                                       )
                                     )),
                          
                            ##extremely successful project########################
                            
                            tabPanel("Extremely Succesful Project",
                                     titlePanel('These projects rasied more than they asked for...'),
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput(inputId="Rest_prop_success_max_ID", label="Choose lower bound of percentage overfunded",
                                                     min=1, max=2000, value=1.2),
                                         #choice to filter goal less than $10
                                         selectInput("Rest_sucessful_showone_ID", "Display options",
                                                     choices = c("Hide goal less than $100","Show all")
                                                     
                                         )),
                                       mainPanel(
                                         fluidPage(
                                           DT::dataTableOutput("Rest_US_successful_ID")
                                         )
                                       )
                                     )
                                     
                            )
                              
                            
                           ),
            
                 
                 
                 
              
                 tabPanel("Data",DT::dataTableOutput("tableID"))
)
#-------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  setwd("~/Desktop/Project RShiny 26th feb/Kickstarter")
  
  Main<-read.csv("/Users/ishraq/Desktop/Project RShiny 26th feb/Kickstarter/Subset of Main.csv",header= T) #Load data
  
  ks18<-read.csv(file = "/Users/ishraq/Desktop/Project RShiny 26th feb/Kickstarter/ks-projects-201801.csv",sep = ',',fill=TRUE)
  bycountry<-fread(file="/Users/ishraq/Desktop/Project RShiny 26th feb/Kickstarter/bycountry.csv")
  withoutlive<-ks18%>%filter(state!="live",region=="United States")%>%group_by(main_category)%>%summarize(rate=sum(state=="successful")/n())
  successful_scatter<-ks18%>%filter(state!="live",region=="United States")%>%group_by(main_category)%>%summarize(projects=n(),successful_projects=sum(state=="successful"))
  
  piedata<-ks18%>%summarise(US_Observation=sum(region=="United States"), Rest_Observation=sum(region!="United States"))
  piedata2<-gather(piedata,key=region,value=observation, US_Observation,Rest_Observation)
  
  output$worldmap_plotID<-renderLeaflet({
    leaflet(bycountry) %>% 
      addProviderTiles("Esri.NatGeoWorldMap")%>%addCircleMarkers(~longitude,~latitude,
                                                                 radius=0.075*sqrt(bycountry$num),
                                                                 label=paste0(bycountry$region,': ',bycountry$num, ' projects'),
                                                                 fillOpacity = 0.3, color = '#D55E00')
  })
  
  US_reactive <- reactive({
    ks18%>%filter(region == "United States")})
  
  #ggplotly output of goal distribution
  output$US_goal_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Processing plots...")
      plotdraft<-US_reactive()%>%
        #add a "no-filter" option to the filter using ifelse statement
      {if(input$state_ID!="All") filter(.,state==input$state_ID,
                                        goal<input$goal_range_ID,goal>input$min_goal_range_ID) else 
                                          filter(.,goal<input$goal_range_ID,goal>input$min_goal_range_ID)}%>%
        #filter category
                                          {if(input$category_under_goal_ID!="All") filter(.,main_category==input$category_under_goal_ID)
                                            else 
                                              (.)}%>%
        ggplot(aes(x=goal))+geom_histogram(binwidth = input$binwidth_ID)+
        labs(x="Project goal",y="Number of project",title="Number of projects of desired funding target")
      ggplotly(plotdraft)})})
  
  #summary for goal given state of the project
  output$summary_ID <- renderPrint({
    dataset <- US_reactive()%>%
    {if(input$state_ID!="All") filter(.,state==input$state_ID)else .}%>%
    {if(input$category_under_goal_ID!="All") filter(.,main_category==input$category_under_goal_ID)
      else 
        (.)}%>%
      select("Summary of project funding"=goal)
    summary(dataset)
  })
  #selected data table given state of the project and the goal
  output$US_tableID <-DT::renderDataTable({
    datatable_goal<-US_reactive()%>% {if(input$state_ID!="All") filter(.,state==input$state_ID,
                                                                       goal<input$goal_range_ID) else 
                                                                         filter(.,goal<input$goal_range_ID)}%>%
      select(ID,name,category,state,goal)
    DT::datatable(datatable_goal)
  })
  
  
  ####main category and subcategory observations############
  #plot histogram for each category given the state of the project and selected observation
  
  
  output$US_category_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Large dataset. Please wait...")
      
      plotdraft2<-US_reactive()%>%{if(input$category_state_ID=="All") . else filter(.,state==input$category_state_ID)}%>%
        group_by(main_category)%>%mutate(totalprojects=n())%>%
        ggplot(aes(x=main_category,fill=category))+
        geom_bar(aes(text=paste("Subcategory:", category, totalprojects)))+
        theme(legend.position="none")+
        labs(x="Project Category",y="Number of project")
      
      ggplotly(plotdraft2,tooltip="text")})
    
  })
  
  #selected data table given state of the project and the category
  output$US_category_tableID <-DT::renderDataTable({
    datatable_category<-US_reactive()%>%
    {if(input$category_state_ID!="All") filter(.,state==input$category_state_ID) else  .}%>%
      select(ID,name,main_category,category,state,goal)
    
    DT::datatable(datatable_category)
  })
  
  ###########sucessful rate analysis by category#################################
  output$US_success_rate_ID<-renderPlotly({
    successrate_plot<-withoutlive%>%
      ggplot(aes(reorder(x=main_category,-rate),y=rate))+
      geom_col(aes(text=paste("Successful rate", rate)))+labs(title="Successful project vs All project",
                                                              subtitle="Points on the left indicate a category with an above 0.5 succesful rate",x="Category",y="Succesful rate")+
      geom_hline(aes(yintercept = mean(withoutlive$rate)))
    ggplotly(successrate_plot,tooltip="text")
  })
  
  ###success analysis with scatter plot######################################
  output$US_scatterplot_ID<-renderPlotly({
    scatter_plot<-successful_scatter%>%
      ggplot(aes(x=projects,y=successful_projects))+
      geom_point(aes(fill=main_category),size=3)+geom_abline(slope=0.5)+
      labs(x="Total Number of projects",y="Number of successful projects")
    ggplotly(scatter_plot)
  })
  
  
  #####Backers Analysis############################################################
  
  output$US_backers_ID<- renderPlotly({
    plotdraft3<-US_reactive()%>%
      group_by(state)%>%
      summarize(average_number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=state,-average_number_of_backers),y=average_number_of_backers))+
      geom_bar(stat="identity", aes(text=paste("Backers:\n", average_number_of_backers)))+labs(x="Average number of backers",y="Outcome of the project")
    ggplotly(plotdraft3,tooltip="text")
  })
  
  output$US_backers_by_category_ID<- renderPlotly({
    plotdraft4<-US_reactive()%>%
      group_by(main_category)%>%filter(state == "successful")%>%
      summarize(average_number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=main_category,-average_number_of_backers),y=average_number_of_backers))+
      geom_bar(stat="identity",aes(text=paste("Backers:\n", average_number_of_backers)))+
      labs(y="Average number of backers",x="Outcome of the project")
    
    ggplotly(plotdraft4,tooltip="text")
  })
  
  
  
  ###almost made it project#########################################################
  output$US_almost_made_it_ID<-DT::renderDataTable({
    datatable_almost<-US_reactive()%>%mutate(percent_funded=pledged/goal*100)%>%
      filter(percent_funded<input$prop_max_ID, percent_funded>input$prop_min_ID)%>%
      select(ID,name,category,state,goal,percent_funded)
    
    DT::datatable(datatable_almost)
  })
  ###extremley successful project#########################################################
  output$US_successful_ID<-DT::renderDataTable({
    datatable_successful<-US_reactive()%>%mutate(over_funded=pledged/goal*100)%>%
      filter(over_funded>input$prop_success_max_ID)%>%
      {if(input$sucessful_showone_ID!="Show all") filter(.,goal>100)
        else .}%>%filter(state=="successful")%>%
      select(ID,name,category,state,goal,over_funded)
    
    DT::datatable(datatable_successful)
  })

  
  ###########################################################################################
  #################Rest of the world##########################################################
  ####data for rest of the world tab with ###############################################
  Rest_US_reactive <- reactive({
    ks18%>%filter(region != "United States")})
  
  
  
  #ggplotly output of goal distribution
  output$Rest_US_goal_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Processing plots...")
      Rest_plotdraft<-Rest_US_reactive()%>%
        #add a "no-filter" option to the filter using ifelse statement for the state of the project
      {if(input$Rest_state_ID!="All") filter(.,state==input$Rest_state_ID,
                                             usd_goal_real<input$Rest_goal_range_ID,usd_goal_real>input$Rest_min_goal_range_ID) else 
                                               filter(.,usd_goal_real<input$Rest_goal_range_ID,usd_goal_real>input$Rest_min_goal_range_ID)}%>%
        #add a filter option to let user select countries
                                               {if(input$Rest_region_ID!="All") filter(.,region==input$Rest_region_ID)
                                                 else .}%>%
        #filter category
                                                 {if(input$Rest_category_under_goal_ID!="All") filter(.,main_category==input$Rest_category_under_goal_ID)
                                                   else 
                                                     (.)}%>%
        ggplot(aes(x=usd_goal_real))+geom_histogram(binwidth = input$Rest_binwidth_ID)+
        labs(x="Project goal",y="Number of project",title="Number of projects of desired funding target")
      ggplotly(Rest_plotdraft)})})
  
  #summary for goal given state of the project
  output$Rest_summary_ID <- renderPrint({
    Rest_dataset <- Rest_US_reactive()%>%
    {if(input$Rest_state_ID!="All") filter(.,state==input$Rest_state_ID)else .}%>%
    {if(input$Rest_region_ID!="All") filter(.,region==input$Rest_region_ID) else .}%>%
      select("Summary of project funding"=usd_goal_real)
    summary(Rest_dataset)
  })
  #selected data table given state of the project and the goal
  output$Rest_US_tableID <-DT::renderDataTable({
    Rest_datatable_goal<-Rest_US_reactive()%>% {if(input$Rest_state_ID!="All") filter(.,state==input$Rest_state_ID,
                                                                                      usd_goal_real<input$Rest_goal_range_ID, usd_goal_real>input$Rest_min_goal_range_ID) else 
                                                                                        filter(.,usd_goal_real<input$Rest_goal_range_ID,usd_goal_real>input$Rest_min_goal_range_ID)}%>%
                                                                                        {if(input$Rest_region_ID!="All") filter(.,region==input$Rest_region_ID) else .}%>%
                                                                                        {if(input$Rest_category_under_goal_ID!="All") filter(.,main_category==input$Rest_category_under_goal_ID)
                                                                                          else 
                                                                                            (.)}%>%
      select(ID,name,category,state,usd_goal_real,region)
    DT::datatable(Rest_datatable_goal)
  })
  
  
  ##########main category observation#############################
  
  #plot histogram for each category given the state of the project and selected observation
  output$Rest_main_US_category_ID<- renderPlotly({
    withProgress({
      setProgress(message = "Processing plots...")
      
      Rest_main_plotdraft2<-Rest_US_reactive()%>%{if(input$Rest_main_category_state_ID=="All") . else filter(.,state==input$Rest_main_category_state_ID)}%>%
      {if(input$Rest_main_category_region_ID!="All") filter(.,region==input$Rest_main_category_region_ID)
        else .}%>%group_by(main_category,category)%>%
        ggplot(aes(x=main_category,fill=category))+
        geom_bar(aes(text=paste("Category:", main_category, "\nSubcategory:", category)))+
        theme(legend.position="none")+
        labs(x="Project Category",y="Number of project")
      
      ggplotly(Rest_main_plotdraft2,tooltip="text")})
    
  })
  #selected data table given state of the project and the category
  output$Rest_main_US_category_tableID <-DT::renderDataTable({
    Rest_main_datatable_category<-Rest_US_reactive()%>%
    {if(input$Rest_main_category_state_ID!="All") filter(.,state==input$Rest_main_category_state_ID) else  .}%>%
    {if(input$Rest_main_category_region_ID!="All") filter(.,region==input$Rest_main_category_region_ID)
      else .}%>%
      select(ID,name,main_category,category,state,usd_goal_real,region)
    
    DT::datatable(Rest_main_datatable_category)
    
  })
  
  
  ###########sucessful rate analysis by category#################################
  output$Rest_US_success_rate_ID<-renderPlotly({
    Rest_successrate<-Rest_US_reactive()%>%filter(state!="live")%>%
    {if(input$Rest_success_rate_ID!="All") filter(.,region==input$Rest_success_rate_ID)
      else .}%>%
      group_by(main_category)%>%summarize(rate=sum(state=="successful")/n())
    Rest_successrate_plot<-Rest_successrate%>%ggplot(aes(reorder(x=main_category,-rate),y=rate))+
      geom_col(aes(text=paste("Successful rate", rate)))+labs(title="Successful project vs All project",
                                                              subtitle="Points on the left indicate a category with an above 0.5 succesful rate",x="Category",y="Succesful rate")+
      geom_hline(yintercept = mean(Rest_successrate$rate))
    ggplotly(Rest_successrate_plot,tooltip="text")
  })
  
  ###success analysis with scatter plot######################################
  output$Rest_US_scatterplot_ID<-renderPlotly({
    Rest_successful_scatter<-Rest_US_reactive()%>%filter(state!="live")%>%
    {if(input$Rest_scatter_ID!="All") filter(.,region==input$Rest_scatter_ID)
      else .}%>%
      group_by(main_category)%>%summarize(projects=n(),successful_projects=sum(state=="successful"))
    
    Rest_scatter_plot<-Rest_successful_scatter%>%
      ggplot(aes(x=projects,y=successful_projects))+
      geom_point(aes(fill=main_category),size=3)+geom_abline(slope=0.5)+
      labs(x="Total Number of projects",y="Number of successful projects")
    ggplotly(Rest_scatter_plot)
  })
  
  
  
  #####Backers Analysis############################################################
  
  output$Rest_US_backers_ID<- renderPlotly({
    Rest_plotdraft3<-Rest_US_reactive()%>%
      group_by(state)%>%
      {if(input$Rest_backers_region_ID!="All") filter(.,region==input$Rest_backers_region_ID)
        else .}%>%
      summarize(average_number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=state,-average_number_of_backers),y=average_number_of_backers))+
      geom_bar(stat="identity", aes(text=paste("Backers:\n", average_number_of_backers)))+
      labs(y="Average number of backers",x="Outcome of the project")
    ggplotly(Rest_plotdraft3,tooltip="text")
  })
  
  output$Rest_backers_by_category_ID<- renderPlotly({
    Rest_plotdraft4<-Rest_US_reactive()%>% group_by(main_category)%>%
    {if(input$Rest_backers_category_region_ID!="All") filter(.,region==input$Rest_backers_category_region_ID)
      else .}%>%
      filter(state == "successful")%>%
      summarize(average_number_of_backers=mean(backers))%>%
      ggplot(aes(reorder(x=main_category,-average_number_of_backers),y=average_number_of_backers))+
      geom_bar(stat="identity",aes(text=paste("Backers:\n", average_number_of_backers)))+
      labs(y="Average number of backers",x="Outcome of the project")
    
    
    ggplotly(Rest_plotdraft4,tooltip="text")
  })
  
  
  
  
  
  ###almost made it project#########################################################
  output$Rest_US_almost_made_it_ID<-DT::renderDataTable({
    Rest_datatable_almost<-Rest_US_reactive()%>%mutate(percent_funded=usd_pledged_real/usd_goal_real*100)%>%
      filter(percent_funded<input$Rest_prop_max_ID, percent_funded>input$Rest_prop_min_ID)%>%
      select(ID,name,category,state,usd_goal_real,region,percent_funded)
    
    DT::datatable(Rest_datatable_almost)
  })
  ###extremley successful project#########################################################
  
  output$Rest_US_successful_ID<-DT::renderDataTable({
    Rest_datatable_successful<-Rest_US_reactive()%>%mutate(over_funded=usd_pledged_real/usd_goal_real*100)%>%
      filter(over_funded>input$Rest_prop_success_max_ID)%>%
      {if(input$Rest_sucessful_showone_ID!="Show all") filter(.,usd_goal_real>100)
        else .}%>%filter(state=="successful")%>%
      select(ID,name,category,state,usd_goal_real,region,over_funded)
    
    DT::datatable(Rest_datatable_successful)
  })
  
  
  output$tableID <- DT::renderDataTable({
    DT::datatable(ks18)})
  
  
  
  

  
  
  
  
  
  
  
  #-------------- 
  Main$ID<-as.factor(Main$ID)
  Main$deadline<-as.Date(Main$deadline)
  Main$launched<-as.Date(Main$launched)
  Main<-cbind.data.frame(Main,no_cases=1)
  Main$deadline.YM<-substr(Main$deadline,1,7)
  Main$launched.YM<-substr(Main$launched,1,7)
  xstate<- aggregate(as.formula(paste("no_cases","~","state")), data = Main, FUN=sum, na.rm=TRUE)
  
 output$plot1<- renderPlotly({m <- list(
    l = 25,
    r = 25,
    b = 150,
    t = 150,
    pad = 5
  )
  
  xstate %>%
    plot_ly(labels = ~state, values = as.formula(paste("~",names(xstate[2]))), width = 600, height = 600) %>%
    add_pie(hole = 0.5, textinfo = 'label+percent') %>%
    layout(showlegend = T,
           xaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           yaxis = list(showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           autosize = F, margin = m)
 }) 
  
 Main$successful<-ifelse(Main$state=="successful"|Main$state=="live","Yes","No")
 
 Main$successfulNo<-ifelse(Main$state=="successful"|Main$state=="live",1,0)
 
 aggmain<-aggregate(cbind(no_cases, successfulNo, goal, pledged, backers, usd.pledged, usd_pledged_real, usd_goal_real)~main_category+category+currency+country+deadline.YM+launched.YM+successful, data = Main, FUN=sum, na.rm=TRUE)%>%
   arrange(main_category,category)
 
 CountCS<-aggregate(no_cases~main_category+category, data = Main, FUN=sum, na.rm=TRUE)%>%
   arrange(main_category)
  
  
 output$nocases<-renderPlotly({
 p<-ggplot(CountCS, aes(x = reorder(main_category,-no_cases), y = no_cases, fill = category))+
   geom_bar(position = "fill",stat = "identity", width = 0.5) +
   xlab('Category')+ ylab('Number of Cases')+
   scale_y_continuous(labels = percent_format()) + 
   theme(axis.text.x=element_text(angle=-90, hjust=0.001)) # "angle" will tilt the labels
 ggplotly(p, tooltip = c("y", "colour", "text", "label", "fill"))})
 
 Main$usd.pledged <- ifelse(is.na(Main$usd.pledged),
                            ave(Main$usd.pledge, FUN= function(x) mean(x, na.rm = TRUE)),
                            Main$usd.pledged)
  
  
 Main$successfulNo<-as.factor(Main$successfulNo)
 Main$state <- ifelse(Main$state=="successful"|Main$state=="live",1,0)
  
 library(caTools)
 set.seed(101)
 split <- sample.split(Main$state, SplitRatio = 0.75)
 final.train <- subset(Main,split == TRUE)
 final.test <- subset(Main, split == FALSE)
  
 

  
   logreg <- glm(formula = successfulNo ~ category   +deadline + pledged + goal +launched  + backers  + usd_pledged_real + usd_goal_real, family=binomial(link='logit'),data=final.test)
summary(logreg)

output$sumtable <- renderPrint({
  mod<-logreg
  a<-summary(mod)
  output$sum<-renderPrint(a)
 
  
} )

output$dplot <- renderPlot({
  mod<- logreg
  par(mfcol=c(2,2))
  plot(mod)
})


regressor <- lm(formula = usd_pledged_real ~ category + main_category + currency + deadline + goal +launched + pledged +state , data= final.test)
summary(regressor)

output$sumtable11 <- renderPrint({
  mod1<-regressor
  b<-summary(mod1)
  output$sum1<-renderPrint(b)

  
} )



output$plotstate<- renderPlotly({
stplot4 <- ggplot(Main, aes(state)) + geom_bar(aes(fill=factor(main_category))) +ggtitle("State") + xlab("")
ggplotly(stplot4)})

output$fac_country<- renderPlotly({cont<-ggplot(Main,aes(country)) + geom_bar(aes(fill=factor(state)))
ggplotly(cont)})



output$project_by_sub<- renderPlotly({subcat.freq <- Main%>%
  group_by(category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

subcat.freq$category <- factor(subcat.freq$category, levels=subcat.freq$category)

ggplot(head(subcat.freq, 10), aes(category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Subcategory") + xlab("Project Subcategory") + ylab("Frequency")  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")})


output$amount<- renderPlotly({  pledged.v.project <-ggplot(Main, aes(main_category, usd.pledged, fill=main_category)) + geom_boxplot() + 
  ggtitle("Amount Pledged vs. Project Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,20000))

ggplotly(pledged.v.project)})


output$pledgedd<- renderPlotly({ pledged.tot <- Main %>%
  group_by(main_category) %>%
  summarize(total=sum(usd.pledged)) %>%
  arrange(desc(total))

pledged.tot$main_category <- factor(pledged.tot$main_category, levels=pledged.tot$main_category)

total.amount <-ggplot(pledged.tot, aes(main_category, total/1000000, fill=total)) + geom_bar(stat="identity") + 
  ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD millions)")  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

ggplotly(total.amount)})








output$succbyyea <- renderPlotly({year.freq <- Main%>%
  filter(year(launched)!="1970") %>%
  group_by(year=year(launched)) %>%
  summarize(count=n())

year_project <- ggplot(year.freq, aes(year, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Number of Projects by Launch Year") + xlab("Year") + ylab("Frequency") + 
  scale_x_discrete(limits=c(2009:2018)) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

ggplotly(year_project)}) 











  #-------------------------
 

  output$plot <- renderPlotly({# pplotly#
    p<-ggplot(data=Main,aes(x = newstate,y= usd.pledged, colour=newstate))+geom_bar(stat="identity")
    ggplotly(p)
    
  })
}

shinyApp(ui, server)
