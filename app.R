#Loading required Packages
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(highcharter)
library(ggplot2)
library(fontawesome)
library(leaflet)
library(shinycssloaders)
#===================================================================================================================================
#Data Preprocessing
df<-read.csv("Inflation-rate.csv")
inflation_data <- read.csv("congeo.csv")
head(df)
colnames(df)<-c('x','Country',c(1980:2022))
head(df)
inf<-df %>% gather(key = "Year",value="Inflationrate",3:45)
inf
inf<-na.omit(inf)
inf<-inf[,c(2:4)]
names(inf)<-c("region","year","inflation")
head(inf)
inf
inf$year<-as.integer(inf$year)
India<-filter(inf,region=="India")
India$inflation<-as.numeric(India$inflation)
India$year<-as.numeric(India$year)
China<-filter(inf,region=="China, People's Republic of")
Ger<-filter(inf,region=="Germany")
Japan<-filter(inf,region=="Japan")
US<-filter(inf,region=="United States")
EU<-filter(inf,region=="European Union")
UK<-filter(inf,region=="United Kingdom")
Fr<-filter(inf,region=="France")
uae<-filter(inf,region=="United Arab Emirates")

country<-c("India","United States","Mexico","Canada","China, People's Republic of","Japan","Russian Federation","Germany","United Kingdom","European Union",
           "ASEAN-5","New Zealand","Australia","Netherlands","Luxembourg",
           "France","Qatar","United Arab Emirates","Saudi Arabia")

unions<-c("Major advanced economies (G7)","European Union","Emerging and Developing Europe","ASEAN-5","Commonwealth of Independent States",
          "Emerging and Developing Asia","Latin America and the Caribbean",
          "Middle East, North Africa, Afghanistan, and Pakistan")
USA1<-inf%>% filter(region=="United States")  
USA1<-round(mean(inf$inflation),2)
USA1

ind<-inf%>% filter(region=="India")  
ind<-round(max(inf$inflation),2)
ind
#============================================================================================================================
#ui for shiny App
ui<-dashboardPage(
  #defines header
  skin = "purple",
  dashboardHeader(
    title="Inflation Rates" ,
    dropdownMenu()
  ),
  
  
  #defines sidebar
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
     
      
      menuItem("Trade Unions",tabName="unions",icon=icon("signal")),
      menuItem("World",tabName="world",icon=icon("globe"))
      
    )
  ),
  
  
  #defines bodys
  dashboardBody(
    tags$head(tags$style(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      
    )),
    
    tabItems(
      
      #First TAB Menu-Dashboard  
      tabItem(tabName = "dashboard",
              
              fluidRow(
                infoBoxOutput('India',width=4),infoBoxOutput('Globe',width=4),infoBoxOutput('USA',width=4)
                
               ),
              
              fluidRow(
                
                
                column(12,
                       
                       box(status="info",selectInput("country",label="Select Country",choices=country),width = 6),
                       box(status="info", sliderInput("year",'Range of year', min = 1980, max =2022, value = c(1980,2022)),width = 6)
                      
                
                       
                ),#end column
                
                #box for plotting the time series plot
                column(12,
                       
                       box(status = "primary",background = "maroon",
                         
                         highchartOutput("hcontainer"),
                         
                         
                         
                         width="12") #end box2
                       
                ), #end column
                hr(),
                h4("Relative inflation rates time series plot",align="center"),
                br(),
                column(12,
                       
                       box(background = "maroon",
                         highchartOutput("hc2"),width=12
                         
                       ) )
                
              )#end row
              
      ),
      
      
      
      
      #second tab menu- ABOUT
      tabItem(tabName="about",
              fluidRow(
                column(width = 8, tags$img(src="inf.jpg", width =800 , height = 400)
                       )),
              fluidRow(column(12,
              h2("What is Inflation ?",style="text-align:center"),
              br(),
              br(),
              box(width=12,height="800px",
                  p(style="font-size:20px",strong("Inflation"),"is a sustained increase in the general price level of goods and services 
                    in an economy over a period of time. It is typically measured by the rate of change of a consumer price index (CPI) or a producer price index (PPI). 
                    Inflation affects everyone in the economy, from consumers to businesses to governments.."),
                  h4(strong('Cause of Inflation')),
                  p(style="font-size:20px", "One of the primary causes of inflation is an increase in the money supply.
                    When the central bank of a country prints more money, it increases the amount of money in circulation. 
                    This, in turn, can lead to an increase in demand for goods and services, which can cause prices to rise.."), 
                  p(style="font-size:20px","Another cause of inflation is an increase in the cost of production.
                    When the cost of raw materials, labor, or other inputs increases, businesses may raise prices to maintain their profit margins.
                    This is known as cost-push inflation"),
                  p(style="font-size:20px","Inflation can have both positive and negative effects on the economy.
                    On the one hand, a moderate level of inflation can be a sign of a healthy economy, as it indicates that there is demand for goods and services. It can also help reduce the burden of debt, as inflation reduces the real value of debt over time."),
                  p(style="font-size:20px","Central banks play a key role in controlling inflation. 
                    They can use monetary policy tools such as interest rates and open market operations to influence the money supply and keep inflation within a target range. Governments can also implement fiscal policy measures such as taxation and spending to help control inflation"),
                  p(style="font-size:20px",strong("Deflation"), "is opposite of inflation. Delfation occurs when the inflation rates become negetive or are below 0. Deflation is more harmful and dangerous for an economy because it means that the prices of goods and services are going to decrease. Now this sounds amazing for consumers like us. But what actually happens is that the demand of goods and services have declined over a long term of time. This directly indicates that a recession is on its way. This brings job losses , declining wages and a big hit to the stock portfolio. Deflation slows economy's growth. As prices fall , people defer(postpone) purchases in hope of a better lower price deal. Due to this companies and firms have to cut down the cost of their goods and products which directly affects the wages of the employees which have to be lowered.")
                  
                 
              )
              
      ))),
      
      tabItem(tabName = "unions",
              
              h3("Time series of Inflation rates of Economic trade unions",align="center") ,
              
              fluidRow(
                
                
                column(12,
                       
                       box(selectInput("region",label="Select Economic Region",choices=unions),width = 12) 
                       
                ),
                
                box(background = "maroon",
                  highchartOutput("hc3"),
                  width=12)
                
              )# end row 
      ),
      tabItem(tabName = "world",
              
              h3("World's Inflation Rates",align="center") ,
              box(background = "maroon",
                  leafletOutput("map"),
                  width=12),hr(),
              box(background = "maroon",
                highchartOutput("hc4"),
                width=12)
              
      )
    )#end tabitems
    
    
  )#end body
  
)#end dashboard
#===================================================================================================================================================
#Server for Shiny App
server <- function(input, output) { 
  
  output$India<-renderInfoBox({
    infoBox(title = p(strong("India"),style="font-size: 20px"),value=ind,subtitle ="Maximum inflatation rate ", icon = icon("rupee-sign"),color = 'teal')
  })
  output$Globe<-renderInfoBox({
    infoBox(title = p(strong("Globe"),style="font-size: 20px"),value=7.5,subtitle ="avg inflatation rate per year", icon = icon("globe"),color = 'teal')
  })
  output$USA<-renderInfoBox({
    infoBox(title = p(strong("USA"),style="font-size: 20px"),value=USA1,subtitle ="minimum inflatation rate", icon = icon("usd"),color = 'teal')
  })
  
  
  output$hcontainer <- renderHighchart ({
    
    #if(input$country==inf$region)
    #{
    df<-inf %>% filter(region==input$country)#making is the dataframe of the country
    
    df$inflation<-as.numeric(df$inflation)
    df$year<-as.numeric(df$year)
    reactive_data <- reactive({
       #Filter data based on slider input
      filtered_data <- df[df$year >= input$year[1]&df$year <=input$year[2], ]
      return(filtered_data)
    })
    
    #plotting the data
    hchart(reactive_data(), "line",color="#DC270C",hcaes(x=year,y=inflation))  %>%
      
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Time series plot of Inflation Rates",align="center") %>%
      hc_subtitle(text="Data Source: IMF",align="center") %>%
      hc_add_theme(hc_theme_elementary()) 
    #to add 3-d effects
    #hc_chart(type = "column",
    #options3d = list(enabled = TRUE, beta = 15, alpha = 15))
    
    
    
    
    
  })
  
  
  output$hc2<-renderHighchart({
    
    highchart() %>% 
      hc_xAxis(categories=c(1980:2022)) %>% 
      hc_add_series(name = "India", data = India$inflation) %>% 
      hc_add_series(name = "USA", data = US$inflation) %>%
      hc_add_series(name = "UK", data = UK$inflation) %>%
      hc_add_series(name = "China", data = China$inflation) %>%
      hc_add_series(name = "Germany", data = Ger$inflation) %>%
      hc_add_series(name="Japan",data=Japan$inflation) %>%
      #to add colors
      hc_colors(c("red","blue","green","purple","darkpink","orange")) %>%
      hc_add_theme(hc_theme_elementary())
    
    
    
    
    
  })
  
  
  output$hc3<-renderHighchart({
    
    union<-inf %>% filter(region==input$region)
    union$year<-as.numeric(union$year)
    union$inflation<-as.numeric(union$inflation)
    
    #plotting
    hchart(union,hcaes(x=year,y=inflation),type="area",color="#2B1F97") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Time series plot of Inflation Rates for Economic Unions",align="center") %>%
      hc_subtitle(text="Data Source: IMF",align="center") %>%
      hc_add_theme(hc_theme_elementary())
    
    
    
    
  })
  output$map <- renderLeaflet({
    # Filter the data for the current year
    
    
    # Create the map
    leaflet(inflation_data ) %>%
      addTiles() %>%
      addMarkers(
        label = ~paste0(Country, ": ", Inflation.Rate)
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })

  
  output$hc4<-renderHighchart({
    world<-inf %>% filter(region=="World")
    world$year<-as.numeric(world$year)
    world$inflation<-as.numeric(world$inflation)
    #plotting the plot
    hchart(world,hcaes(x=year,y=inflation),type="area",color="#B915A3") %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Time series plot of Inflation Rates for World",align="center") %>%
      hc_subtitle(text="Data Source: IMF",align="center") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  
  
}
shinyApp(ui=ui,server=server)


