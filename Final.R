library(tidyverse)
library(shiny)
library(ggthemes)
library(dslabs)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(ggrepel)
library(png)
library(scales)
library(arsenal)
library(RColorBrewer)
library(readr)
library(httr)
library(magick)

coln <- cols(
  id = col_integer(),
  name = col_character(),
  host_id = col_integer(),
  host_name = col_character(),
  neighbourhood_group = col_character(),
  neighbourhood = col_character(),
  latitude = col_double(),
  longitude = col_double(),
  room_type = col_character(),
  price = col_integer(),
  minimum_nights = col_integer(),
  number_of_reviews = col_integer(),
  last_review = col_date(format = ""),
  reviews_per_month = col_double(),
  calculated_host_listings_count = col_integer(),
  availability_365 = col_integer()
)
li <- read_csv("https://raw.githubusercontent.com/JIngyichen0701/Group_Final_Project/master/listings.csv", col_types = coln)
colnames(li)[5]<-'Neighbourhood Group'
colnames(li)[6]<-'Neighbourhood'
colnames(li)[7]<-'Latitude'
colnames(li)[8]<-'Longitude'
colnames(li)[9]<-'Room Type'
colnames(li)[10]<-'Price'
colnames(li)[11]<-'Minimum Nights'
colnames(li)[12]<-'Number of Reviews'
colnames(li)[13]<-'Reviews per Month'
colnames(li)[14]<-'Listings per Host'
colnames(li)[15]<-'Availability per 365'
li$`Room Type` <- as.factor(li$`Room Type`)
li$`Neighbourhood Group` <- as.factor(li$`Neighbourhood Group`)
li$Neighbourhood <- as.factor(li$Neighbourhood)
li$Log_Price = log10(li$Price)



listings <- read.csv("https://raw.githubusercontent.com/JIngyichen0701/Group_Final_Project/master/listings.csv")
listings2 <- listings[,c("host_name","calculated_host_listings_count","room_type","name", "number_of_reviews", "price")]

ui <- fluidPage(
  titlePanel("Airbnb in Singapore: Find Your Ideal Place"),
   tabsetPanel(
    
       tabPanel("Basic Info",
             sidebarLayout(
               fluidRow(
                 column(6,
                 # Add some text and a couple of hyper links before the slider for year
                 br(),
                 p(em("by Jingyi Chen, Marina Chen, Xiaoxuan Liu")),
                 p(a("Screencast can be found here!", href= "https://youtu.be/pib9RUsHMs0")),
                 p(strong("Background&Motivation")), 
                 p("Airbnb, Inc. is an online marketplace for arranging or offering lodging
                 , primarily homestays, or tourism experiences. It has been growing dramatically since its inception in 2008 
                 with the number of rentals listed on its website growing exponentially each year. It currently covers more 
                 than 81,000 cities and 191 countries worldwide. Airbnb has successfully disrupted the traditional hospitality
                 industry as more and more travelers resort to Airbnb as their priority accommodation provider. "),
                 br(),
                 p("Singapore has
                 been one of the hottest markets for Airbnb, especially in Asia. There were 
                 estimated 7,000 Singapore property listings on Airbnb as of November 2016. An Singapore host can also earn 
                 an average of $5,000 a year from listing their property on Airbnb. One possible attribute of the success of 
                 Airbnb in Singapore is that tourism is a major industry and contributor to the Singaporean economy, and now
                 more tourists prefer homestays rather than hotel rooms to experience local culture and lifestyle."),
                 br(),
                 p("We are 
                 therefore interested in understanding the Airbnb rental landscape in Singapore from customers’ decision-making
                 perspective. We conducted an exploratory analysis of Singapore Airbnb open data leveraging various static and 
                 interactive visualization tools. We also provided a forecasting of future Airbnb market in Singapore."),
                 br(),
                 p(strong("Objectives")),
                   br(),
                 p("This Shiny app is designed to provide customers with information additional to filters 
                 on Airbnb website to help them decide where to live and potential factors that they can consider to optimize their
                 Airbnb experiences in Singapore.")),
          
                 column(6,
 br(),
p(strong("Methods")),
p(strong("1. Data Source:"),"the open-source dataset was obtained from Kaggle, named “Singapore Airbnb.” The original source is http://insideairbnb.com/get-the-data.html. 
The data was collected on August 18, 2019. Variables in this dataset include room name, room ID, host name, host ID, neighborhood group (region), neighborhood (subregion),
latitude, longitude, room type (entire home/apt, private room, shared room), price (S$ per night), minimum nights of stay, availability in 365 days, number of reviews, last 
review date, reviews per month, and calculated host list count (total listings per host."),
br(),
p(strong("2. Data Cleaning & Exploration:")," We first identified missing values for each variable in the dataset. We then explored the data with plots to examine the distribution of variables 
of interest and made appropriate transformation accordingly."),
br(),
p(strong("3. Data Analysis & Visualization:"),"We first summarized descriptive statistics of neighborhood group distribution, price, minimum nights of stay, availability per 365 days, number of
review, and listings per host for each room type. We also visualized the spatial distribution of all available listings in Singapore by data collection date to allow customers to identify availability
in the region of their interest with a rough sense of the price. As price and room type are usually one of the most considered factors, we particularly visualized price difference across five regions 
in Singapore and proportion of room type among available listings for easier comparison."),
p(strong("4. Shiny App Design:"),"We use Shiny’s image and plot interaction features to provide users direct interative experience.By filtering the neighbourhood, price and room type requirements, the
         plots and the table present users general information of listing hosts that satisfy those requirements and represent the specific price of the rentals."),
p(strong("5. Model:"),"In order to model the pricing, we took log transformation of price. We choose xgboost model. XGBoost is an implementation of gradient boosted decision trees designed for speed and performance.Boosting is an ensemble technique where new models are added to correct the errors made by existing models. Models are added sequentially until no further improvements can be made.Gradient boosting is an approach where new models are created that predict the residuals or errors of prior models and then added together to make the final prediction."),
p(strong("6. Video:")," We made a short screencast to introduce our project and the functionality of this Shiny app. Click here to watch!"),
)),
             
               fluidRow(
                 column(6, img(src = "https://cdn.wccftech.com/wp-content/uploads/2019/10/airbnb-678x381.png", 
                               height = "500px", width = "600px", align = "center")),
               column(6, img(src = "https://media.tacdn.com/media/attractions-splice-spp-674x446/07/3d/9f/2d.jpg", 
                             height = "500px", width = "600px", align = "center")))
             )),
       tabPanel("Data Visulization",
                fluidRow( 
                 br(),
                 p("Singapore is an island country off the southern tip of the Malay Peninsula in Southeast Asia. It has been Southeast Asia's most modern city for over a century. The city blends Malay, Chinese, Arab, Indian and English cultures and religions. Its unique ethnic tapestry affords visitors a wide array of sightseeing and culinary opportunities from which to choose. A full calendar of traditional festivals and holidays celebrated throughout the year adds to its cultural appeal. Located at the tip of the Malay Peninsula, Singapore's tropical climate welcomes both leisure and business travelers year round. 
 Singapore is divided into five regions for developments, namely, the Central, West, North, North-East and East regions:"),
                   br(),
p(strong("Central Region:"), "Singapore’s main prime metropolitan area, of which owning any private residential property here would consider one as a ‘high net-worth individual’. This is largely due to its locational attributes, its close proximity to the central business district (CBD), the quality of the properties, as well as the high property values as compared to the rest of the regions."),
br(),
p(strong("West Region:"), "the largest region in terms of land area and is the second most populous region after the Central Region. It is the home to a majority of Singapore's heavy industries, mainly the petrochemical industry."),
br(),
p(strong("North Region:"), "the second largest region in terms of land area. The region is home to several attractions, namely the Singapore Zoo, Night Safari and River Safari."),
br(),
p(strong("North-East Region:"), "the most densely populated region. Located within the North-Eastern Islands planning area, Pulau Ubin is a popular tourist attraction with both local and foreign visitors visiting the island as it is one of the last rural areas in Singapore, with an abundance of natural flora and fauna."),
br(),
p(strong("East Region:"), "the 2nd most densely populated among the five, and has the smallest land area. Manufacturing makes up majority of the economical activity in the region."),

fluidRow(
  tableOutput("table1")
),

fluidRow(
  p("Most listings are located in Central region where major commercial zones and tourism attractions are. 90% of entire home/apt type is located in the Central. If you want to live 
  close to North or North-East region to explore natural landscape, you may want to consider private room for more choices.
"),
br(),
p("The average price is S$227/night for entire/home/apt, S$111/night for private room, and S$66/night for shared room. Private room type requires at least 21 days of stay, the longest among three
  types. Shared room has a much shorter minimum stay of 4 days. If you plan a longer than half-month travel, entire home/apt would be better choices for privacy and comfort during stay, depending on budget."),
br(),
p("Entire home/apt has most availability in a year, but all room types has availability up to over 100 days. In particular, entire home/apt and private room have over 200 days availability.
")
),

fluidRow(
  tableOutput("table2")
),
                 fluidRow(
                 column(6,plotOutput("map1"),p("Most listings concentrate in the Central region, very few in the North region. Most expensive ones are also in these two regions. North-East and East regions have similar price and count distribution, most of which cost less than S$5,000.
")),
                  column(6,plotOutput("map2"))
                ),
                
                fluidRow( 
                  column(6,plotOutput("box1")),
                  column(6,plotOutput("pie1"))
                  ),
                fluidRow(
                  p("Based on the most updated statistics, Airbnb price in the Central is the highest, which makes sense as most commercial and entertainment activities are concentrated in that region. It’s interesting that prices of North and North-East region is the cheapest, while 
                    safari, zoos, and other tourist attractions are located. So if you are more interested in natural landscape than modern entertainment, you may want to consider a rental in the North/North-East region for vicinity to places you want to go and CHEAPER PRICE.")
                ),

                fluidRow(
                  column(4,plotOutput("scatter1")),
                  column(4,plotOutput("scatter2")),
                  column(4,plotOutput("scatter3"))
                ),
                fluidRow(
                p("Obviously, shared room is in general the cheapest, and entire home/apt is the most expensive type. For entire home/apt, the price tends to be cheaper with more number of reviews, listings per host, or availability in 365 days; but reverse for private room. Shared room
                  type has limited availability, so these factors may not play a role in its pricing. If you want to save budget, choose shared room regardlessly.")
),
                )),
  
       tabPanel( "Find Your Ideal Place",
  
       fluidRow(
    titlePanel(
      "Find Your Ideal Place"
    ),
    sidebarLayout(
      sidebarPanel(
          p("This app uses Shiny’s image and plot interaction 
          features to provide users direct interative experience. Through 
          this shiny app, we help airbnb users to explore following questions:"),
br(),
p("1) What are the listings distribution that satisfy the location and budget requirements?"),
br(),
p("2) What are hosts who provide the house with most ideal room type, affordable price and good neiboorhood?"),
br(),
p("3) Detailed information for hosts that provide most ideal listing price (number of reviews, number of listings...) 
Users could customize their preference and search for listings much easier using this Shiny app."),
          

        selectInput("Region", label = "Select a Region",
                    choices = as.list(levels(listings$neighbourhood_group))),
        selectInput("Neighbourhood", label = "Select a neighbourhood",
                    choices = as.list(levels(listings$neighbourhood))),
        sliderInput("Price", "Select a Maximum Price:", 1, 500, 200),
        selectInput("Type", label = "Select a Room Type",
                    choices = as.list(levels(listings$room_type)))
        
      ),
      mainPanel(
        column(6, plotOutput("scatterPlot1")),
        column(6, plotOutput("Plot2",click = "plot1_click",brush = brushOpts(
          id = "plot1_brush")),
          htmlOutput("select_price")),
          fluidRow(
          tableOutput("table"))))
        
    )),
    tabPanel("Prediction",
             sidebarLayout(
               sidebarPanel(

                 p("In order to model the pricing, we took log transformation of price. We choose xgboost model. XGBoost is an
                     implementation of gradient boosted decision trees designed for speed and performance.Boosting is an ensemble
                     technique where new models are added to correct the errors made by existing models.
                     Models are added sequentially until no further improvements can be made.Gradient boosting is an approach where new models
                     are created that predict the residuals or errors of prior models and then added together to make the final prediction. 
                     It is called gradient boosting because it uses a gradient descent algorithm to minimize the loss when adding new models.
                    After grid search to tune parameters, we find the best parameter which have both low train error and test error, and the importance feature are shown from the graph.
                    The most important features are room tpye, latitude, availability, minimum_night, longitute, number_of_review and neighborhood.")),
               
               # Adding br() will add space between the text above and the dropdown
               # menu below
               #r()),
               mainPanel(
                 plotOutput("hist") )))
    ))


server <- function(input, output){
  
    output$scatterPlot1 = renderPlot({
      listings%>% filter(neighbourhood_group %in% input$Region & price < input$Price)%>% 
        ggplot(aes(latitude, longitude, color=neighbourhood, size=price)) +
        geom_point(alpha = 0.3) +scale_colour_brewer( palette="RdYlGn")+
        xlab("Latitude") +
        ylab("Longitude")
    })
    output$Plot2 = renderPlot({
       listings%>% filter(neighbourhood %in% input$Neighbourhood & price < input$Price & room_type %in% input$Type)%>% 
        ggplot(aes(host_name,price,fill="cond")) + scale_fill_brewer(palette="RdYlGn")+
        coord_flip()+
        geom_bar(stat = "identity") +
        theme_classic()+
        xlab("")

    })
    
    output$map1 = renderPlot({
    li %>%
      filter(Latitude, Longitude, `Neighbourhood Group`== `Neighbourhood Group` & !is.na(`Neighbourhood Group`)) %>%
      ggplot(aes(Longitude, Latitude, size=Price)) +
      geom_point(aes(color=`Neighbourhood Group`), alpha=0.5) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "white")) + 
      scale_colour_brewer("Neighborhood Group", palette="RdYlGn")
    })
    
    output$box1 = renderPlot({
      li %>%
        ggplot(aes(`Neighbourhood Group`, Log_Price, fill = `Neighbourhood Group`)) + 
        geom_boxplot(alpha=0.7) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "white")) + 
        scale_fill_brewer("Neighborhood Group", palette="RdYlGn")
    })
 
    output$map2 = renderPlot({   
    ima <- image_read("https://raw.githubusercontent.com/JIngyichen0701/Group_Final_Project/master/sin.png")
    par(xpd = F, mar = par()$mar + c(0,0,0,7))
    plot(x=c(103.6,104),y=c(1.244,1.455),type='n', xlab="Longitude", ylab="Latitude", box=FALSE)
    lim <- par()
    rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    grid() 
    points(li[li$`Neighbourhood Group`=='Central Region',]$Longitude, li[li$`Neighbourhood Group`=='Central Region',]$Latitude,cex=0.4,pch=19,col='firebrick3')
    
    points(li[li$`Neighbourhood Group`=='North Region',]$Longitude, li[li$`Neighbourhood Group`=='North Region',]$Latitude,cex=0.4,pch=19,col='khaki1')
    
    points(li[li$`Neighbourhood Group`=='North-East Region',]$Longitude, li[li$`Neighbourhood Group`=='North-East Region',]$Latitude,cex=0.4,pch=19,col='olivedrab3')
    
    points(li[li$`Neighbourhood Group`=='East Region',]$Longitude, li[li$`Neighbourhood Group`=='East Region',]$Latitude,cex=0.4,pch=19,col='sandybrown')
    
    points(li[li$`Neighbourhood Group`=='West Region',]$Longitude, li[li$`Neighbourhood Group`=='West Region',]$Latitude,cex=0.4,pch=19,col='mediumseagreen')
    
    legend(x="right",
           c("Central Region","North Region","North-East Region","East Region", "West Region"), 
           col=c("firebrick3","khaki1","olivedrab3","sandybrown","mediumseagreen"), 
           pch=19,bty="n",merge=FALSE, inset=c(-0.25,0), xpd = TRUE, title="Neighbourhood Group")
    })
    
    output$pie1 = renderPlot({
      a<-t(table(li$`Room Type`))
      value<-as.data.frame(a)
      
      blank_theme <- theme_minimal()+
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=14, face="bold")
        )
      
      value %>% ggplot(aes(x="", y=Freq, fill=Var2)) +
        geom_bar(width = 1, stat = "identity") + 
        coord_polar("y", start=0) + 
        guides(fill=guide_legend(title="room type")) +
        scale_fill_brewer(palette="RdYlGn") + 
        blank_theme + 
        theme(axis.text.x=element_blank()) +
        geom_text(aes(y = c(Freq), 
                      label = percent(Freq/7907)), size=5, position = position_dodge(2), hjust=0.5)
    })
    
    output$scatter1 = renderPlot({
      li %>%
        ggplot(aes(`Number of Reviews`, Log_Price, color = `Room Type`)) + 
        geom_point(alpha=0.7) +
        scale_colour_brewer("Room Type", palette="RdYlGn") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "white")) + 
        scale_fill_brewer("Neighborhood Group", palette="RdYlGn")
    })
    
    
    output$scatter2 = renderPlot({
      li %>%
        ggplot(aes(`Listings per Host`, Log_Price, color = `Room Type`)) + 
        geom_point() + 
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "white")) + 
        scale_colour_brewer("Room Type", palette="RdYlGn") 
    })
    
    output$scatter3 = renderPlot({
      li %>%
        ggplot(aes(`Availability per 365`, Log_Price, color = `Room Type`)) + 
        geom_point() +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.background = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "white")) + scale_colour_brewer("Room Type", palette="RdYlGn")
    })
    
    output$table1 = renderTable({
      summary1 <- li %>% select(`Neighbourhood Group`, `Room Type`)
      
      control1 <- tableby.control(
        test = F,
        total = T,
        numeric.stats = c("meansd", "medianq1q3", "range"),
        cat.stats = "countpct",
        stats.labels = list(
          meansd = "Mean (SD)",
          medianq1q3 = "Median (Q1, Q3)",
          range = "Min - Max"
        )
      )
      
      
      table1 <- tableby(summary1$`Room Type` ~ .,
                        data = summary1,
                        control = control1,
      )
      
      summary(table1,
              title = " "
      )
      
    })
    
    output$table2= renderTable({
      summary2 <- li %>% select(`Room Type`, Price, `Minimum Nights`, `Availability per 365`, `Number of Reviews`, `Listings per Host`)
      
      control2 <- tableby.control(
        test = F,
        total = T,
        numeric.stats = c("meansd", "medianq1q3", "range"),
        cat.stats = "count",
        stats.labels = list(
          meansd = "Mean (SD)",
          medianq1q3 = "Median (Q1, Q3)",
          range = "Min - Max"
        )
      )
      
      
      table2 <- tableby(summary2$`Room Type` ~ .,
                        data = summary2,
                        control = control2,
                        digits=0
      )
      
      summary(table2,
              title = " "
      )
    })
    
    # Print the name of the x value
    output$select_price <- renderText({
      if (is.null(input$plot1_click$x)) return("")
      else {
        name <- round(input$plot1_click$x)
        HTML("The Price You've selected <code>", name, "</code>",
             "<br><br>Here are the first 5 hosts that ",
             "match that category:")
      }
    })
    
    # Print the name of the x value
    output$table <- renderTable({
        keeprows<- round(input$plot1_click$x) == as.numeric(listings$price)
        head(listings2[keeprows, ], 5,)
        })
    output$hist <- renderPlot({
      
      barplot(c(0.33,0.195,0.108,0.0917,0.0615,0.0605,0.0596,0.0294),
              main = "Feature importance from xgboost model",
              xlab = "Features",
              ylab = "Relative Importance",
              ylim = c(0, 0.35),
              names.arg = c("Private Room","Shared Room","Latitude","Availability per 365","Listings per Host",
                            "Minimum Nights","Longitude","Number of Reviews"),
              col = "darkred",cex.names=0.9,angle = 45
      )
      
    })

 }
  shinyApp(ui = ui, server = server)
  
  