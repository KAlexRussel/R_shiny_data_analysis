
#GRoup1 Email Address
#kouawoualex1234@email.com
#honfoemmanuel1@gmail.com 
#agossoumichely@gmail.com 
#cindytchango01@gmail.com


library(shiny)
library(shinythemes)
library(foreign)
library(ggplot2)
library(magrittr, include.only = c("%<>%","%$%"))
library(DT)


pdata2 <- read.dta("ap1 .dta")

pdata1 <- read.dta("b .dta")

data_m <- merge(pdata1,pdata2, by="menage")

fdata <- na.omit(data_m)

donnees <- data.frame(
  Name = c("Kouawou Alex-Russel"),
  Email = c("kouawoualex1234@email.com")#name and eamail of memebers 
)
donnees2 <- data.frame(
  Variables = c("b01", "b02", "b03", "b04","b05","b06","b07","b08","b09","b10","b11","b12"),
  Label = c("Walls Materials", "Flooring Materials", "Roof materials", 
            "Housing Status","Housing payment","Source of water",
            "Distance to water","ligthing Source","Distance to telephone services",
            "Distance to passable road","Distance to paved road ","DIstance to sous_prefecture")#datas for the variables and thier label
)
# Define UI for application that draws a histogram
ui <- fluidPage( 
  tags$head(
    tags$style(
      HTML(
        "
        .flex-container{
        display: flex;
        align-item:center;
        gap:20px
        }
        .flex-item{
        margin:10px;
        width:50%;
        }
        "
        
      )
    )#css code for styling
  ),
    
  theme = shinytheme("flatly"), # themes use in the ui
  
  navbarPage(#create a navbar
    "Benin farm Survey ",#title of app
    tabPanel("General Overview",icon = icon("house"),
             
             h2("Presented By"),
             br(),
             tableOutput("tableau"),
             h2("Benin Small Farmer Survey"),
             br(),
             p("This survey was conducted over the period August to November 1998,
             it aimed to describe rural livelihoods, track changes in conditions 
             for farmers over time,and assess the impact of agricultural reforms 
              on rural households.
              Our work is more focus on the household characteristics. for that,
              we extracted from farmer survey the data on  household characteristics.
              The question which we want to answer is how the housing characteristic 
              determines his cost. Descriptive analyses were used to describe the
              differents variables. Furthermore, we estimate a linear regression 
              model to show the relation between housing characteristic and the
              cost.
               "),
             
             h3("Hypothesis"),
             br(),
             p("The characteristics of the household affect the monthly payment"),
             
            
             
    ),
    tabPanel(" Data Overview ", icon = icon("database"), 
             verbatimTextOutput("overviewText"),
             actionButton("showDataBtn", "Data Import", style = "width: 100%; height: 100%;"),
             DTOutput("table")),
  
   
    tabPanel("Histogram",icon=icon("bar-chart"), #fist link on the navbar
             h3("A Histogram showing the Monthly payment for housing"),
             br(),
             p("This visual representation of the frequency distribution of payment amounts, shows that most houses are rent at 2000 and there somme ouliers where 
                some houselod rent at 30,000 and above, we can see cleraly the distribution on the plot below.
               ."),
             br(),
             br(),
            
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "bins",
                             label ="Number of bins", #label of the cart
                             min= 0, #minimum value of bins
                             max= 40, #maximum value of bins
                             value = 30, #the default bins 
                             step = 2), #interval of input 
                 sliderInput(inputId = "memb1",
                             label ="Regulate Prices", #label of the cart
                             min= 0, #minimum value of bins
                             max= 42000, #maximum value of bins
                             value = 42000),  
                 
              
                
                 HTML("<p style='color: black;'><b>Maximum value:</b></p>"),
                 textOutput("txtout3"),
                 HTML("<p style='color: black;'><b>Minimum value:</b></p>"),
                 textOutput("txtout4"),
                 HTML("<p style='color: black;'><b>Average value:</b></p>"),
                 textOutput("txtout5")
                
                 
               ),
               mainPanel(
                 plotOutput(outputId = "hist") 
               
                 
                 #a function which display graphs takes in parementer the outpur id displut which is a histogram
               ) #mainPanel
               
             )#siderbar layout
             
    ), #Navbar 1, tabPanel
    tabPanel("Scatter plot",icon=icon("dot-circle"), #fist link on the navbar
             h3("Scatter plot"),
             br(),
             p("This Scatter plot shows the corroletaion between different household charistics and the the monthly payment for housing "),
             br(),
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "varx",
                   label = "Select the X-axis Variable:",
                   choices = list(
                     "b05",
                     "b07","b09",
                     "b10","b11", "b12")),#creating a swtich to chabge the variable on the x- axis
                 
                 
                 selectInput(
                   inputId = "vary",
                   label = "Select the y-axis Variable",
                   choices = list(
                     "b05",
                     "b07","b09",
                     "b10","b11", "b12")),#creating a swtich to chabge the variable on the y- axis
                 
                 
                           ),#sidebarpanel
               mainPanel(
                 plotOutput(outputId = "scatter") 
                     ) #mainPanel
                             
                 ),#siderLayout
             br(),
             textOutput("wordList")
               ), #scatter plot, tabPanel
               
  
    tabPanel("Box Plot",icon = icon("th-large"),
            
             h3("A Box Plot showing the Number of individuals in a household"),
             br(),
             p("This box plot  visually summarizes the central
             tendency with the median line, highlights the spread with the interquartile range (IQR), 
             and identifies potential outliers beyond the whiskers, providing a concise overview of 
             the distribution and key statistics in the dataset.
               This graphical representation aids in understanding the variability and 
               typical values of monthly housing payments for effective data comparison and analysis."),
             br(),
             
             div( class="flex-container",
                  div(
                    plotOutput(outputId = "distplot3"),class="flex-item"
                  ),
                  div(
                    plotOutput(outputId = "distplot4"),
                    class="flex-item"
                  )
                  
             ),
          
             ),
    tabPanel("Qualitative analaysis" ,icon=icon("table"),
             
             h3("Here we have the differenet bar chart of some qualitative variables"),
             br(),
             p("
               This graphical representation aids in understanding the qualitative variables."),
            
             br(),
             br(),
             
             div( class="flex-container",
                  div(
                    plotOutput(outputId = "barchart1"),class="flex-item"
                  ),
                  div(
                    
                    plotOutput(outputId = "barchart2"),
                    class="flex-item"
                  ),
                  div(
                    
                    plotOutput(outputId = "barchart3"),
                    class="flex-item"
                  ))#use of flex box to display the graph horizontally
             
            
             ),
    tabPanel("Regressions",icon = icon("calculator"),
             
             h3("Below you can find the regression table"),
             br(),
             br(),
            
        div( class="flex-container",
             div(
              # tableOutput(outputId = "table1"),class="flex-item"
               verbatimTextOutput("table1")
             ),
             div(
               
               HTML("<h4 style='color: black;'><b>Explanation</b></h4>"),
               br(),
               p("- Several coefficients for different building materials (b01 to b03) have significant impacts on the outcome, with varying t-values and p-values."),
               p("- The coefficients for b04 (Rent, Borrowed, Used without pay) show significant effects on the outcome with corresponding t-values and p-values."),
  
               p("- Overall, the R-squared value for the model is 0.22, indicating the proportion of variance explained by the model."),
               
               p("The results of the analysis show that building materials 
                 have a significant effect on  Monthly payment for the housings."),
               br(),
               
               HTML("<h4 style='color: black;'><b>Conclusion</b></h4>"),
               br(),
               p(" In conclusion, houses with tulle roofs, tiled terraces and cement 
                 walls are more likely to be more expensive. However, the model is not 
                 predictive enough to estimate the  Monthly payment for the housing based
                 on its characteristics."),
               br(),
               tableOutput("tableau1"),
               class="flex-item"
             ),
             
            
           
            )
       
    )#siderbar layout
            
  )#navbarPage
  
)#FluidPAge

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  btn_status <- reactiveVal(FALSE)
  
  # observe the tap on the bouton "Import Data"
  observeEvent(input$showDataBtn, {
    # updtae the bouton
    btn_status(!btn_status())
  })
  
  # print the data base with "Import Data"
  output$overviewText <- renderText({
    if (btn_status()) {
      "Overview of the database"
    } else {
      ""
    }
  })
  
  # Print out or close the database "Import Data"
  output$table <- renderDT({
    if (btn_status()) {
      datatable(
        fdata,
        options = list(
          scrollX = TRUE,
          pageLength = 5  # print out  5 lines by page
        )
      )
    } else {
      NULL
    }
  })
  
  # Reactive expression for Descriptive Analysis variables
  stat_des_data <- reactive({
    if (btn_status()) {
      fdata
    } else {
      NULL
    }
  })
  
  output$stat_des <- renderPrint(
    as.data.frame(t(stat.desc(fdata[,c(3, 13, 15, 17, 18, 19, 20)])))
    
  )
  
  
  output$tableau <- renderTable({
    donnees
  })#print  a table
  output$tableau1 <- renderTable({
    donnees2
  })
  


  #render regression table
  output$table1 <- renderPrint(
    {
      fdata %>% 
      lm(b05 ~ b01 + b02 + b03 + b04 + b07 + b08 + b09 + b10 + b11 + b12, data = .) %>% 
      summary()
      ##summary(model)$coefficients
      #tabl_res <- data.frame("Coefficients" = names(coef(model)), "Estimate" = summary(model)$coefficients[,1], "Standard error" = summary(model)$coefficients[,2], "t value" = summary(model)$coefficients[,3], "P-value" = summary(model)$coefficients[,4],"R_Squared"=summary(model)$r.squared)
      #regression_results()
    }
  )
  #print some text
  output$txtout <- renderText({
    paste(input$txt1, input$txt2, sep = " ")
    
  })

  #print a histogramm with the mean 
  output$hist = renderPlot({#render a plot
    
    d <- fdata$b05
    d <- na.omit(d)
    x <- fdata$b05[fdata$b05 < input$memb1] # takes the value of the a2  variable in a the data1 dataset
    x <- na.omit(x) #omit missing values like ,dont consider missing values ,values in na
    bins <- seq(min(x), max(x), length.out= input$bins + 1 )
    
    output$txtout3 <- renderText({ max(d)})
    output$txtout4 <- renderText({ min(d)})
    output$txtout5 <- renderText({ mean(d)})
    
    fdatafil <- fdata[fdata$b05 < input$memb1,]
    fdatafil <- na.omit(fdatafil)
    
    mean_value <- mean(fdatafil)
    ggplot(fdatafil, aes(x = b05) )+
      geom_histogram(aes(y = ..density..), bins = input$bins, fill = "skyblue", color = "black", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      geom_vline(xintercept = mean_value, linetype = "dashed", color = "green", size = 1) +#mean
      annotate("text", x = mean_value, y = 0, vjust = -1, label = paste("Mean = ", round(mean_value, 2)), color = "green") +
      labs(title = "Histogram with Mean Value",
           x = "Values",
           y = "Density")
    
  } )
  values <- list(b01="Walls Materials",b02="Flooring Materials",b03="Roof materials",
                 b04="Housing Status",b05="Housing payment",b06="Source of water",
                 b07="Distance to water",b08="ligthing Source",b09="Distance to telephone services",
                 b10="Distance to passable road",b11="Distance to paved road ", b12="DIstance to sous_prefecture")
  
  # Render the formatted list
  output$wordList <- renderPrint({
    formatted_text <- paste(names(values), "=",  values, collapse = ", ")
    cat(formatted_text)
  })
  #render a scatter plot
  output$scatter = renderPlot({#render a plot
    x_col <- input$varx
    y_col <- input$vary
    ggplot(fdata, aes_string(x = x_col, y = y_col)) +
      geom_point(color = "#4CAF50", size = 4, alpha = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +#add the line of regression on the 
      labs(title = "Scatter Plot showing relation between all the variables ", x = x_col, y = y_col) +
      theme_minimal()
  })
  
  #boxplot
  output$distplot3 <- renderPlot({
   # x_cot = fdata[input$var4,]
    ggplot(fdata, aes(y =a2)) +
      geom_boxplot(fill = "#3366cc", color = "#3366cc", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Boxplot of members of a Household", y = "Number of Person in a household")
  })
  
  output$distplot4 <- renderPlot({
    p <- fdata[fdata$a2 < 20, ]
    ggplot(p, aes( y = a2)) +
      geom_boxplot(fill = "#D67A82", color = "#D67A82", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Boxplot of members of a Household less than 20", y = "Number of Person in a household") 
      
  })
    
  #render barcharts
 output$barchart1 <- renderPlot({
   barplot(prop.table(table(fdata$b01)), main = "Bar chart main materials for walls construction ", xlab = " Main construction material of walls", ylab = "frequence", ylim = c(0, 1), col = c(1, 2, 3, 4, 5, 6), legend.text = TRUE)
   text(x=1:length(prop.table(table(fdata$b01))), y = prop.table(table(fdata$b01)), labels = round(prop.table(table(fdata$b01)),2)*100, pos = 3, cex = 0.8, col = "black")
 })
 
 output$barchart2 <- renderPlot({
   barplot(prop.table(table(fdata$b02)), main = "Barchart Flooring material ", xlab = " Flooring material ", ylab = "frequence", ylim = c(0, 0.8), col = c(2, 3, 4, 5), legend.text = TRUE)
   text(x=1:length(prop.table(table(fdata$b02))), y = prop.table(table(fdata$b02)), labels = round(prop.table(table(fdata$b02)),2)*100, pos = 3, cex = 0.8, col = "black")
 })
 output$barchart3 <- renderPlot({
   barplot(prop.table(table(fdata$b03)), main = "Barchart  Roof material is made of ",, xlab = "  Roof material is made of ", ylab = "frequence", ylim = c(0, 0.8), col = c(2, 3, 4, 5), legend.text = TRUE)
   text(x=1:length(prop.table(table(fdata$b03))), y = prop.table(table(fdata$b03)), labels = round(prop.table(table(fdata$b03)),2)*100, pos = 3, cex = 1, col = "black")
 })
 
  
}#server

# Run the application 
shinyApp(ui = ui, server = server)
