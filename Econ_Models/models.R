

# Uses Table 2 currently generated from model_chart.R 
library(plotly)
library(dplyr)
library(magrittr)
library(shiny)
library(ggplot2)
library(RColorBrewer)

data = readxl::read_excel("model_data.xlsx")

table2 = data %>%
  dplyr::select("Department" = "Department...1", 
                "Models" = "Number of business critical models", 
                "DEL_Nominal" = "2018-19...40",
                "AME_Nominal" = "2018-19...48",
                "DEL_Real" = "2018-19...56",
                "AME_Real" = "2018-19...64",
                "Economists" =  "Economics",
                "Statisticians" = "Statistics",
                "Digital_Data_Technology" =  "Digital, Data and Technology" ,
                "Operational_Research" = "Operational Research" ,
                "Science_Engineering" = "Science and Engineering",
                "Social_Research" = "Social Research"
  ) %>% 
  mutate(Total = Economists + Statisticians + Digital_Data_Technology + Operational_Research + Science_Engineering + Social_Research)


ui <- fluidPage(navbarPage(title = "My Application",
            tabPanel("Description", mainPanel(fluidRow(includeMarkdown("sources.Rmd")))),
            tabPanel("Modelling Departments", 
              sidebarLayout(
                sidebarPanel(
                      selectInput("select",
                      h3("Select Profession"), 
                      choices = list("Government Analysts" = "Total",
                                     "Economists" = "Economists",
                                     "Statisticians" = "Statisticians",
                                     "Digital, Data and Technology" = "Digital_Data_Technology",
                                     "Operational Research" = "Operational_Research",
                                     "Science and Engineering" = "Science_Engineering",
                                     "Social_Research" = "Social Research"), selected = "Total"),
                      checkboxInput("checkbox", "Log", value = TRUE),
                      radioButtons("ame", h3("Radio buttons"), 
                                   choices = list("Real DEL" = "DEL_Real", 
                                                  "Real AME" = "AME_Real",
                                                  "Nominal DEL" = "DEL_Nominal",
                                                  "Nominal AME" = "AME_Nominal")),
                      h5("Note: No Data available for DfT currently")
                      ),
                mainPanel(br(),
                         plotlyOutput("model_plot")
                          )
                           )    
                     ), 
            
            tabPanel(
                     "Distribution of Models by Department",
              sidebarLayout(
                sidebarPanel(
                       selectInput("select2",
                                   h3("Select Graph"),
                                   choices = list("Pie" = "Pie", "Bar" = "Bar"), selected = "Pie")
                              ),
                mainPanel(
                  shiny::plotOutput("pie_chart")
                
              )
                )
              
                     ) # end of second panel
    
    
         ) # navbarPage end
  ) #fluidPage end

server = function(input, output) {
  
  output$model_plot = renderPlotly({
    if (input$checkbox) {
    
    del = input$ame
    role = input$select
    model = ggplot(table2, aes(x = get(role), y= Models, label = Department)) + geom_point(alpha = 0.5, colour = "red", aes(size= get(del)))+ 
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_size_continuous(range = c(2, 15)) +
      scale_colour_continuous(guide = FALSE) + xlab("Number of Employed") + ggtitle(paste("Percentage of Models vs ", role)) + scale_x_log10()
   
     } else {
      
       del = input$ame
       role = input$select
       model = ggplot(table2, aes(x = get(role), y= Models, label = Department)) + geom_point(alpha = 0.5, colour = "red", aes(size= get(del)))+ 
         theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_size_continuous(range = c(2, 15)) +
         scale_colour_continuous(guide = FALSE) + xlab("Number of Employed") + ggtitle(paste("Percentage of Models vs ", role))  
       ggplotly(model, tooltip = c("Department")) 
      
    }
  })
  
  
  
  
  output$pie_chart = renderPlot({
    if (input$select2 == "Pie") {
    
    pal <- colorRampPalette(c("blue", "red"))
    h = pal(12)
      ggplot(table2, aes(x = 2, y = Models, fill = reorder(Department, Models))) +
      geom_bar(stat = "identity") + coord_polar("y", start=0) + 
      geom_text(aes(label = Department), position = position_stack(vjust = 0.5)) +
      labs(x = NULL, y = NULL, fill = NULL, title = "Models by Department") + scale_fill_manual(values = h) + 
      theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#666666")) + xlim(0.5, 2.5)  
    } else {
        
      table2 %>% 
        ggplot(aes(x = reorder(Department, Models), y = Models)) + geom_col(position = position_stack(reverse = TRUE),  fill="#FF9999", colour="black") +
        coord_flip() + theme_minimal() + xlab("Department") + ylab("Percentage of Models") +  theme(axis.line = element_blank(), axis.ticks = element_blank())
                                                                    
    }
    
  })
  
  
}

shinyApp(ui= ui, server = server)
