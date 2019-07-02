

# Uses Table 2 currently generated from model_chart.R 
library(plotly)
library(shiny)
library(ggplot2)
library(readxl)

data = readxl::read_excel("model_data.xlsx")

# Table with Main data, just department, num of models, emplyment total, and latest 2019 DEL
table1 = data %>%
  select("Department" = "Department...1", "Models" = "Number of business critical models", "Total", "DEL" = "2018-19...18")

#Table 2 for use with shiny

table2 = data %>%
  dplyr::select("Department" = "Department...1", 
                "Models" = "Number of business critical models", 
                "DEL" = "2018-19...18",
                c(4:9), 
                "Total")

ui <- fluidPage(navbarPage(title = "My Application",
            tabPanel("Description", mainPanel(fluidRow(includeMarkdown("sources.Rmd")))),
            tabPanel("Modelling Departments", 
              sidebarLayout(
                sidebarPanel(
                      selectInput("select",
                      h3("Select Profession"), 
                      choices = list("Total Number of Employees" = "Total", "Economists" = "Economics",
                                     "Statisticians" = "Statistics", "Finance" = "Finance", 
                                     "Digital, Data and Technology" = "Digital, Data and Technology",
                                     "Operational Delivery" = "Operational Delivery", "Operational Research" = "Operational Research"), selected = "Total"),
                      checkboxInput("checkbox", "LOG", value = TRUE),
                      h5("Note: No Data available for DfT disaggregated currently")
                      ),
                mainPanel(br(),
                         plotlyOutput("model_plot")
                          )
                           )    
                     ), 
            
            tabPanel(
                     "Distribution of Models by Department", h3("titlsssse"),
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
    
    role = input$select
    model = ggplot(table2, aes(x = get(role), y= Models, label = Department)) + geom_point(alpha = 0.5, colour = "red", aes(size= DEL))+ 
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_size_continuous(range = c(2, 15)) +
      scale_colour_continuous(guide = FALSE) + xlab("Number of Employed") + ggtitle(paste("Number of Models vs ", role)) + scale_x_log10()
   
     } else {
      
       role = input$select
       model = ggplot(table2, aes(x = get(role), y= Models, label = Department)) + geom_point(alpha = 0.5, colour = "red", aes(size= DEL))+ 
         theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_size_continuous(range = c(2, 15)) +
         scale_colour_continuous(guide = FALSE) + xlab("Number of Employed") + ggtitle(paste("Number of Models vs ", role))  
       ggplotly(model, tooltip = c("Department", "DEL")) 
      
    }
  })
  
  
  
  
  output$pie_chart = renderPlot({
    if (input$select2 == "Pie") {
    
    pal <- colorRampPalette(c("blue", "red"))
    h = pal(12)
      ggplot(table1, aes(x = 2, y = Models, fill = reorder(Department, Models))) +
      geom_bar(stat = "identity") + coord_polar("y", start=0) + 
      geom_text(aes(label = Department), position = position_stack(vjust = 0.5)) +
      labs(x = NULL, y = NULL, fill = NULL, title = "Models by Department") + scale_fill_manual(values = h) + 
      theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#666666")) + xlim(0.5, 2.5)  
    } else {
        
      table1 %>% 
        ggplot(aes(x = reorder(Department, Models), y = Models)) + geom_col(position = position_stack(reverse = TRUE),  fill="#FF9999", colour="black") +
        coord_flip() + theme_minimal() + xlab("Department") + ylab("Percentage of Models") +  theme(axis.line = element_blank(), axis.ticks = element_blank())
                                                                    
    }
    
  })
  
  
}

shinyApp(ui= ui, server = server)
