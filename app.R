library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(rgdal)
library(plotly)
library(RColorBrewer)
library(DT)


dengue_data <- read.csv("cleaned_dengue_Data.csv")%>%
  as_tibble()

dengueData<-dengue_data%>%pivot_longer(cols=colnames(dengue_data)[3:14],
                                        names_to = "Month",
                                        values_to = "Cases")

dengueData$Month <-factor(dengueData$Month,levels = unique(dengueData$Month))

# Load Shapefiles
nepal_sh <- readOGR(dsn = "/home/riya/wd/Local Unit", layer = "local_unit", stringsAsFactors = FALSE)
nepal_sh_df <- fortify(nepal_sh, region = "DISTRICT")

# Merge Data
cases_df <- dengue_data[, c(2, 15)]
cases_df$District <- unique(nepal_sh_df$id)
colnames(cases_df) <- c("id", "Total.cases")
nepal_sh_df <- merge(nepal_sh_df, cases_df, by = "id")
mycolors <- colorRampPalette(brewer.pal(10, "RdYlBu"))(15)

# creating a function to plot lineplot
plot_lineplot <- function(data, province = "All", district) {
  if (!("All" %in% province)) {
    data <- data %>%
      filter(Province == province, District %in% district)
  }
  
  # if no remaining data, return NULL
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  ggplot(data, aes(x = Month, y = Cases, color = District, group = District)) +
    geom_line() +
    geom_point() +
    labs(title = stringr::str_glue("Dengue cases - {province} Province"),
         x = "Month",
         y = "Cases") +
    scale_fill_manual(values = mycolors) +
    theme_light()
}

ui <- navbarPage(
  shinyWidgets::useShinydashboard(),
  title = div(HTML("<b>Dengue 2023 Nepal Dashboard</b>")),
  tabPanel(
    title = "Home",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "select_province",
            label = "Select Province",
            choices = c("All", "Sudurpaschim", "Lumbini", "Gandaki", "Madhesh", "Bagmati", "Koshi", "Karnali"),
            selected = "Koshi",
            multiple = FALSE
          ),
          selectInput(
            inputId = "select_district",
            label = "Select District",
            choices = dengueData$District,
            selected = "Sunsari",
            multiple = TRUE
          ),
          HTML(rep("<br>", 30))
        ),
        mainPanel(
          fluidRow(
            tags$head(tags$style(HTML('.info-box {min-height: 80px;} .info-box-icon {height: 80px; line-height: 60px;}'))),
            infoBox(value = tags$p(sum(dengueData$Cases), style = "font-size: 110%;"), "Total cases", fill = TRUE, icon = tags$i(class = "fas fa-mosquito"), color = "light-blue"),
            infoBox(value = tags$p("77", style = "font-size: 110%;"), "Districts affected", fill = TRUE, icon = tags$i(class = "fas fa-map"), color = "blue"),
            infoBox(value = tags$p("20", style = "font-size: 110%;"), "Confirmed Deaths", fill = TRUE, icon = tags$i(class = "fas fa-heart-circle-xmark"), color = "purple")
          ),
          fluidRow(
            style = "border: 1px solid lightgrey; border-radius: 15px; padding: 10px;",
            br(),
            plotlyOutput('cases_per_month', height = "175px", width = "100%")
          ),
          br(),
          fluidRow(
            column(
              6,
                style = 'border: 1px solid lightgrey; border-radius: 15px;',
                br(),
                div(HTML('<b>Distribution of cases across Nepal</b> '), style = 'display: inline-block;'),
                br(), br(),
                plotlyOutput("choropleth_map", height = "300px", width = "100%")
              
            ),
            column(
              6,
              
                style = 'border: 1px solid lightgrey; border-radius: 15px; padding-left: 10px;',
                br(),
                div(HTML("<b>Province wise distribution</b>"), style = 'display: inline-block;'),
                br(), br(),
                plotOutput("province_trend", height = "300px", width = "100%")
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title="Data",
    fluidPage(
      mainPanel(
        fluidRow(
          dataTableOutput("data")
        )
      )
    )
  )
)



# Server
server <- function(input, output, session) {
  observe({
    provinces_data <- dengueData %>%
      filter(Province == input$select_province)
    districts <- unique(provinces_data$District)
    
    # If the selected district is not in the updated list, reset it to all districts
    if (!is.null(input$select_district) && any(!input$select_district %in% districts)) {
      updateSelectInput(session, "select_district", choices = districts, selected = NULL)
    } else {
      updateSelectInput(session, "select_district", choices = districts, selected = input$select_district)
    }
  })
  dengue_plot <- reactive({
    plot_lineplot(dengueData, province = input$select_province, district = input$select_district)
  })
  output$cases_per_month <-renderPlotly({
    ggplotly(dengue_plot())
  })
  
  output$province_trend <- renderPlot({
    prov_cases <-dengue_data%>%
      group_by(Province)%>%
      summarise(total=sum(Total))%>%
      mutate(per_burden = round((total/sum(total))*100))%>%
      arrange(desc(per_burden))
    
    ggplot(prov_cases,aes(x=Province,y=per_burden,fill=total))+
      geom_bar(stat="identity")+
      geom_text(aes(label = paste0(per_burden, "%")), vjust = -0.5, size = 3.5, color = "black") +
      labs(x="Province",
           y="National burden(%)")+
      theme_light()+theme(axis.title.x = element_text(size = 10,face="bold"),
                          axis.text.x = element_text(size = 10,angle=20),
                          axis.title.y = element_text(size = 10,face="bold"),
                          legend.position = "none")
    
    
  })
  
  output$choropleth_map <- renderPlotly({
    ggplotly(plot_choropleth(nepal_sh_df))
      
  })

  output$data<-renderDataTable({
    dengue_data
  })
}

shinyApp(ui, server)