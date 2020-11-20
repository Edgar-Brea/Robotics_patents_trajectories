library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(tidyr)


# Import patents data csv file and tidy up:
data <- read.csv("robotics_patents_2009_2019.csv", stringsAsFactors = F)
data$ipc_section <- paste(data$ipc_section, data$ipc_section_name, sep = " - ")
data$ipc_class <- paste(data$ipc_class, data$ipc_class_name, sep = " - ")
data$ipc_subclass <- paste(data$ipc_subclass, data$ipc_subclass_name, sep = " - ")
data$ipc_maingroup <- paste(data$ipc_maingroup, data$ipc_maingroup_name, sep = " - ")
data <- data[, names(data) %in% c("patent_id", "pub_date", "ipc_section", "ipc_class", "ipc_subclass", "ipc_maingroup")]




ui <- fluidPage(
    h1(id="big-heading", "Patent classification of robotics-related patents over time"),
    tags$style(HTML("#big-heading{color: black; font-size: 35px; text-align: center;}")),
    
    h2(id="small-heading", "Data from the PATENTSCOPE database (WIPO)"),
    tags$style(HTML("#small-heading{color: black; font-size: 24px; text-align: center;}")),
    #titlePanel(HTML("<h1><center><font size: 18> Patent classification of robotics-related patents over time </font></center></h1>"), windowTitle = "Patent classification of robotics-related patents over time"),
    #titlePanel(HTML("<h2><center><font size: 6> Data from the WIPO PATENTSCOPE database </font></center></h2>")),
    tabsetPanel(
        tabPanel("Heatmap", plotOutput("plot", width = "100%", height = "500px")),
        tabPanel("Table", DT::DTOutput("table"))
    ),
    hr(),
    fluidRow(
        column(3,
               selectInput("ipc_code_level", "Level of IPC code hierarchy:", c("ipc_maingroup", "ipc_subclass", "ipc_class"))),
        column(4, offset = 1,
               sliderInput("top_x", "No. of IPC codes used for quarterly ranking:", 5, 20, 15, step = 1)),
        column(3,
               dateRangeInput("dates", "Patent's publication date range:", start = as.Date(min(data$pub_date), format = "%d.%m.%Y"), end = as.Date(max(data$pub_date), format = "%d.%m.%Y")))
        )
)




server <- function(input, output) {
    resulting_data <- reactive({
        #filter data by user's date choice:
        data <- data %>%
            filter(as.Date(pub_date, format = "%d.%m.%Y") >= input$dates[1] & as.Date(pub_date, format = "%d.%m.%Y") <= input$dates[2])
        #change dates to quarter of year:
        data <- data %>% mutate(period = case_when( #create new column with period (e.g. Q3.2013)
            as.numeric(substr(pub_date,4,5)) < 4 ~ paste0(substr(pub_date,7,10), ".Q1"),
            as.numeric(substr(pub_date,4,5)) >= 4 & as.numeric(substr(pub_date,4,5)) < 7 ~ paste0(substr(pub_date,7,10), ".Q2"),
            as.numeric(substr(pub_date,4,5)) >= 7 & as.numeric(substr(pub_date,4,5)) < 10 ~ paste0(substr(pub_date,7,10), ".Q3"),
            as.numeric(substr(pub_date,4,5)) >= 10 ~ paste0(substr(pub_date,7,10), ".Q4")
        ))
        #aggregate all patents by ipc_codes, with the level chosen by the user:
        data_aggregated <- data %>%
            group_by(period, !!as.name(input$ipc_code_level)) %>%
            tally()
        names(data_aggregated)[2:3] <- c("ipc_code", "no_patents")
        #rank by period and get top x, with x chosen by the user:
        data_aggregated_top_x <- data_aggregated %>%
            group_by(period) %>%
            mutate(rank = dense_rank(desc(no_patents))) %>% #dense_rank leaves no gaps across ranks when there are ties
            top_n(input$top_x, -rank) # select top x
    })
    
    output$plot <- renderPlot({
        #prepare data for plot:
        data_to_plot <- resulting_data() %>%
            arrange(period) #sort by period
        
        data_to_plot <- data_to_plot[,-3]
        data_to_plot$rank <- as.factor(data_to_plot$rank)
        data_to_plot <- data_to_plot %>%
            complete(period, ipc_code, fill = list(rank = NA)) #complete with NAs periods where the IPC was not in the top x
        data_to_plot <- data.frame(data_to_plot)
        
        #truncate or pad all ipc names to avoind plot shrinking (nchar = 100)
        data_to_plot$ipc_code_modified <- ifelse(nchar(data_to_plot$ipc_code) > 100, 
                                                  stringr::str_trunc(data_to_plot$ipc_code, 100, "right"), 
                                                  stringr::str_pad(data_to_plot$ipc_code, 100, "right"))
        
        myPalette <- colorRampPalette(rev(brewer.pal(7, "Reds"))) #creating the palette
        
        #plot:
        ggplot(data_to_plot,aes(x = period,y = ipc_code_modified, fill = rank)) +
            geom_tile(colour = "grey60", size = .05) +
            guides(fill=guide_legend(title="Rankings (period)")) +
            scale_y_discrete(position = "right") +
            scale_fill_manual(values = myPalette(input$top_x), na.value = "white", labels = c(as.character(seq(1:input$top_x)), paste0("not in top ", input$top_x))) +
            theme(legend.position = "left",
                  axis.text.y = element_text(size = 10),
                  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.title = element_text(size = 18, face = "bold", hjust = -0.2),
                  panel.background = element_blank(), #element_rect(colour = "grey75", size = 0.05),
                  panel.grid = element_blank(),
                  legend.key.size = unit(0.6, "cm"),
                  legend.key.width = unit(0.4, "cm"),
                  legend.title = element_text(size = 11),
                  legend.text = element_text(size = 11),
                  plot.margin = margin(0.3, 0, 0, 0, "cm"))
    })
    
    output$table <- DT::renderDT(filter = "top", rownames = F, {
        data_to_show <- resulting_data()
    })
}




shinyApp(ui = ui, server = server)


