## app.R ##
library(shinydashboard)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(forcats)
library(kernelboot)
library(mgcv)

ui <- dashboardPage(
  dashboardHeader(title = "The First 13 Weeks"),
  dashboardSidebar(fileInput("file1", "Choose weight data file (*.xlsx)", accept = ".xlsx"),
                   radioButtons("who_norm", "Display WHO norm data for:",
                                c("Female babies" = "female",
                                  "Male babies" = "male",
                                  "No norm data" = "none")),
                   hr(),
                   p("You can download a sample XLSX file to document the weight of your baby here.
                     This file can be opened with virtually spreadsheet application including MS Excel or LibreOffice Calc.
                     Keep it safe!"),
                   downloadButton("downloadDefaultData", "Download "),
                   hr(),
                   p("This app does not save any of your data, but just plots them.
                   The app is for information purposes only and not for diagnosis, talk to a qualified health practitioner!"),
                   p("Find the code here:")
                   ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fillRow(box(plotOutput("plot1"), width = "100%"),
    )
  )
)

server <- function(input, output) {
  output$downloadDefaultData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("sample_weight_data.xlsx")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      file.copy("default_weight_data.xlsx", file)
    })
  
  output$plot1 <- 
    renderPlot({
      file <- input$file1
      ext <- tools::file_ext(file$datapath)
      
      if(is.null(input$file1)){
        df <- read_xlsx("default_weight_data.xlsx")
      }
      else {
        req(file)
        validate(need(ext == "xlsx", "Please upload an xlsx file"))
        df <- read_xlsx(file$datapath)
      }
      
      df <- df %>% mutate(`Weight (in g)` = `Weight (in g)` + rnorm(dim(df)[1])) 
      df <- df %>% rename(Weight_in_g = `Weight (in g)`)
      df$nDateTime <- as.numeric(as.numeric(df$Date) + as.numeric(df$Time))
      df$DateTime <- as.POSIXct(df$nDateTime, origin = "1970-01-01")
      
      bm <- gam(Weight_in_g ~ s(nDateTime, bs = "tp", k = dim(df)[1]/2), data=df)
      
      #Once an hour
      df_expanded <- expand.grid(nDateTime = seq(min(df$nDateTime), max(df$nDateTime), 60*60))
      df_expanded$DateTime <- as.POSIXct(df_expanded$nDateTime, origin = "1970-01-01")
      predict_gam <- predict(bm, df_expanded, type = "link", se.fit = TRUE)
      
      df_expanded$Weight_in_g <- predict_gam$fit
      df_expanded$Weight_in_g_LCI <- predict_gam$fit - 2*predict_gam$se.fit
      df_expanded$Weight_in_g_HCI <- predict_gam$fit + 2*predict_gam$se.fit
      
      df_expanded <- df_expanded %>% mutate(Weight_diff_7_days_in_g = Weight_in_g - lag(Weight_in_g, 24*7))
      
      # Read in the WHO weight for age chart for girls
      if(input$who_norm != "none") {
        if(input$who_norm == "female") {
          df_WHO_nomogram_wfa_g <- read_xlsx("WHO_tab_wfa_girls_p_0_13.xlsx")
        }
        else if(input$who_norm == "male") {
          df_WHO_nomogram_wfa_g <- read_xlsx("WHO_tab_wfa_boys_p_0_13.xlsx")
        }
        
        colnames(df_WHO_nomogram_wfa_g) <- c("Week", "L", "M", "S",
                                             "0.1", "1", "3",  "5", "10", "15",
                                             "25",  "50", "75", "85", "90", "95",
                                             "97", "99", "99.9")
        
        df_WHO_nomogram_wfa_g_tidy <- pivot_longer(df_WHO_nomogram_wfa_g, cols = -c("Week", "L", "M", "S"), names_to = "WHO Percentile", values_to = "Weight_in_kg")
        df_WHO_nomogram_wfa_g_tidy <- df_WHO_nomogram_wfa_g_tidy %>% mutate(Weight_in_g = Weight_in_kg*1000)
        df_WHO_nomogram_wfa_g_tidy$`WHO Percentile` <- factor(df_WHO_nomogram_wfa_g_tidy$`WHO Percentile` ,
                                                              levels = c("0.1",  "1",  "3",  "5",  "10", "15",
                                                                         "25",   "50", "75", "85", "90", "95",
                                                                         "97",  "99", "99.9"))
        
        df_WHO_nomogram_wfa_g_tidy <- df_WHO_nomogram_wfa_g_tidy %>% mutate(nDateTime = min(df$nDateTime) + Week*7*24*60*60)
        df_WHO_nomogram_wfa_g_tidy$DateTime <- as.POSIXct(df_WHO_nomogram_wfa_g_tidy$nDateTime, origin = "1970-01-01")
      }
      
      dodge <- position_dodge(width=0.9)
      q <- ggplot(data=df, aes(x=DateTime, y = Weight_in_g))
      
      if(input$who_norm != "none") {
        q <- q + geom_smooth(data = df_WHO_nomogram_wfa_g_tidy, aes(x=DateTime, y = Weight_in_g, color = `WHO Percentile`, group = `WHO Percentile`),
                 method = "gam", formula = y~s(x, k=5), se=F, size=0.5)
      }
      
      q <- q +
        geom_ribbon(data=df_expanded, aes(ymin = Weight_in_g_LCI, ymax = Weight_in_g_HCI), fill = "grey70",
                    alpha = 0.5) +
        geom_line(data=df_expanded, linewidth = 0.75) +
        #geom_smooth(method = "gam", formula = y~s(x,k=9))+
        geom_point(alpha = 0.65, fill= "red",
                   shape = 21, size = 3, stroke = 1, color ="black",
                   position = dodge) +
        geom_line(data=df_expanded, aes(x=DateTime, y = 2000 + 4*Weight_diff_7_days_in_g), color = "red", linewidth = 0.75) +
        scale_y_continuous(sec.axis = sec_axis(~ (. - 2000)/4, name = "Seven-day weight change (g)")) +
        xlab("Time") +
        ylab("Weight (g)") + 
        theme_bw(base_size = 15) +
        coord_cartesian(ylim = c(max(2000, min(df$Weight_in_g) - 500), max(4500, max(df$Weight_in_g) + 500)), xlim = c(min(df_expanded$DateTime),
                                                                                       max(df_expanded$DateTime)+ 3*24*60*60))+
        theme(axis.line.y.right = element_line(color = "red"), 
              axis.ticks.y.right = element_line(color = "red"),
              axis.text.y.right = element_text(color = "red"), 
              axis.title.y.right = element_text(color = "red"),
              legend.position="top",
              plot.title = element_text(hjust = 0.5)) +
        ggtitle("Baby Weight Chart") +
        scale_x_datetime(date_breaks = "10 day", date_labels = "%d %b")
      
      q
    })
}

shinyApp(ui, server)