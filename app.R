## app.R ##
library(shinydashboard)
library(readxl)
library(tidyr)
library(scales)
library(dplyr)
library(ggplot2)
library(lme4)
library(forcats)
library(kernelboot)
library(mgcv)

ui <- dashboardPage( skin = "green",
  dashboardHeader(title = "The First Months"),
  dashboardSidebar(fileInput("file1", "Choose weight data file (*.xlsx)", accept = ".xlsx"),
                   radioButtons("who_norm", "Display WHO norm data for:",
                                c("Female babies" = "female",
                                  "Male babies" = "male",
                                  "No norm data" = "none")),
                   hr(),
                   p("You can download a sample XLSX file to document the weight of your baby here.
                     This file can be opened with virtually any spreadsheet application including MS Excel or LibreOffice Calc.
                     Keep it safe!"),
                   downloadButton("downloadDefaultData", "Download "),
                   hr(),
                   p("This app does not save any of your data, but just plots them.
                   The app is for information purposes only and not for diagnosis, talk to a qualified health practitioner!"),
                   p("Find the code here (CC0):"),
                   tags$a(href="https://github.com/suddhasourav/baby_weight_dashboard", 
                          "github.com/suddhasourav/\nbaby_weight_dashboard")
                   ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tags$head(tags$style("#plot1{height:88vh !important;}")),
    fillRow(box(plotOutput("plot1"), width = "100%", height = "200%")
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
      
      df$DateTime <- df$Date + (df$Time - as.POSIXct(trunc(df$Time, units="days")))
      df$nDateTime <- as.numeric(df$DateTime)
      df_week_span <- difftime(max(df$DateTime), min(df$DateTime), unit="weeks")
      print(df_week_span)
      
      bm <- gam(Weight_in_g ~ s(nDateTime, bs = "tp", k = floor(dim(df)[1]/2)), data=df)
      
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
          if(df_week_span <= 13) {
            df_WHO_nomogram_wfa <- read_xlsx("WHO_tab_wfa_girls_p_0_13.xlsx")
          }
          else {
            ui$dashboardHeader$title = "The First X Weeks"
            df_WHO_nomogram_wfa <- read_xlsx("WHO_tab_wfa_girls_p_0_5_yrs.xlsx")
            df_WHO_nomogram_wfa[,1] <- 4.285714*df_WHO_nomogram_wfa[, 1]
          }
        }
        
        else if(input$who_norm == "male") {
          if(df_week_span <= 13) {
            df_WHO_nomogram_wfa <- read_xlsx("WHO_tab_wfa_boys_p_0_13.xlsx")
          }
          else {
            df_WHO_nomogram_wfa <- read_xlsx("WHO_tab_wfa_boys_p_0_5_yrs.xlsx")
            df_WHO_nomogram_wfa[,1] <- 4.285714*df_WHO_nomogram_wfa[, 1]
        }
      }
  
      colnames(df_WHO_nomogram_wfa) <- c("Week", "L", "M", "S",
                                             "0.1", "1", "3",  "5", "10", "15",
                                             "25",  "50", "75", "85", "90", "95",
                                             "97", "99", "99.9")
        
      df_WHO_nomogram_wfa_tidy <- pivot_longer(df_WHO_nomogram_wfa, cols = -c("Week", "L", "M", "S"), names_to = "WHO Percentile", values_to = "Weight_in_kg")
      df_WHO_nomogram_wfa_tidy <- df_WHO_nomogram_wfa_tidy %>% mutate(Weight_in_g = Weight_in_kg*1000)
      df_WHO_nomogram_wfa_tidy$`WHO Percentile` <- factor(df_WHO_nomogram_wfa_tidy$`WHO Percentile` ,
                                                              levels = c("0.1",  "1",  "3",  "5",  "10", "15",
                                                                         "25",   "50", "75", "85", "90", "95",
                                                                         "97",  "99", "99.9"))
        
      df_WHO_nomogram_wfa_tidy <- df_WHO_nomogram_wfa_tidy %>% mutate(nDateTime = min(df$nDateTime) + Week*7*24*60*60)
      df_WHO_nomogram_wfa_tidy$DateTime <- as.POSIXct(df_WHO_nomogram_wfa_tidy$nDateTime, origin = "1970-01-01")
      }
      
      ylimlow <- max(2000, min(df$Weight_in_g) - 500)
      ylimhigh <- max(4500, max(df$Weight_in_g) + 500)
      
      dodge <- position_dodge(width=0.9)
      q <- ggplot(data=df, aes(x=DateTime, y = Weight_in_g))
      
      if(input$who_norm != "none") {
        k_gam_WHO <- dim(df_WHO_nomogram_wfa)[1]
        print(k_gam_WHO)
        q <- q + geom_smooth(data = df_WHO_nomogram_wfa_tidy, aes(x=DateTime, y = Weight_in_g, color = `WHO Percentile`, group = `WHO Percentile`),
                 method = "gam", formula = y~s(x, k=k_gam_WHO), se=F, size=0.5)
      }
      
      q <- q +
        geom_ribbon(data=df_expanded, aes(ymin = Weight_in_g_LCI, ymax = Weight_in_g_HCI), fill = "grey70",
                    alpha = 0.5) +
        geom_line(data=df_expanded, linewidth = 0.75) +
        geom_point(alpha = 0.65, fill= "red",
                   shape = 21, size = 3, stroke = 1, color ="black",
                   position = dodge) +
        geom_line(data=df_expanded, aes(x=DateTime, y = ylimlow + 4*Weight_diff_7_days_in_g), color = "red", linewidth = 0.75) +
        scale_y_continuous(sec.axis = sec_axis(~ (. - ylimlow)/4, name = "Seven-day weight change (g)")) +
        xlab("Time") +
        ylab("Weight (g)") + 
        theme_bw(base_size = 15) +
        coord_cartesian(ylim = c(ylimlow, ylimhigh), xlim = c(min(df_expanded$DateTime),
                                                                                       max(df_expanded$DateTime)+ 7*24*60*60))+
        theme(axis.line.y.right = element_line(color = "red"), 
              axis.ticks.y.right = element_line(color = "red"),
              axis.text.y.right = element_text(color = "red"), 
              axis.title.y.right = element_text(color = "red"),
              #axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="top",
              plot.title = element_text(hjust = 0.5)) +
        ggtitle("Baby Weight Chart") +
        scale_x_datetime(breaks = breaks_pretty(n=20), labels = label_date_short(format = c("%Y", "%b", "%d", ""), sep = "\n"))
      
      q
    })
}

shinyApp(ui, server)