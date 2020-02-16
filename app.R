#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# To deploy:
# library(rsconnect)
# deployApp()

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(kableExtra)
library(plotly)
library(shinythemes)
library(data.table)

# Read data.
data <- as.data.frame(fread("./data/tsbart-shiny-data.csv", stringsAsFactors = F))

# Load plotting theme.
source('ggtheme-publication.R')

#==========================================================================
# Define UI for application
#==========================================================================

ui <- fluidPage(theme = shinytheme("flatly"),
   
   # Add CSS tags
  # tags$style("#myNumericInput {font-size:200px;height:10px;}"),
                
   # Application title
   titlePanel("Stillbirth Risk Prediction with tsBART (Starling, 2020)"),
   
   #---------------------------------------------------------------
   # Sidebar with inputs
   #---------------------------------------------------------------
   sidebarLayout(
      sidebarPanel(width = 4,
         
         # Gestational age.
         sliderInput("gestage", "Gestational age (w)", min = 34, max = 42, value=39, step=1),
         
         # Mom age
         sliderTextInput(
            inputId = "agegrp", 
            label = "Maternal age (y)", 
            grid = TRUE, 
            force_edges = TRUE,
            choices = c("<20", "20-24", "25-29", "30-34", "35-39", "40-44", "45+"),
            selected = "25-29"),
               
         
         # Diabetes, HTN, other risk factors.
         awesomeCheckboxGroup(
            inputId = "med",
            label = "Medical Hx", 
            choices = c("Diabetes", "Hypertension", "Other risk factors")
         ),
         
         
         # Ethnicity.
         awesomeRadio(
            inputId = "eth",
            label = "Maternal ethnicity", 
            choices = c("Black (non-Hispanic)", "Hispanic", "Other", "White (non-Hispanic)"),
            selected = "White (non-Hispanic)"
         ),
         
         # Weight gain quantile.
         sliderTextInput(
            inputId = "wtgainQ", 
            label = "Weight gain at gest age (Q)", 
            grid = TRUE, 
            force_edges = TRUE,
            choices = c("[0,0.1)", "[0.1,0.25)", "[0.25,0.75)", "[0.75,0.9)", "[0.9,1]"),
            selected = "[0.25,0.75)"), 
         
         # Birth weight quantile.
         sliderTextInput(
            inputId = "birthwtQ", 
            label = "Birth wt. at gest age (Q)", 
            grid = TRUE, 
            force_edges = TRUE,
            choices = c("[0,0.1)", "[0.1,0.25)", "[0.25,0.75)", "[0.75,0.9)", "[0.9,1]"),
            selected = "[0.25,0.75)"), 
        
         # Infant sex
         materialSwitch(
            inputId = "male",
            label = "Female infant", 
            value = 0,
            status = "info"
         ),
         
         # Induced labor.
         materialSwitch(
            inputId = "induce",
            label = "Induced labor", 
            value = 0,
            status = "info"
         ),
         
         # Parity.
         materialSwitch(
            inputId = "multiparous",
            label = "Multiparous", 
            value = 0,
            status = "info"
         )
         
      ), # End sidebar panel.  
      
      #---------------------------------------------------------------
      # Main panel, with tabs for Plots and Tables.
      #---------------------------------------------------------------
      
      mainPanel(

         # Two tabs:  One for plots, one for table output.
         tabsetPanel(
            # Plotting tab.
            tabPanel("Plot", 
                     
                     fluidRow(
                        plotOutput("stillbirth_risk_plot", height=300, width=500)
                     ), 
                     
                     fluidRow(
                        plotOutput("stillbirth_change_plot", height=300, width=500)
                     )
            ), 
            
            # Table tab.
            tabPanel("Data", 
                     
                     fluidPage(
                        tableOutput("table")
                     )      
            ), 
            tabPanel("Reference", 
                     
                     fluidRow(
                        p("The",strong("tsBART")," method used in this analysis is by Starling et al. The following reference should be cited:")
                     ),
            
                     
                     fluidRow(
                        p("Jennifer E. Starling, Jared S. Murray, Carlos M. Carvalho, Radek Bukowski, James G. Scott.", 
                          em("BART with Targeted Smoothing: an analysis of patient-specific stillbirth risk."), 
                          "In press, Annals of Applied Statistics, 2020.")
                     ), 
                     
                     fluidRow(
                        p("The R package tsbart implements the BART with Targeted Smoothing method, and is available
                                 at", tags$a(href="https://github.com/jestarling/tsbart", "https://github.com/jestarling/tsbart"),".")
                        ),  
                     
                     fluidRow(
                        p("This application predicts stillbirth risk using features of the maternal-fetal dyad. It runs using
                                 on data derived from analysis of publicly available Electronic Health Record data.  More information on
                                 data source and replicating the analysis is available at",
                          tags$a(href="https://github.com/jestarling/tsbart-analysis", "https://github.com/jestarling/tsbart-analysis"),".")
                        ),

                     
                     fluidRow(
                        p(strong("Medical Hx Input:"),"Other risk factors include any of the following:  anemia,
                                 cardiac disease, lung disease, diabetes mellitus, renal disease, or Rh sensitization.
                                 Pregnancy complications, such as gestational diabetes, abruption, preeclampsia,
                                 are excluded (consistent with",
                          tags$a(href="https://www.ajog.org/article/S0002-9378(12)02200-4/fulltext", 
                                 "Mandujano et al. 2013"),".")
                        )        
            ) 
            
         ) # End tabsetPanel
      ) # End mainPanel
   )
)

#==========================================================================
# Define server logic
#==========================================================================

server <- function(input, output) {
   
   ###---------------------------------------------------------------
   ### Return the requested dataset 
   ###---------------------------------------------------------------
   datasetInput <- reactive({
      
      # Process medical condition inputs.
      d_ht = 'Neither'; othr = 0
      
      if('Other risk factors' %in% input$med){
         othr=1
      }
      
      if( ('Hypertension' %in% input$med) & !('Diabetes' %in% input$med) ){
         d_ht = 'Htn'
      } else if( !('Hypertension' %in% input$med) & ('Diabetes' %in% input$med) ){
         d_ht = 'Diabetes'
      } else if( ('Hypertension' %in% input$med) & ('Diabetes' %in% input$med)  ){
         d_ht = 'Both'
      }
      
      # Process ethnicity inputs.
      # Ethnicity.
      d_eth = ifelse(input$eth=="Black (non-Hispanic)",  "Black NonHisp",
             ifelse(input$eth=="Hispanic", "Hispanic", 
                    ifelse(input$eth=="Other", "Other", "White NonHisp")))
      
      # Subset data based on inputs.
      subset = data[which(
            data$agegrp == input$agegrp &
            data$diab_htn == d_ht &
            data$otherRisk == othr &
            data$momEthnicity == d_eth &
            data$wtgainQ == input$wtgainQ &
            data$birthwtQ == input$birthwtQ &
            data$multiparous == input$multiparous & 
            data$induce == input$induce &
            data$male == input$male
      ),]
      
      print(input$agegrp); print(d_ht); print(othr); print(input$eth); print(input$wtgainQ); print(input$birthwtQ); print(input$multiparous)
      print(input$induce); print(input$male)
      return(subset)
   
   })
   
   ###---------------------------------------------------------------
   ### Main stillbirth risk plot.
   ###---------------------------------------------------------------
   
   output$stillbirth_risk_plot <- renderPlot({
      
      ggdf = datasetInput()
      #print(dim(datasetInput()))
      #print(head(datasetInput()))

      theme_set(theme_bw(base_size=11, base_family='Helvetica'))
      
      # Set up title with risk info for that patient/ga combo.
      subtitle_rr = round(as.numeric(ggdf$phat_adj[which(ggdf$gest_age==input$gestage)]), 2)
      subtitle_lb = round(as.numeric(ggdf$phat_lb_adj[which(ggdf$gest_age==input$gestage)]), 2)
      subtitle_ub = round(as.numeric(ggdf$phat_ub_adj[which(ggdf$gest_age==input$gestage)]), 2)
      mysub = paste0('Estimated risk: ', subtitle_rr, " (", subtitle_lb, ", ", subtitle_ub, ") per 1,000.")
         
      print(subtitle_rr)
      print(mysub)
      
      # Custom y-limit.
      myylim = ifelse(as.numeric(max(ggdf$phat_adj>7.5)), 12, 7.5)
      
      # Create plot.
      plt = ggplot(ggdf, aes(x=gest_age, y=phat_adj)) +
         geom_ribbon(aes(x=gest_age, ymin=phat_lb_adj, ymax=phat_ub_adj), alpha=0.3, fill='grey20') +
         geom_line(size=1.2, colour='blue4') +
         coord_cartesian(xlim = c(34,42), ylim = c(0,myylim), expand=F) +
         labs(x = 'Gestational age (w)',
              y = 'Risk Given Gest. Age',
              title = 'Risk of stillbirth per 1,000 pregnancies.',
              subtitle = mysub) +
         theme_Publication()+
         geom_vline(mapping = NULL, data = NULL, xintercept = input$gestage, linetype=1, size=.8, colour='green4')
      
      # Print plot
      print(plt)
   })
 
   ###---------------------------------------------------------------
   ###  Stillbirth risk change over time plot.
   ###---------------------------------------------------------------  

   output$stillbirth_change_plot<- renderPlot({
      
      #print(head(ggpanel))
      #print(dim(ggpanel))
      
      theme_set(theme_bw(base_size=12, base_family='Helvetica'))
      
      # Create plot.
      plt = ggplot(datasetInput(), aes(x=gest_age, y=phat_wk_chg)) +
         geom_smooth(size=1.2, colour='blue4', se=F) +
         coord_cartesian(xlim = c(34,42), expand=F) +
         labs(x = 'Gestational age (w)',
              y = expression(paste('Risk'[W],' - Risk'[W-1])),
              title = 'Change in risk from previous week.') +
         theme_Publication()+
         geom_vline(mapping = NULL, data = NULL, xintercept = input$gestage, linetype=1, size=.8, colour='green4')
      
      # Print plot
      print(plt)
   })
   
   ###---------------------------------------------------------------
   ### Output data frame.
   ###---------------------------------------------------------------
   
   output$table <- function() {

      #---------------------------
      # CREATE DATA SET

      # Subset data based on inputs.
      subset = datasetInput()

      # Sort dataset by gest_age then trt.
      subset = subset[order(subset$gest_age),c('gest_age','phat_adj','phat_lb_adj','phat_ub_adj')]

      # Create percent increase for each gestational age from previous.
      subset$pct_incr = noquote(format(round((subset$phat_adj  - lag(subset$phat_adj,1))/ 
                                                lag(subset$phat_adj,1) * 100, 2), nsmall=2))
      subset$pct_incr[1] = ""
      
      # Create absolute week-week increase.
      subset$abs_incr = noquote(format(round(subset$phat_adj  - lag(subset$phat_adj,1), 2), nsmall=2))
      
      # Format a column for output.
      subset$phat = ""
      for(i in 1:nrow(subset)){
         subset$phat[i] = paste0(format(round(subset$phat_adj[i],3),nsmall=3),
                                 " (",
                                 format(round(subset$phat_lb_adj[i],3),nsmall=3),
                                 " - ",
                                 format(round(subset$phat_ub_adj[i],3),nsmall=3),
                                 ")"
                                 )
      }

      subset$pct_incr = format(subset$pct_incr,nsmall=3)
      subset$pct_incr[1] = ""
      
      subset$abs_incr = format(subset$abs_incr,nsmall=3)
      subset$abs_incr[1] = ""
      
      # Create table for output.
      printtab = subset[,c('gest_age','phat','abs_incr','pct_incr')]
      colnames(printtab) = c('Week', 
                             'Risk per 1,000 births', 
                             'W/W Risk Change (Net)',
                             'W/W Risk Change (%)')

      rownames(printtab) = NULL
      print(head(printtab))

      #---------------------------
      # OUTPUT KABLE
      printtab %>%
         knitr::kable(format = "html", align = 'lccc') %>%
         kable_styling("striped", full_width = F) %>%
         row_spec(which(printtab[,"Week"] == input$gestage), bold = T, color='blue4') %>%
         column_spec(1, width = "3em") %>%
         column_spec(2, width = "14em") %>%
         column_spec(3:4, width = "6em")

   }
}

#==========================================================================
# Run the application 
#==========================================================================

shinyApp(ui = ui, server = server)

