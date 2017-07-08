#Loading packages
      library(ggplot2)
      library(shinythemes)
      library(markdown)

#R script that only runs when app is first loaded
      ##Reading btw17_strukturdaten.csv file
      strukturdaten<-read.csv("data/btw17_strukturdaten_UTF-8.csv", 
                              header = T, 
                              sep = ";", 
                              skip = 8, 
                              dec = ",", 
                              na.strings = ".", 
                              fileEncoding = "UTF-8",
                              stringsAsFactors=FALSE,
                              check.names=FALSE)
      
      ##Dropping Laender and Bund aggregates
      strukturdatenWK<-strukturdaten[strukturdaten[,2]<900,]
      
      ##Ordering by alphabetical oder
      strukturdatenWK<-strukturdatenWK[order(strukturdatenWK$"Wahlkreis-Name"),]

#Server-function
      server <- function(input, output) {
        
        output$plot <- renderPlot({
          
          ##Reassigning input variables
          wk = input$wk
          stat = input$stat
      
          ##Creating new variable to indicate chosen Wahlkreis
          strukturdatenWK[,"myWK"]<-strukturdatenWK[,"Wahlkreis-Name"]==wk
          
          ##Histogramm
          ggplot(strukturdatenWK,aes(color = myWK, size = myWK)) +
            ggtitle(stat) +
            geom_point(aes(x=Land,y=strukturdatenWK[,stat])) + 
            xlab("") +
            ylab("") +
            scale_colour_manual(values=c("black","red")) +
            theme_bw() +
            theme(axis.text.x  = element_text(angle=60, hjust=1, size=16), 
                  axis.text.y  = element_text(size=16),
                  plot.title = element_text(size=22),
                  legend.position="none")
          })
      
      }

#User interface
ui <- fluidPage(theme = shinytheme("paper"),
  
  navbarPage("myWahlkreis",
             tabPanel("Stats",
                      
                      # Title
                      titlePanel("#btw17 myWahlkreis"),
                      
                      br(),
                      
                      ##First row - Data input
                      fluidRow(
                        column(3, 
                               
                               h4("What's your Wahlkreis?"),
                               
                               selectInput("wk", "Choose your Wahlkreis",
                                           choices = strukturdatenWK[,"Wahlkreis-Name"]),
                               
                               br()
                               
                        ),
                        column(3,
                               
                               h4("What do you want to know?"),
                               
                               selectInput("stat", "Choose your Statistik",
                                           choices = colnames(strukturdatenWK)[4:51]),
                               br(),
                               br()
                               
                        ),
                        
                        column(3,
                               
                               h4("How to read the graph:"),
                               includeMarkdown("explainer.md"),
                               br(),
                               br()
                               
                        ),
                        column(3,
                               h4("Share your Wahlkreis-Statistik:"),
                               bookmarkButton(),
                               br(),
                               br()
                        )
                      
                            
                      
                      ),
                      
                      ##Second row - Data output
                      fluidRow(
                      plotOutput("plot", width = "700", height = "450")
                      )
             ),
             
             tabPanel("About",
                      
                      includeMarkdown("about.md")
             )
  )
  
  
)


enableBookmarking(store = "url")

shinyApp(ui = ui, server = server)
