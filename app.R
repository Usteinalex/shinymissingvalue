# application d'exploration des données Dephy
## pour lancer l'application depuis le console du dossier shiny
# shiny::runApp('Shiny_repres_comp.R')
## ou dans Rstudio, ouvrir le fichier et cliquer sur "Run App"

## load packages 
library(shiny)
library(shinyjs)
library(rintrojs)
library(shinyBS)
#library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(zoo)
library(Hmisc)
library(VIM)
library(locfit)
library(bcv)
library(missForest)
library(Amelia)

#source(..., locale="UTF-8")
options(encoding = "UTF-8")
#Sys.setlocale("LC_ALL", "fr_FR.UTF-8")


#### Partie user interface  --------------------------------------------------------------|  
header <- dashboardHeader(# title = "EDA + Modeling Mocoriba"
  title = shinyDashboardLogo(
    theme = "onenote",
    boldText = "Missing values Imputation",
    mainText = " " ,
    badgeText = "v1.0"))

sidebar <- dashboardSidebar(
  collapsed = FALSE,
  # Input: Select a file ----
  fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  # Horizontal line ----
  tags$hr(),
  
  # Input: Checkbox if file has header ----
  checkboxInput("header", "Header", TRUE),
  
  # Input: Select separator ----
  radioButtons("sep", "Separator",
               choices = c(Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"),
               selected = ","),
  
  # Input: Select quotes ----
  radioButtons("quote", "Quote",
               choices = c(None = "",
                           "Double Quote" = '"',
                           "Single Quote" = "'"),
               selected = '"'),
  
  # Horizontal line ----
  tags$hr(),
  
  # Input: Select number of rows to display ----
  radioButtons("disp", "Display",
               choices = c(Head = "head",
                           All = "all"),
               selected = "head")
)  
  

body <- dashboardBody(
  shinyDashboardThemes(theme = "blue_gradient"),
  useShinyjs(),
  introjsUI(),
#____________________________________________________________________________________________________##
    fluidRow(
        box(title = "Donnée Brute", solidHeader = TRUE, status = "success",
            tableOutput("contents"), width = 6),

        
        box(selectInput(inputId = "MethodImp",
                        label = "Choisissez une methode d'imputation:",
                        choices = c("LOCF", "Moyenne", "Mediane", "kNN",
                                    "LOESS", "SVD", "MissForest", "AmeliaII")), 
            title = "Donnée traitée par une methode",
            width = 6, solidHeader = TRUE, 
            collapsed = FALSE,
            collapsible = TRUE,
            status = "warning",
            tableOutput("DataImput")
          )
        
        
        # box(title = "Performances et pratiques (valeurs)", width = 6,
        #     solidHeader = TRUE, status = "warning", collapsed = FALSE,
        #     collapsible = TRUE, plotOutput("boxViolonSel_reel", height = 200)),
        # 
        # 
        # box(title = "Position géographique", width = 6, solidHeader = TRUE, status = "success", 
        #     collapsed = FALSE, collapsible = TRUE, leafletOutput("carto", height = 200))
    
    ),
    
    ##________________________________________________________________________________________________________## 
   
)

ui <- dashboardPage(header, sidebar, body)

#### partie server #### 
server <- function(input, output){
  data <- reactive({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- as.data.frame(read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)) 
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  Display <- reactive({
    if(input$disp == "head") {
      return(head(data(), 10))
    }
    else {
      return(data())
    }
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    Display()
  })
    
    output$DataImput <- renderTable({
      data <- Display()
      if(input$MethodImp == "LOCF"){
        dat.locf=na.locf(data,na.rm=FALSE)
        dat.locf=na.locf(dat.locf,na.rm=FALSE, fromLast=TRUE) 
        
      }else if(input$MethodImp == "Moyenne"){
        dat.moy <- impute(data()[,3:5], fun=mean)
        
        finalDT <- cbind(data()[, 1:2], dat.moy)
        if(input$disp == "head") {
          return(head(finalDT, 10))
        }
        else {
          return(finalDT)
        }
        
      }else if(input$MethodImp =="Mediane"){
        med=apply(data()[,3:5],1,median,na.rm=TRUE)
        dat.med=data()[,3:5]
        ind.na=which(is.na(dat.med),arr.ind=TRUE)
        dat.med[ind.na]=med[ind.na[,1]]
        
        finalDT <- cbind(data()[,1:2], dat.med)
        if(input$disp == "head") {
          return(head(finalDT, 10))
        }
        else {
          return(finalDT)
        }
      }else if(input$MethodImp =="kNN"){
        dat.kNN=kNN(data, k=5, imp_var=FALSE)
        
      }else if(input$MethodImp == "LOESS"){
        dat.imputed=rbind(colnames(data()[,3:5]),data()[,3:5])
        indices=1:nrow(dat[,3:5])
        dat.loess= apply(dat.imputed, 2, function(j) {
          predict(locfit(j[-1] ~ indices), indices)
        })
        finalDT <- cbind(data()[,1:2], as.data.frame(dat.loess))
        if(input$disp == "head") {
          return(head(finalDT, 10))
        }
        else {
          return(finalDT)
        }
        
      }else if(input$MethodImp == "SVD"){
        dat.SVD=impute.svd(data()[,3:5],k=3,maxiter=1000)$x
        
        finalDT <- cbind(data()[,1:2], as.data.frame(dat.SVD))
        if(input$disp == "head") {
          return(head(finalDT, 10))
        }
        else {
          return(finalDT)
        }
        
      }else if(input$MethodImp == "MissForest"){
        dat.missForest<-missForest(data()[,3:5],maxiter=10,
                                   ntree = 20, variablewise = TRUE)$ximp
        finalDT <- cbind(data()[,1:2], as.data.frame(dat.missForest))
        if(input$disp == "head") {
          return(head(finalDT, 10))
        }
        else {
          return(finalDT)
        }
      }else if(input$MethodImp == "AmeliaII"){
        dat.amelia=amelia(data()[,3:5],m=1)$imputations$imp1
        
        finalDT <- cbind(data()[,1:2], as.data.frame(dat.amelia))
        
        if(input$disp == "head") {
          return(head(finalDT, 10))
        }
        else {
          return(finalDT)
        }
    
      }else{
        return()
      }
    }) 
  
}




# Run the application 
shinyApp(ui = ui, server = server)

