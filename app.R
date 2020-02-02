#################
#################
###SHINY EPIDELTA
#################
#################
#Rosa Mulder
#Oct 30 2019


################
###Load packages
################s
library(ggplot2)      
library(shiny)
library(shinyWidgets)   #to change background color
library(imager)         #to show images
library(shinyjs)        #to hide buttons



############
###Locations
############
dfloc     <- "./Dataframe/"                        
figureloc <- "./Figures/"                        
logoloc   <- "./Logos/"                        



#######
###Load
#######
load(paste0(dfloc,"/sub_191114.RData"))     




############
###CpG names
############
cpgs <- rownames(sub)
names(cpgs) <- cpgs



#################
###USER INTERFACE
#################
ui <- tagList(               
  
  titlePanel(fluidRow(column(6, tags$h1(tags$strong(tags$code("THE EPIDELTA PROJECT")))),
                      column(1, tags$img(src=paste0(logoloc,'Logo_EMC.png'), height = 40, width = 80, href="https://www.erasmusmc.nl/")),  
                      column(1, tags$img(src=paste0(logoloc,'Logo_GenR.png'), height = 40, width = 40, href="https://generationr.nl/researchers/")),  
                      column(1, tags$img(src=paste0(logoloc,'Logo_BristolUni.png'), height = 40, width = 80, href="https://www.bristol.ac.uk/")), 
                      column(1, tags$img(src=paste0(logoloc,'ALSPAC.png'), height = 40, width = 30, href="http://www.bristol.ac.uk/alspac/"))),  
             windowTitle="epidelta project"),
  
  
  navbarPage("",
             tabPanel(HTML(paste("[placeholder for article reference]", 
                      "", 
                      sep="<br/>")),
                      ###Input objects
                      sidebarPanel(helpText(HTML(paste("current options are:", 
                                                       "cg26840798","cg23201812","cg12376829",            
                                                       "cg01119831","cg02597894","cg21244580",
                                                       "cg00029246","cg07661340","cg27457631",
                                                       "cg03371609","cg26921482","cg01483824",
                                            "full 450K autosomal sites will follow!", sep="<br/>"))),
                                   
                                   useShinyjs(),   #to hide buttons

                                   textInput(inputId="typeCpG",label="CpG site of interest",placeholder="cg00029246",value="cg00029246"),
                                   actionButton(inputId = "update",label = "Go"),
                                    
                                   selectInput(inputId="cpg_list_options",label="load results",
                                               choices=list("all"=1,
                                                            "M1 change estimate Bonferroni significant (1E-07)"=2,
                                                            "M1 inter-individual variation in change Bonferroni significant (1E-07)"=3)), 
                                   downloadButton(outputId="download_button", label="download"),
                                   
                                   width=2)),
             
             mainPanel(fluidRow(column(4, offset=0.5, h3("M2 - nonlinear changes"), imageOutput(outputId = "Predicted_data_M2_bycohort")),
                                column(4, offset=1, h3("M3 - sex differences in change"), imageOutput(outputId = "Predicted_data_M3_bysex")),
                                column(2, offset=1, h4("INFO"), htmlOutput(outputId = "info"))),
                       fluidRow(column(12, tableOutput("table_results"))))))
  

######################
###SERVER INPUT OUTPUT
######################
server <- function(input, output) {
  

  ############
  ###Graph CpG
  ############
  data_cpg <- eventReactive(input$update, {
    input$typeCpG})
  
  
  #long plot from results                  
  output$Predicted_data_M2_bycohort <- renderImage({
    
    graphname_M2_bycohort <- paste0(figureloc,data_cpg(),'_M2_bycohort_191114.png')   # NOTE: pdf is much smaller, but am not able to use the pdf version
    image_M2_bycohort     <- graphname_M2_bycohort
    
    list(src = image_M2_bycohort, 
         contentType = 'image/png',
         #contentType = 'application/pdf',                                            # doesn't work
         width = 400,
         height = 300,
         title = "Model 2 - nonlinear changes",            
         alt = "Sorry something went wrong for this graph")
  }, deleteFile = FALSE)
  

  output$Predicted_data_M3_bysex <- renderImage({
    
    graphname_M3_bysex    <- paste0(figureloc,data_cpg(),'_M3_bysex_191114.png')
    image_M3_bysex        <- graphname_M3_bysex
    
    list(src = image_M3_bysex, 
         contentType = 'image/png',
         width = 400,
         height = 300,
         title = "Model 3 - sex differences",
         alt = "Sorry something went wrong for this graph")
  }, deleteFile = FALSE)

  
  output$table_results <- renderTable({
    
    mat           <- matrix(nrow=10, ncol=5)
    rownames(mat) <- c("M1 intercept","M1 change", "M2 intercept", "M2 change", "M2 slope change at 6y", "M2 slope change at 9y", "M3 intercept", "M3 change", "M3 sex", "M3 change by sex")
    colnames(mat) <- c("b", "se", "p","inter-individual variance, sd","inter-individual variance, p")
    
    
    mat[1,1]  <- format(sub[rownames(sub)==data_cpg(),"M1.intercept.estimate"],digits=3,scientific=TRUE)
    mat[2,1]  <- format(sub[rownames(sub)==data_cpg(),"M1.change.estimate"],digits=3,scientific=TRUE)
    mat[3,1]  <- format(sub[rownames(sub)==data_cpg(),"M2.intercept.estimate"],digits=3,scientific=TRUE)
    mat[4,1]  <- format(sub[rownames(sub)==data_cpg(),"M2.change.estimate"],digits=3,scientific=TRUE)
    mat[5,1]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange6.estimate"],digits=3,scientific=TRUE)
    mat[6,1]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange9.estimate"],digits=3,scientific=TRUE)
    mat[7,1]  <- format(sub[rownames(sub)==data_cpg(),"M3.intercept.estimate"],digits=3,scientific=TRUE)
    mat[8,1]  <- format(sub[rownames(sub)==data_cpg(),"M3.change.estimate"],digits=3,scientific=TRUE)
    mat[9,1]  <- format(sub[rownames(sub)==data_cpg(),"M3.sex.estimate"],digits=3,scientific=TRUE)
    mat[10,1] <- format(sub[rownames(sub)==data_cpg(),"M3.changebysex.estimate"],digits=3,scientific=TRUE)

    mat[1,2]  <- format(sub[rownames(sub)==data_cpg(),"M1.intercept.se"],digits=3,scientific=TRUE)
    mat[2,2]  <- format(sub[rownames(sub)==data_cpg(),"M1.change.se"],digits=3,scientific=TRUE)
    mat[3,2]  <- format(sub[rownames(sub)==data_cpg(),"M2.intercept.se"],digits=3,scientific=TRUE)
    mat[4,2]  <- format(sub[rownames(sub)==data_cpg(),"M2.change.se"],digits=3,scientific=TRUE)
    mat[5,2]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange6.se"],digits=3,scientific=TRUE)
    mat[6,2]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange9.se"],digits=3,scientific=TRUE)
    mat[7,2]  <- format(sub[rownames(sub)==data_cpg(),"M3.intercept.se"],digits=3,scientific=TRUE)
    mat[8,2]  <- format(sub[rownames(sub)==data_cpg(),"M3.change.se"],digits=3,scientific=TRUE)
    mat[9,2]  <- format(sub[rownames(sub)==data_cpg(),"M3.sex.se"],digits=3,scientific=TRUE)
    mat[10,2] <- format(sub[rownames(sub)==data_cpg(),"M3.changebysex.se"],digits=3,scientific=TRUE)
    
    mat[1,3]  <- " "
    mat[2,3]  <- format(sub[rownames(sub)==data_cpg(),"M1.change.p"],digits=3,scientific=TRUE)
    mat[3,3]  <- " "
    mat[4,3]  <- format(sub[rownames(sub)==data_cpg(),"M2.change.p"],digits=3,scientific=TRUE)
    mat[5,3]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange6.p"],digits=3,scientific=TRUE)
    mat[6,3]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange9.p"],digits=3,scientific=TRUE)
    mat[7,3]  <- " "
    mat[8,3]  <- format(sub[rownames(sub)==data_cpg(),"M3.change.p"],digits=3,scientific=TRUE)
    mat[9,3]  <- format(sub[rownames(sub)==data_cpg(),"M3.sex.p"],digits=3,scientific=TRUE)
    mat[10,3] <- format(sub[rownames(sub)==data_cpg(),"M3.changebysex.p"],digits=3,scientific=TRUE)
    
    mat[1,4]  <- format(sub[rownames(sub)==data_cpg(),"M1.intercept.rand.sd"],digits=3,scientific=TRUE)
    mat[2,4]  <- format(sub[rownames(sub)==data_cpg(),"M1.change.rand.sd"],digits=3,scientific=TRUE)
    mat[3,4]  <- format(sub[rownames(sub)==data_cpg(),"M2.intercept.rand.sd"],digits=3,scientific=TRUE)
    mat[4,4]  <- format(sub[rownames(sub)==data_cpg(),"M2.change.rand.sd"],digits=3,scientific=TRUE)
    mat[5,4]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange6.rand.sd"],digits=3,scientific=TRUE)
    mat[6,4]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange9.rand.sd"],digits=3,scientific=TRUE)
    mat[7,4]  <- format(sub[rownames(sub)==data_cpg(),"M3.intercept.rand.sd"],digits=3,scientific=TRUE)
    mat[8,4]  <- format(sub[rownames(sub)==data_cpg(),"M3.change.rand.sd"],digits=3,scientific=TRUE)
    mat[9,4]  <- " "
    mat[10,4] <- " "
    
    mat[1,5]  <- format(sub[rownames(sub)==data_cpg(),"M1.intercept.rand.p"],digits=3,scientific=TRUE)
    mat[2,5]  <- format(sub[rownames(sub)==data_cpg(),"M1.change.rand.p"],digits=3,scientific=TRUE)
    mat[3,5]  <- format(sub[rownames(sub)==data_cpg(),"M2.intercept.rand.p"],digits=3,scientific=TRUE)
    mat[4,5]  <- format(sub[rownames(sub)==data_cpg(),"M2.change.rand.p"],digits=3,scientific=TRUE)
    mat[5,5]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange6.rand.p"],digits=3,scientific=TRUE)
    mat[6,5]  <- format(sub[rownames(sub)==data_cpg(),"M2.slopechange9.rand.p"],digits=3,scientific=TRUE)
    mat[7,5]  <- format(sub[rownames(sub)==data_cpg(),"M3.intercept.rand.p"],digits=3,scientific=TRUE)
    mat[8,5]  <- format(sub[rownames(sub)==data_cpg(),"M3.change.rand.p"],digits=3,scientific=TRUE)
    mat[9,5]  <- " "
    mat[10,5] <- " "
    mat 
  }, rownames=TRUE)
  
  
  ################
  ###Download data
  ################
  data_subresults <- reactive({
    if(input$cpg_list_options==1){
      epideltadata <- sub
    }else if(input$cpg_list_options==2){
      epideltadata <- sub[sub$M1.change.p<1e-7,]
    }else if(input$cpg_list_options==3){
      epideltadata <- sub[sub$M1.change.rand.p<1e-7,]
    }
  })
  
  output$download_button <- downloadHandler(                
    filename = function(){
      paste("epidelta_", Sys.Date(),".txt", sep='\t')},
    
    content= function(file){write.table(data_subresults(), file, row.names=TRUE)})
  
  
  ################
  ###Add some info
  ################
  output$info <- renderUI(HTML(paste("Three linear mixed models were tested",
                                     "",
                                     "M1:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + u<sub>1i</sub>Age<sub>ij</sub> + 
                                     u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>",
                                     "",
                                     "M2:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + &beta;<sub>2</sub>(Age<sub>ij-6</sub>) + 
                                     &beta;<sub>3</sub>(Age<sub>ij-9</sub>) + u<sub>1i</sub>Age<sub>ij</sub> + u<sub>2i</sub>(Age<sub>ij-6</sub>) + u<sub>3i</sub>(Age<sub>ij-9</sub>) + 
                                     u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>",
                                     "",
                                     "M3:  M<sub>ijk</sub> = &beta;<sub>0</sub> + u<sub>0i</sub> + &beta;<sub>1</sub>Age<sub>ij</sub> + u<sub>1i</sub>Age<sub>ij</sub> + 
                                     &beta;<sub>2</sub>Sex<sub>i</sub>Age<sub>ij</sub> u<sub>0k</sub> + covariates + &epsilon;<sub>ijk</sub>",
                                     "",
                                     "Covariates: batch, estimated white blood cells, gestational age, sex, cohort",
                                     "The graphs display the predicted data based on the models", sep="<br/>")))
  
}

############
###SHINY APP
############
shinyApp(ui=ui, server=server)




