#Strategy -> 
# 1. save the comments in a separate file along with DLP+Name+TimeStamp
# 2a. Merge with ScoresF & display the concatenation of comments.
# 2b. Merge with ScoresF & display only the latest comment.

# !!! Important to save as a data.table otherwise it does not work !!!
# scoresF<-read.csv(file="/home/mint/R/AnalyticsOnDemand2/data/scoresF.csv")
# scoresF$Comments<-""
# saveRDS(as.data.table(scoresF[scoresF$var1=="ZOSTER (SHINGRIX)",
#                               c("var1","var2","N","EBGM","QUANT_05","Stat","Pval","Comments")]),"scoresF.rds")

library(shiny)
library(shinyjs)
## shinysky is to customize buttons
library(shinysky)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)
library(aws.s3)

s3BucketName <- "shingrix"
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAIYLHB5SP2ZBP2AZA",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "eu-west-3")

rm(list = ls())
useShinyalert()
shinyServer(function(input, output, session){
  
  ### interactive dataset 
  vals_trich<-reactiveValues()
  vals_trich$Data<-s3readRDS("scoresF.rds",bucket=s3BucketName)

  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    fluidPage(
          hr(),
          column(6,
 #                HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                 ### tags$head() This is to change the color of "Add a new row" button
                 tags$head(tags$style(".butt3{background-color:#4d1566;} .butt3{color: #e6ebef;}")),
                 div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit comment", class="butt3") )),
          column(6,
 #               HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                 tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
                 div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Updated_trich",label = "Save", class="butt4") ),
                 ### Optional: a html button 
                 # HTML('<input type="submit" name="Add_row_head" value="Add">'),
                 HTML('</div>') ),
          
          column(12,dataTableOutput("Main_table_trich")),
           tags$script("$(document).on('click', '#Main_table_trich button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")

      ) 
    })
  
  #### render DataTable part ####
  output$Main_table_trich<-renderDataTable({
    DT=vals_trich$Data
    datatable(DT,selection = 'single',
              escape=F,
              rownames=F) })
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    file_path <- file.path(tempdir(),"scoresF.rds")
    saveRDS(vals_trich$Data,file_path)
    # Upload the file to S3
    s3saveRDS(file_path, object = "scoresF.rds", bucket = s3BucketName)
    shinyalert(title = "Saved!", type = "success")
  })
  
  ### edit button
  observeEvent(input$mod_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modification"),align="center"),
            hr(),
            # See output$row_modif below
            dataTableOutput('row_modif'),
            actionButton("save_changes","Save changes"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
    )
  })

  #### modify part
  output$row_modif<-renderDataTable({
    selected_row=input$Main_table_trich_rows_selected
    old_row=vals_trich$Data[selected_row]
    row_change=list()
    row_change[["Comments"]]<-paste0('<input class="new_input" value= ','"',old_row[["Comments"]],'"',' type="textarea"  id=new_Comments><br>')
    row_change=as.data.table(row_change)
    setnames(row_change,c("Comments"))
    DT=row_change
    DT 
    },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none",rownames=F)
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 colnames(DF)=c("Comments")
                 vals_trich$Data[input$Main_table_trich_rows_selected,c("Comments")]<-DF$Comments
                 
               }
  )
 
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
  
})
