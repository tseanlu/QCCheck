library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(knitr)
library(tinytex)
library(gridExtra)
library(haven)
library(DT)
library(grid)
library(tableone)
library(ggplot2)
library(reshape)
library(lme4)
library(QCfuns)
library(parsedate)
library(dplyr)
library(parsedate)
source("QCfuns.R")
setwd("\\\\cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data/rawdata")
namraw=list.files()
inputdatar=namraw[!grepl("sas7bdat,v",namraw)]
setwd("\\\\cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data")
namsdtm=list.files()
inputdatas=namsdtm[grepl("sas7bdat",namsdtm)]
setwd("\\\\cdars.pfizer.com/cdars/prod/prjA409/internship/A4091056/saseng/cdisc3_0/data_vai")
namadam=list.files()
CRFspec1<-"Collection date" #the spec could be customized based on the specification
# for CRF.
inputdataa=namadam[grepl("sas7bdat",namadam)]

shinyUI(navbarPage("SAP QC Shiny App",
                   navbarMenu("Data Table",
                              tabPanel("Raw Data",
                                       h1("The Raw Data"),
                                       p("This tabs shows the raw data as received from
                                         CRF, check briefly if there is any difference"),

                                       sidebarPanel(
                                         selectInput("datasetr",
                                                     "Choose a Raw dataset:",
                                                     choices=inputdatar),
                                         tags$br(),
                                         actionBttn("updater", "Update Raw data view",
                                                    color="default",style="jelly",
                                                    size="sm")),

                                       mainPanel(
                                         DT::dataTableOutput("viewr"))
                                       ),
                              tabPanel("SDTM Data",
                                       h1("The SDTM Data"),
                                       p("This tabs shows the SDTM data as validated from
                                         PDS data, check briefly if there is any difference"),
                                       sidebarPanel(
                                         selectInput("datasets",
                                                     "Choose a SDTM dataset:",
                                                     choices=inputdatas),
                                         tags$br(),
                                         actionBttn("updates", "Update SDTM data view",
                                                    color="default",style="jelly",
                                                    size="sm")),

                                       mainPanel(
                                         DT::dataTableOutput("views"))
                                       ),
                              tabPanel("ADAM Data",
                                       h1("The AdaM Data"),
                                       p("This tabs shows the final data that meets the CDISC
                                         , check carefully if there is any difference"),
                                       sidebarPanel(
                                         selectInput("dataseta",
                                                     "Choose a ADaM dataset:",
                                                     choices=inputdataa),
                                         tags$br(),
                                         actionBttn("updatea", "Update ADaM data view",
                                                    color="default",style="jelly",
                                                    size="sm")),

                                       mainPanel(
                                         DT::dataTableOutput("viewa")))
                              ),
                   tabPanel("QCcheck",
                            sidebarPanel(
                              fileInput("file1", "Upload Spec Data Here:",
                                        accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                              actionBttn("QCdataset","QC check items for Dataset",
                                         color="primary",style="jelly",
                                         size="sm"),
                              tags$br(),
                              actionBttn("QCgeneral","QC check items for General",
                                         color="success",style="jelly",
                                         size="sm"),
                              tags$br(),
                              actionBttn("QCtable","QC check items for Table",
                                         color="warning",style="jelly",
                                         size="sm"),
                              tags$br(),
                              actionBttn("QClisting","QC check items for Listing",
                                         color="danger",style="jelly",
                                         size="sm"),
                              tags$br(),
                              actionBttn("QCfigure","QC check items for Figure",
                                         color="royal",style="jelly",
                                         size="sm"),
                              tags$br(),
                              actionBttn("QCsdtm","QC check items for SDTM Dataset",
                                         color="primary",style="jelly",
                                         size="sm")),
                            mainPanel(
                              htmlOutput("Message"),
                              downloadBttn("QC_Reports","QC reports",
                                           color="warning",style="unite",size="sm"))),
                   tabPanel("Freq Table",
                            selectInput("subset","Choose Safety population",
                                        choices=c("SAFETY","FULL")),
                            selectInput("fqdomain","Choose a dataset",
                                        choices=c("ADSL","ADAE")),
                            selectInput("strata","Choose a variable to stratify",
                                        choices=c("OVERALL","SEX")),
                            actionBttn("updatestrata","Update",
                                       color="royal",style="jelly",
                                       size="sm"),
                            DT::dataTableOutput("table")),
                   navbarMenu("Efficacy",
                              tabPanel("Boxplot",
                                       selectInput("score","Choose Score Category",
                                                   choices=c("Pain subscale","Physical Function")),
                                       actionBttn("updatescore","Update",
                                                  color="warning",style="jelly",
                                                  size="sm"),
                                       plotOutput("boxplot"),
                                       verbatimTextOutput("anova")
                              ),
                              tabPanel("Line Graph",
                                       sliderInput("n",
                                                   "Number Of Observations:",
                                                   value=500,min=10,max=2929),
                                       plotOutput("lineplot")
                              ),
                              tabPanel("Statistics",
                                       radioButtons("cscore","Choose Score Category",
                                                    choices=c("Pain subscale","Physical Function")),
                                       radioButtons("cstat","Choose Output",
                                                    choices=c("ANOVA","Summary")),
                                       verbatimTextOutput("stat")
                              )
                   ),

                   tabPanel("Adverse Event",
                            radioButtons("barpie","Choose a plot",
                                         choices=c("piechart","barplot","percentplot")),
                            radioButtons("subset","Subseting",
                                         choices=c("Fulldata","Based on SAP")),

                            plotOutput("chart")
                   )



                   ))
