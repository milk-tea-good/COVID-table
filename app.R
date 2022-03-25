# library(shiny)
# library(rsconnect)
# rsconnect::deployApp('C:/Users/user/Desktop/R Language/Covid-19')
# rm(list=ls())

countries <- c("Australia", "Austria", "Cambodia", "Canada", "China", "Colombia", 
               "Denmark", "Egypt", "Finland", "France", "Germany", "Greece", "Iceland", 
               "India", "Indonesia", "Iraq", "Italy", "Japan", "Korea, South", "Malaysia", 
               "Mexico", "Monaco", "Netherlands", "New Zealand", "Norway", "Pakistan", "Peru", 
               "Philippines", "Poland", "Romania", "Russia", "Singapore", "South Africa", 
               "Taiwan*", "Thailand", "Turkey", "US", "Uganda", "Ukraine", "United Kingdom")

covConfirmedUrl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/
master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

covConfirmed <- read.table(covConfirmedUrl, header=TRUE, sep=",", comment.char = "\'", fill = TRUE)

covConfirmed <- covConfirmed[-c(3:349)]

# 抓出預設的40個國家
index=1 
covConfirmed_new <- data.frame()
for(i in 1:nrow(covConfirmed)){
  if(countries[index]==covConfirmed[i, 2]){
    if(covConfirmed[i, 2]=="Canada" & covConfirmed[i, 1]!="Ontario") next
    if(covConfirmed[i, 2]=="China" & covConfirmed[i, 1]!="Beijing") next
    if(covConfirmed[i, 2]=="Denmark" & covConfirmed[i, 1]!="") next
    if(covConfirmed[i, 2]=="France" & covConfirmed[i, 1]!="") next
    if(covConfirmed[i, 2]=="Netherlands" & covConfirmed[i, 1]!="") next
    if(covConfirmed[i, 2]=="New Zealand" & covConfirmed[i, 1]!="") next
    if(covConfirmed[i, 2]=="United Kingdom" & covConfirmed[i, 1]!="") next
    covConfirmed_new[index,c(1:ncol(covConfirmed))] <- covConfirmed[i,c(1:ncol(covConfirmed))]
    index=index+1
  }
  if(index==length(countries)+1)
    break
}

# 整理表格格式
covConfirmed <- covConfirmed_new
rm(covConfirmed_new)

names(covConfirmed) <- gsub("X", "", names(covConfirmed), ignore.case = FALSE)
mydf <- unlist(strsplit(names(covConfirmed)[-c(1:2)], "\\."))
mydf <- as.data.frame(matrix(mydf, ncol = 3, byrow = TRUE))

myname <- c()
for (i in 1:nrow(mydf)) {
  myname <- c(myname, paste0(paste0("20", mydf[i, 3]), "-", mydf[i, 1], "-", mydf[i, 2]))
}
rm(mydf)
names(covConfirmed)[-c(1:2)] <- myname

# shiny的UI
ui <- fluidPage(
  h2(id="big-heading", "Table of the area of the COVID-19 confirmed number of people"),
  tags$style(HTML("#big-heading{color:#000000; font-family: Taipei Sans TC Beta}")),
  
  uiOutput("tab"),
  
  selectInput("select", label = h3(id="target_country", "Choose the country"), choices = list("Australia"=1, "Austria"=2, "Cambodia"=3, "Canada"=4, "China"=5, "Colombia"=6,
                                                                                              "Denmark"=7, "Egypt"=8, "Finland"=9, "France"=10, "Germany"=11, "Greece"=12,
                                                                                              "Iceland"=13, "India"=14, "Indonesia"=15, "Iraq"=16, "Italy"=17, "Japan"=18,         
                                                                                              "Korea, South"=19, "Malaysia"=20, "Mexico"=21, "Monaco"=22, "Netherlands"=23, "New Zealand"=24, 
                                                                                              "Norway"=25, "Pakistan"=26, "Peru"=27, "Philippines"=28, "Poland"=29, "Romania"=30, 
                                                                                              "Russia"=31, "Singapore"=32, "South Africa"=33, "Taiwan"=34, "Thailand"=35, "Turkey"=36, 
                                                                                              "US"=37, "Uganda"=38, "Ukraine"=39, "United Kingdom"=40, "View All"=41), selected = 34),
  tags$style(HTML("#target_country{color:#000000; font-family: Taipei Sans TC Beta}")),
  
  dateRangeInput("dates", label = h3(id="target_date", "Choose date(2021~now)")),
  tags$style(HTML("#target_date{color:#000000; font-family: Taipei Sans TC Beta}")),
  
  actionButton("action", label = h5(id="upload", "click to update")),
  tags$style(HTML("#upload{color: #002060; font-family: Taipei Sans TC Beta}")),
  
  hr(),
  
  dataTableOutput("newConfirmed")
)

# shiny的server
server <- function(input, output) {
  startDate <- eventReactive(input$action, {
    date <- input$dates
    date[1]
  })
  endDate <- eventReactive(input$action, {
    date <- input$dates
    date[2]
  })
  newData <- eventReactive(input$action, {
    date <- input$dates
    if(as.integer(input$select)!=41 & as.integer(date[1])-18623>=0){
      newCovConfirmed <- cbind(covConfirmed[1:2], covConfirmed[(as.integer(date[1])-18625):(as.integer(date[2])-18625)])
      newCovConfirmed <- newCovConfirmed[as.integer(input$select),1:ncol(newCovConfirmed)]
      newCovConfirmed
    }
    else if(as.integer(date[1])-18623>=0){
      newCovConfirmed <- cbind(covConfirmed[1:2], covConfirmed[(as.integer(date[1])-18625):(as.integer(date[2])-18625)])
      newCovConfirmed
    }
  })
  url <- a("Click Me!", href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")
  output$tab <- renderUI({
    tagList("Data Source:", url)
  })
  output$startDate <- renderText({ startDate() })
  output$endDate <- renderText({ endDate() })
  output$newConfirmed <- renderDataTable({ newData() })
}

# 執行shinyapp
shinyApp(ui = ui, server = server)

# 2021-1-1 = 18628(-18625)