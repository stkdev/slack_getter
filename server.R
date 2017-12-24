source("func.R")

library("shiny")
library("httr")
library("magrittr")

shinyServer(function(input, output, session) {
  # init
  endPoint.history <- 'https://slack.com/api/channels.history'
  endPoint.user <- "https://slack.com/api/users.list"
  
  # slackへの送信パラメータ
  getParams <- reactive({
    oldest = as.numeric(as.POSIXct(as.Date(input$date_range[1])))
    latest = as.numeric(as.POSIXct(as.Date(input$date_range[2])))
    token = input$token
    channel = input$channel
    
    params <- list(
      token = getVal(token),
      channel = getVal(channel),
      count = 500,
      latest = latest,
      oldest = oldest
    )
    params
  })
  
  # トークンTextBoxの生成
  output$tokenOut <- renderUI({
    urlInfo <- parseQueryString(session$clientData$url_search)
    
    textInput('token',"トークン", urlInfo[["token"]])
  })
  
  # チェンネルIDのTextBoxの生成
  output$channelOut <- renderUI({
    urlInfo <- parseQueryString(session$clientData$url_search)
    
    textInput('channel',"チェンネルID", urlInfo[["channel"]])
  })
  
  # コピペ用URLのInputBox
  output$urlOut <- renderUI({
    cdata <- session$clientData
    
    textInput('url',
              "url", 
              paste0(
                cdata$url_protocol,
                "//",
                cdata$url_hostname,
                ":",
                cdata$url_port,
                cdata$url_pathname,
                "?token=",
                input$token,
                "&channel=",
                input$channel
              )
    )
  })
  
  # slackからの応答処理
  getSlackData <- reactive({
    params <- getParams()
    
    if(is.na(params$token) || is.na(params$channel) || 
       is.null(params$token) || is.null(params$channel) || 
       length(params$token) == 0 || length(params$channel) == 0){
      return(NA)
    }
    
    resp <- POST(endPoint.history, 
                body = params,
                encode = "multipart",
                content_type = "application/x-www-form-urlencoded"
#                ,verbose()
                )
    jsonFromSlack <- content(resp, "parsed")
    
    json2row <- ifelse(input$chk_custom, json2row.custom, json2row.defult)
    makeData(jsonFromSlack, json2row)
  })
  
  # グラフタブ
  ## 全体の集計
  output$table_all <- renderTable({
    tbl <- getSlackData()
    
    check <- validateData(tbl)
    
    if(check$is_error){
      return(check$message)
    }
    
    ret <- as.data.frame(table(tbl[,"user"]))
    names(ret)<- c("key","count")
    
    ret <- ret[order(ret$count,decreasing = T),]
    rbind(ret,data.frame(key = "合計", count = sum(ret$count) ))
  })
  
  ## 全体の集計グラフ
  output$graph_all <- renderPlot({
    tbl <- getSlackData()

    check <- validateData(tbl)
    
    if(check$is_error){
      return(check$message)
    }
    
    ret <- sort(table(tbl[,"user"]))

    par(family = "HiraKakuProN-W3")
    pie(ret)
  })
  
  ## 絞り込みの集計表
  output$table_filterd <- renderTable({
    tbl <- getSlackData()
    
    check <- validateData(tbl)
    if(check$is_error){
      return(check$message)
    }
    
    filter <- grep(pattern = input$refine, tbl[,"search"])
    if(sum(filter)==0){
      return("0件です。")
    }
    
    ret <- as.data.frame(table(tbl[filter,"user"]))
    names(ret)<- c("key","count")
    
    ret <- ret[order(ret$count,decreasing = T),]
    ret <- ret[ret$count>0,]
    rbind(ret,data.frame(key = "合計", count = sum(ret$count) ))
    
  })
  
  ## 絞り込みの集計グラフ
  output$graph_filterd <- renderPlot({
    tbl <- getSlackData()
    
    check <- validateData(tbl)
    
    if(check$is_error){
      return(check$message)
    }
    
    filter <- grep(pattern = input$refine, tbl[,"search"])
    if(sum(filter)==0){
      return("0件です。")
    }
    
    ret <- sort(table(tbl[filter,"user"]))

    par(family = "HiraKakuProN-W3")
    pie(ret)
  })
  
  # 一覧表タブ
  output$list <- renderTable({
    tbl <- getSlackData()
    
    check <- validateData(tbl)
    
    if(check$is_error){
      return(check$message)
    }
    
    filter <- grep(pattern = input$refine, tbl[,"search"])
    ret <- tbl[filter,(1:ncol(tbl)-1)]

    ret
  })
  
  # CSVタブ
  output$csv <- renderText({
    tbl <- getSlackData()
    
    check <- validateData(tbl)
    
    if(check$is_error){
      return(check$message)
    }
    
    paste("CSVダウンロードボタンつくる？")
  })
  
})
