# 関数いろいろ

requireLibs <- function(libs) {
  for(lib in libs){
    if(!require(lib, character.only = T)){
      install.packages(lib)
      require(lib, character.only = T)
    }
  }
}

# nullチェック
getVal <- function(obj){
  if(is.null(obj) || is.na(obj)){
    return("")
  }else{
    return(obj)
  }
}

# 雑にチェック
validateData <- function(tbl){
  ret <- data.frame(
    is_error = F,
    message = ""
  )
  
  if(is.null(tbl)){
    ret$is_error = T
    ret$message = "データが空です。"
  }else if(is.na(tbl)){
    ret$is_error = T
    ret$message = "エラーで取得できなかったみたい。"
  }else if(nrow(tbl)==0){
    ret$is_error = T
    ret$message = "0件です。"
  }
  
  return(ret)
}


######
# 拡張する場合は変更するとこ
######

# slackから受け取ったjsonを１メッセージごとに処理するとこ
json2row <- function(message){
  dat <- 
    c(getVal(message$type),
      getVal(message$user),
      getVal(message$text),
      format(
        as.POSIXct(as.numeric(message$ts), origin="1970-01-01"),
        "%Y/%m/%d"
      ),
      format(
        as.POSIXct(as.numeric(message$ts), origin="1970-01-01"),
        "%H:%M:%S"
      ))
  names(dat) <- c("type", "user", "text", "年月日", "時分秒")
  
  dat
}

# slackから受け取ったjsonを処理するとこ
makeData <- function(jsonFromSlack){
  ret <- NA
  if(is.list(jsonFromSlack[["messages"]])){
    json <- jsonFromSlack[["messages"]]
    
    ret <- do.call(rbind,
                   lapply(json,json2row)
    ) 
    if(!is.null(ret) && !is.na(ret)){
      ret <- ret %>% as.data.frame
    }else{
      ret <- NA
    }
  }
  return(ret)
}
