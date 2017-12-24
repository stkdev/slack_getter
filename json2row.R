# slackから受け取ったjsonを１メッセージごとに処理するとこ
json2row.default <- function(message){
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
  dat <- c(dat,dat[3])
  names(dat) <- c("type", "user", "text", "年月日", "時分秒", "search")
  
  dat
}


######
# 拡張する場合は変更するとこ
######

json2row.custom <- function(message){
  if(getVal(message$subtype) != "bot_message"){
    return(NA)
  }
  dat <-
    c(getVal(message$type),
      getVal(lapply(message$attachments[[1]]$fields,function(l){if(l$title=="企業名"){l$value}}) %>% unlist),
      getVal(lapply(message$attachments[[1]]$fields,function(l){if(l$title=="画面名"){l$value}}) %>% unlist),
      getVal(message$attachments[[1]]$pretext),
      format(
        as.POSIXct(as.numeric(message$ts), origin="1970-01-01"),
        "%Y/%m/%d"
      ),
      format(
        as.POSIXct(as.numeric(message$ts), origin="1970-01-01"),
        "%H:%M:%S"
      ))
  dat <- c(dat, paste(dat[2],dat[3],dat[4],sep=" "))
  names(dat) <- c("type", "user", "page", "text", "年月日", "時分秒", "search")
  
  dat
}

