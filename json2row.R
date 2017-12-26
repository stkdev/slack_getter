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
  json2row.default(message)
}

