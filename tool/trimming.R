#地図画像のトリミング（古い地図の装飾向け）

library("jpeg")
library("png")
library("imager")
library("rsvg")

#df <- read.csv("out_WestGunma.json.csv", header = FALSE)


path <- "C:/your-path"
flist <- list.files(paste(path, "set", sep="/"), recursive=TRUE, pattern="jpg", full.names=TRUE)

for(name in flist){

  tryCatch( {
    #name = "C:/your-path/set/1376448/image.jpg"
    print(name)
    
    jpg <- readJPEG(name)
    
    oR <- jpg[,,1]
    oG <- jpg[,,2]
    oB <- jpg[,,3]  
    #oT <- (oR + oG + oB)/3
    oT <- 0.299*oR + 0.587*oG + 0.114*oB 
    
    nRow = nrow(oT)  
    nCol = ncol(oT)
    
    #Col
    oT_target_row <- oT[c(floor(nRow*0.2):floor(nRow*0.8)),]
    sdCol <- apply(oT_target_row, 2, sd)
    muCol <- apply(oT_target_row, 2, mean)
    plot(sdCol, type="b", main=paste("col_sd", name), ylim=c(0,1))
    points(muCol, type="b", main="col_mean", col="red")
    
    sdMinCol <- checkSdMean(sdCol, muCol)
    points(sdMinCol, type="b", main="col_check", col="blue")
    hintCol <- pickupSdMean(sdCol, muCol)
    nnc <- length(hintCol)
    startCol <- hintCol[1] + 15
    endCol <- hintCol[nnc] - 15
    
    #Row
    oT_target_col <- oT[,c(floor(nCol*0.2):floor(nCol*0.8))]
    sdRow <- apply(oT_target_col, 1, sd)
    muRow <- apply(oT_target_col, 1, mean)
    plot(sdRow, type="b", main=paste("row_sd", name), ylim=c(0,1))
    points(muRow, type="b", main="row_mean", col="red")
    
    sdMinRow <- checkSdMean(sdRow, muRow)
    points(sdMinRow, type="b", main="row_check", col="blue")
    hintRow <- pickupSdMean(sdRow, muRow)
    nnr <- length(hintRow)
    startRow <- hintRow[1] + 15
    
    #endRow <- hintRow[nnr] -15
    
    #checkR <- hintRow[nnr] - hintRow[1] 
    #スケールバー対応(微妙)
    #白の要素の数を数えた方が良いかも？
    oT_check_col <- oT_target_col[,c(c(0:10))]
    muRow_check <- apply(oT_check_col, 1, mean)
    if(muRow_check < 0.7){
      endRow <- hintRow[nnr] -15
    }else{
      endRow <- hintRow[nnr] -30
    }
    
    
    trim <- jpg[c(startRow:endRow),c(startCol:endCol),]
    newname <- sub("/image.jpg", ".jpg", name)
    writeJPEG(trim, newname)
  },
  error = function(err){
    print(err)
  })
}

checkSdMean <- function(sd, mu){
  v <- NULL
  n <- length(sd)
  threshhold <- min(mean(mu), 0.7)
  for(i in 1:n){
    if(((i > 5 && i < n-5))
       && (mu[i] < threshhold) 
       #&& (sd[i]<sd[i-1]) && (sd[i]<sd[i+1])  
       && (mu[i]<mu[i-1]) && (mu[i]<mu[i+1])
       ){
      v <- c(v, 1)
    }else{
      v <- c(v, 0)
    }
  }
  
  return(v)
}

pickupSdMean <- function(sd, mu){
  v <- NULL
  n <- length(sd)
  threshhold <- min(mean(mu), 0.7)
  for(i in 1:n){
    if((i > 5) && (i < n-5)
       && (mu[i] < threshhold) 
       #&& (sd[i]<sd[i-1]) && (sd[i]<sd[i+1])  
       && (mu[i]<mu[i-1]) && (mu[i]<mu[i+1])
       ){
      v <- c(v, i)
    }
  }
  
  check <- max(v) - min(v)
  
  if( check < n*0.5 ){
    v <- pickupSdMeanSub(sd, mu)
  }
  
  return(v)
}


pickupSdMeanSub <- function(sd, mu){
  v <- NULL
  n <- length(sd)
  for(i in 1:n){
    if((i > 5) && (i < n-5)
       && (sd[i]<sd[i-1]) && (sd[i]<sd[i+1])  
       && (mu[i]<mu[i-1]) && (mu[i]<mu[i+1])
    ){
      v <- c(v, i)
    }
  }
  
  return(v)
}

#----------------------------------------------------------------------------

pickupMax2 <- function(vector) {
  max1 <- 0
  max2 <- 0
  
  for(i in vector){
    a <- vector[i]
    if(a > max1){
      max1 <- a
    }else if(a > max2){
      max2 <- a
    }
  }
  return(max2)
}

diffCalc <- function(vector, interval) {
  v <- NULL
  n <- length(vector) - interval
  for(i in 1:n){
    diff <- abs(vector[i] - vector[i+interval])
    v <- c(v, diff)
  }
  return(v)
}


calcIntervalMin <- function(vector, interval) {
  minVal <- NULL
  n <- length(vector)
  for(i in (interval+1):n){
    start <- i - interval + 1
    end <- i
    target <- vector[start:end]
    minValue <- min(target)
    minVal = c(minVal, minValue)
  }
  return(minVal)
}




