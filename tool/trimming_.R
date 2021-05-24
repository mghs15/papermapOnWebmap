#地図画像のトリミング（古い地図の装飾向け）

library("jpeg")

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
    oT_target_row <- oT[c(floor(nRow*0.15):floor(nRow*0.85)),]
    sdCol <- apply(oT_target_row, 2, sd)
    muCol <- apply(oT_target_row, 2, mean)
    plot(sdCol, type="b", main=paste("col_sd", name), ylim=c(0,1))
    points(muCol, type="b", main="col_mean", col="red")
    
    hintCol <- pickupSdMean(sdCol, muCol)
    nnc <- length(hintCol)
    abline(v=hintCol, col="blue")
    startCol <- hintCol[1] + 15
    endCol <- hintCol[nnc] - 15
    
    #Row
    oT_target_col <- oT[,c(floor(nCol*0.15):floor(nCol*0.85))]
    sdRow <- apply(oT_target_col, 1, sd)
    muRow <- apply(oT_target_col, 1, mean)
    plot(sdRow, type="b", main=paste("row_sd", name), ylim=c(0,1))
    points(muRow, type="b", main="row_mean", col="red")
    
    hintRow <- pickupSdMean(sdRow, muRow)
    nnr <- length(hintRow)
    abline(v=hintRow, col="blue")
    startRow <- hintRow[1] + 15
    endRow <- hintRow[nnr] -15
    

    trim <- jpg[c(startRow:endRow),c(startCol:endCol),]
    newname <- sub("/image.jpg", ".jpg", name)
    writeJPEG(trim, newname)
  },
  error = function(err){
    print(err)
  })
}


pickupSdMean <- function(sd, mu){
  v <- NULL
  n <- length(mu)
  nh <- floor(n/2)
  threshhold <- max(min(mu[5:nh]), min(mu[(nh+1):(n-5)]))
  for(i in 1:n){
    if((i > 5) && (i < n-5)
       && (mu[i] <= threshhold) 
       #&& (sd[i]<sd[i-1]) && (sd[i]<sd[i+1])  
       && (mu[i]<mu[i-1]) && (mu[i]<mu[i+1])
       ){
      v <- c(v, i)
    }
  }
  
  check <- max(v) - min(v)
  #地図の領域が紙面全体の半分以下はないという仮定
  #半分以下なら、検出がうまくいっていないので、素直な抽出を行う
  if( check < n*0.5 ){ 
    print("a sub method used")
    v <- pickupSdMeanSub(sd, mu)
  }
  
  check <- max(v) - min(v)
  #地図の領域が紙面全体の半分以下はないという仮定
  #半分以下なら、検出がうまくいっていないので、素直な抽出を行う
  if( check < n*0.5 ){ 
    print("a loose method used")
    v <- pickupSdMeanLoose(sd, mu)
  }
  
  return(v)
}


pickupSdMeanSub <- function(sd, mu){
  v <- NULL
  n <- length(mu)
  threshhold <- mean(mu)
  for(i in 1:n){
    if((i > 5) && (i < n-5)
       && (mu[i] <= threshhold) 
       && (sd[i]<sd[i-1]) && (sd[i]<sd[i+1])  
       && (mu[i]<mu[i-1]) && (mu[i]<mu[i+1])
    ){
      v <- c(v, i)
    }
  }
  
  return(v)
}

pickupSdMeanLoose <- function(sd, mu){
  v <- NULL
  n <- length(mu)
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

checkSdMean <- function(sd, mu){
  v <- NULL
  n <- length(mu)
  nh <- floor(n/2)
  threshhold <- max(min(mu[5:nh]), min(mu[(nh+1):(n-5)]))
  for(i in 1:n){
    if(((i > 5 && i < n-5))
       && (mu[i] <= threshhold) 
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




