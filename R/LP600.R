# Hello, LP600!
#
# This is an example function named 'read_LP600'
# which reads Tecan Magellan Spec Data'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#   function for reading OD600 from LP600 .txt files into AUDIT
read_oneplate <- function(file){
  library(reshape2)
  plate = readr::read_lines(file,skip=8,n_max = 1)
  plate =gsub("  Plate Number:            ","",plate)
  plate = as.numeric(plate)

  x = readr::read_delim(file,delim="\t",skip=27,col_names = TRUE )
  ncoln = ncol(x)

  names(x)[1] = "runtime"

  x$runtime = as.numeric(x$runtime)

  mx = reshape2::melt(x,id.vars = "runtime",value.name = "measure",variable.name = "well")
  mx$plate = plate
  mx$well = as.character(mx$well)
  w=which(nchar(mx$well)==2)
  s = substr(mx$well[w],1,1)
  s2 = substr(mx$well[w],2,2)
  s=paste0(s,0)
  s=paste0(s,s2)
  mx$well[w]=s
  
  Res <- as.POSIXlt(paste(Sys.Date(), mx$runtime))
  res=Res$hour*3600+Res$min*60+Res$sec

  mx$runtime = res
  
  
  

  mx = mx[,c("plate", "well",  "runtime", "measure")]
  mx
}

read_LP600 <- function (file) {
	 read_oneplate <- function (file) {
    library(reshape2)
    plate = readr::read_lines(file, skip = 8, n_max = 1)
    plate = gsub("  Plate Number:            ", "", plate)
    plate = as.numeric(plate)
    x = readr::read_delim(file, delim = "\t", skip = 27, col_names = TRUE)
    ncoln = ncol(x)
    names(x)[1] = "runtime"
    x$runtime = as.numeric(x$runtime)
    mx = reshape2::melt(x, id.vars = "runtime", value.name = "measure", 
        variable.name = "well")
    mx$plate = plate
    mx$well = as.character(mx$well)
    w = which(nchar(mx$well) == 2)
    s = substr(mx$well[w], 1, 1)
    s2 = substr(mx$well[w], 2, 2)
    s = paste0(s, 0)
    s = paste0(s, s2)
    mx$well[w] = s
    
    mx = mx[, c("plate", "well", "runtime", "measure")]
    mx
}
	
###################	
	
	 library(reshape2)
	 library(readr)
	 library(stringr)
	 
	 lines = readr::read_lines(file)
	 
	 lens = length(lines)
	 
	 
	 findpreamble = "Field Group"
	 findODstart =  "Time"
	 wpreamble = NULL
	 wstart = NULL
	 
	 for(i in 1:lens)  wpreamble[[i]] = stringr::str_locate(lines[i],findpreamble)
	 for(i in 1:lens)  wstart[[i]] = stringr::str_locate(lines[i],findODstart)
	 names(wstart) = 1:lens
	 names(wpreamble) = 1:lens
    
	 wna= sapply(wstart,function(x) all(is.na(x)))
   ww = which(wna==T)

	 wstart = wstart[-ww]
	 
	 wna= sapply(wpreamble,function(x) all(is.na(x)))
   ww = which(wna==T)
	 
	 wpreamble = wpreamble[-ww]
	 
	 npreamble = as.numeric(names(wpreamble))
	 nstart = as.numeric(names(wstart))
	 
	 diff = nstart - npreamble + 3
	 
	 liff = length(diff)
	 if(liff!=1) {
	 	
	 n_max = npreamble[2]-diff[1]
	 
	 ######### read in the plates one-by-one
	 skip = nstart - 1
	 
	 plate = NULL
	 
	 for(i in 1:liff) plate[[i]] = readr::read_delim(file,skip = skip[i],n_max = n_max, delim = "\t")
	 
	 ######## add plate names
	 
	 f = function(x,y) {
	 	x[,"plate"] = y
	 	x
	 }
	 namofplate = as.list(1:liff)
	 for(i in 1:liff) plate[[i]]$plate = namofplate[i]
	 
	 
	 plate3 = do.call(rbind,plate)
	 
	 w = which(names(plate3) == "Time")
	 names(plate3)[w] = "runtime"
   
   
   mx = reshape2::melt(plate3, id.vars = c("runtime","plate"), value.name = "measure", 
        variable.name = "well")
   
   
   
   mx$well = as.character(mx$well)
   
   w = which(nchar(mx$well) == 2)
   s = substr(mx$well[w], 1, 1)
   s2 = substr(mx$well[w], 2, 2)
   s = paste0(s, 0)
   s = paste0(s, s2)
   mx$well[w] = s
   
   mx$runtime = as.numeric(mx$runtime)
   # Res <- as.POSIXlt(paste(Sys.Date(), mx$runtime))
   # res = Res$hour * 3600 + Res$min * 60 + Res$sec
   # mx$runtime = res
   
   mx = mx[, c("plate", "well", "runtime", "measure")]
   mx
	 } else {
	 	mx = read_oneplate(file)
	 	mx}
	 mx
}