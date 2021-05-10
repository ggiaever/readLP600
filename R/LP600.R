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
read_LP600 = function(file){
  library(reshape2)
  # filename = readr::read_lines(file,n_max = 1)
  # wells = readr::read_lines(file,skip = 3,n_max=1)
  # annot = readr::read_lines(file,skip = 4,n_max=1)
  x = readr::read_delim(file,delim="\t",skip=27,col_names = TRUE )
  ncoln = ncol(x)

  names(x)[1] = "runtime"

  x$runtime = as.numeric(x$runtime)

  mx = reshape2::melt(x,id.vars = "runtime",value.name = "measure",variable.name = "well")
  mx$plate = 0
  mx$well = as.character(mx$well)
  w=which(nchar(mx$well)==2)
  s = substr(mx$well[w],1,1)
  s2 = substr(mx$well[w],2,2)
  s=paste0(s,0)
  s=paste0(s,s2)
  mx$well[w]=s
  mx$plate = 0
  mx = mx[,c("plate", "well",  "runtime", "measure")]
  mx
}
