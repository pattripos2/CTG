library(urltools)
library(curl)
library(jsonlite)

# presumes no quotes in rawtext itself
#
JeffNCIt <- function(rawtext="Temozolomide and glioblastoma and bears, oh my") {
  mystring=sprintf("http://trex.xcures.com:4242/ncitbindings?s=%s",rawtext)
  mystring=URLencode(mystring)
  suppressWarnings(  {
     con = curl( mystring,"r")
    fu=readLines(con)
    close(con)
  }  )

  fromJSON(fu)
}

