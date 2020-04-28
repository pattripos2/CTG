library(plumber)
apr <- plumber::plumb("myAPI.R")
apr$run(port=3551)

