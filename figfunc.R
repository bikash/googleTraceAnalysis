biplot.features <- function(x, robust = TRUE, col, ...) {
  nc <- nrow(x)
  naomit.x <- na.omit(x)
  na.act <- na.action(naomit.x)
  if (is.null(na.act)) {
    avl <- 1:nc
  } else {
    avl <- (1:nc)[-na.action(naomit.x)]
  }
  if (missing(col)) {
    col <- c("#000000", "darkred")
  } else {
    lencol <- length(col)
    if (lencol == 1L) {
      col <- rep(col, 2)
    } else {
      col <- unique(col)[1:2]
    }
  }
  if (robust) {
    rbt.pca <- pcaPP::PCAproj(naomit.x, k = 2, scale = sd, center = mean)
  } else {
    rbt.pca <- princomp(naomit.x, cor = TRUE)
  }
  biplot(rbt.pca, col = col, xlabs = avl, xlab = "PC1", ylab = "PC2", 
         cex = c(0.8, 1), ...)
}


#'  Sanitization of System Datasets
#'
#'  Sanitizing  time series data from utitlity providers.
#'  Input is a data frame with columns cpurate, memory_usage, page_cache, diskio_time, cycle_inst,start_date,end_date
#'  @name unplugg_sanitize
#'  @param input_df Data frame with the following columns at a minimum
#'    TYPE: type of data: Ex 'Electric Usage'
#'    DATE: date of activity, i.e. each row entry
#'    START TIME: start time of each entry
#'    USAGE: usage for current activity, i.e. count
#'    UNITS: units of usage
#'  @return returned value is a data frame with columns TYPE, DATE, TIME, USAGE, UNITS and TIMESTAMP
#'   
#'  Currently optimized for PGE data available through Opower
#'  

unplugg_sanitize <- function(input_df) {
  
  # Install necessary package(s)
  library(lubridate)
  names(input_df) <- tolower(names(input_df))
  # Extract and append timestamp
  input_df$timestamp = as.POSIXct(input_df$start_date, origin="1970-01-01")
  input_df$etimestamp = as.POSIXct(input_df$end_date, origin="1970-01-01")
  #input_df$timestamp = parse_date_time(paste(input_df$start_date, ' ', input_df$start.time), 'mdy hm')
  
  return(input_df)
}
#unplugg_sanitize(Data1)
