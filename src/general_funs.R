' 
 Script: "General functions"

 Author: Valeria Bonapersona
 Contact: v.bonapersona-2 (at) umcutrecht.nl

'


# upload data -------------------------------------------------------------
# Function by Jeromy Anglim from https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


# Function to read only sheets of interest from files 
read_my_sheets <- function(filename, sheets, tibble = FALSE) {
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}



# Harmonize summary stats ---------------------------------------------------
make_numeric <- function(x) {
  return(suppressWarnings(as.numeric(as.character(x))))
}

median_to_mean <- function(median, iqr) {
  # check inputs are numeric
  median <- make_numeric(median)
  
  # transform iqr to low and high
  iqr_low <- str_remove_all(iqr, "_.*") %>% as.character() %>% as.numeric()
  iqr_high <- str_remove_all(iqr, ".*_") %>% as.character() %>% as.numeric()
  
  # function from Hozo (2005)
  (iqr_low + 2*median + iqr_high) / 4
}

iqr_to_sd <- function(median, iqr) {
  
  # check inputs are numeric
  median <- make_numeric(median)
  
  # transform iqr to low and high
  iqr_low <- str_remove_all(iqr, "_.*") %>% as.character() %>% as.numeric()
  iqr_high <- str_remove_all(iqr, ".*_") %>% as.character() %>% as.numeric()
  
  
  # calculate var according to Hozo (2005) formula
  my_var <- 1/12 * (
    (((iqr_low -2*median + iqr_high)^2)/4) + 
      ((iqr_high - iqr_low)^2)
    )
  
  # sd is the sqrt of the variance
  sqrt(my_var)
}

sem_to_sd <- function(sem, n) {
  sem <- make_numeric(sem)
  n <- make_numeric(n)
  
  return(sem*sqrt(n))
}

get_mean_range <- function(my_range) {
  
  if(str_detect(my_range, "_")) {
    range_low <- str_remove_all(my_range, "_.*") %>% as.character() %>% as.numeric()
    range_high <- str_remove_all(my_range, ".*_") %>% as.character() %>% as.numeric()
    my_mean <- mean(c(range_low, range_high))
    
  } else {
    my_mean <- make_numeric(my_range)
  }
  
  return(my_mean)
  
}



# Analysis ----------------------------------------------------------------

t.test2 <- function(m1,m2,var1,var2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (var1/n1) + (var2/n2) )
    # welch-satterthwaite df
    df <- ( (var1/n1 + var2/n2)^2 )/( (var1/n1)^2/(n1-1) + (var2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*var1 + (n2-1)*var2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat[["p-value"]]) 
}


