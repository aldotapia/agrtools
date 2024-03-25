#' Convert hourly data to daily data
#'
#' This function converts hourly data to daily data. The function can calculate the daily data based on the mean, sum, min, max, standard deviation, variance, and number of observations based on the suffix of each column.
#'
#' @param df data.frame, the data frame with hourly data
#' @param datecol character, the name of the column with the date
#' @param func character, the function to calculate the daily data, it can be 'min', 'max', 'mean' or any other function
#' @param use_suffix logical, if TRUE the function will calculate the daily data based on the suffix of each column
#' @param no_suffix_func character, the function to calculate the daily data when the column has no suffix
#' @param na.rm logical, if TRUE the function will remove NA values
#' @param func_suffix character, the suffix of each function to identify. By default these are: 'min', 'max', 'mean', 'sum', 'sd', 'var', 'n'
#'
#' @return data.frame, the data frame with daily data
#'
#' @examples
#' data("piscoelqui")
#' df = piscoelqui$data
#' df = hourly_to_daily(df, date = "date", use_suffix = TRUE)
#'
#' df = hourly_to_daily(df, date = "date", func = "mean", use_suffix = FALSE)
#'
#' @export
hourly_to_daily <- function(df, datecol='date',
                            func = 'mean',
                            use_suffix = FALSE,
                            no_suffix_func = 'sum',
                            na.rm = TRUE,
                            func_suffix = c('min', 'max', 'mean', 'sum', 'sd', 'var', 'n')){

  if(!(datecol %in% names(df))){
    stop("datecol should be in df")
  }

  df = df[order(df[['date']]),]
  df = df[!duplicated(df[['date']]),]
  if(na.rm){
    df = df[!is.na(df[['date']]),]
  }

  if (!lubridate::is.Date(df[[datecol]]) | !lubridate::is.POSIXct(df[[datecol]]) |
      !lubridate::is.POSIXt(df[[datecol]]) | !lubridate::is.POSIXlt(df[[datecol]])) {
    if (is.character(df[[datecol]])) {
      if (sum(grepl(pattern = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", x = df[[datecol]])) == length(df[[datecol]])) {
        df[['date']] = lubridate::date(df[[datecol]])
      } else{
        stop("date should be in YYYY-MM-DD format")
      }
    }
  }

  if(datecol != 'date'){
    df = df[,names(df) != datecol]
  }

  if(use_suffix){
   cols = names(df)[names(df) != 'date']
   colsf = gsub('.*_', '', cols)
   colsf[!(colsf %in% func_suffix)] = no_suffix_func
   dfl = list()
   ctype = sapply(df[,names(df) %in% cols], function(x) class(x))

   dfg = df %>%
     dplyr::group_by(date)
   for(i in 1:length(cols)){
     if(ctype[i] %in% c('characther','factor')){
         df_temp = dplyr::summarise_at(mode = names(which.max(table(df[[cols[i]]])))) %>%
           dplyr::ungroup()
         names(df_temp) = c(datecol, cols[i])
         dfl[[i]] = df_temp
     }
     if(ctype[i] %in% c('numeric','integer')){
         df_temp = dplyr::summarise_at(dfg, cols[i], colsf[i]) %>%
           dplyr::ungroup()
         names(df_temp) = c(datecol, cols[i])
         dfl[[i]] = df_temp
     }
   }
   dfo = Reduce(function(...) merge(..., by=datecol, all.x=TRUE), dfl)
  }else{
    dfo = df %>%
      dplyr::group_by(date) %>%
      dplyr::summarise_all(func, na.rm = na.rm) %>%
      dplyr::ungroup()
  }
  return(dfo)
}
