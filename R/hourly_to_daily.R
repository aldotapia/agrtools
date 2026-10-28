#' Convert hourly data to daily data
#'
#' This function converts hourly data to daily data. The function can calculate the daily data based on the mean, sum, min, max, standard deviation, variance, and number of observations based on the suffix of each column.
#'
#' @param df data.frame, the data frame with hourly data
#' @param datecol character, the name of the column with the date. It can be a Date, a date-time (POSIXct) or a character in 'YYYY-MM-DD HH:MM:SS' format
#' @param func character or function, the function to calculate the daily data, it can be 'min', 'max', 'mean' or any other function
#' @param use_suffix logical, if TRUE the function will calculate the daily data based on the suffix of each column
#' @param no_suffix_func character, the function to calculate the daily data when the column has no suffix
#' @param na.rm logical, if TRUE the function will remove NA values
#' @param func_suffix character, the suffix of each function to identify. By default these are: 'min', 'max', 'mean', 'sum', 'sd', 'var', 'n'
#'
#' @return data.frame, the data frame with daily data. Only numeric columns are aggregated.
#'
#' @examples
#' data("piscoelqui")
#' df = piscoelqui$data
#' df = hourly_to_daily(df, datecol = "date", use_suffix = TRUE)
#'
#' df = hourly_to_daily(df, datecol = "date", func = "mean", use_suffix = FALSE)
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

  df = as.data.frame(df)
  df = df[!duplicated(df[[datecol]]), ]
  df[[datecol]] = tryCatch(lubridate::date(df[[datecol]]), error = function(e) {
    stop("datecol should be a Date, POSIXct or 'YYYY-MM-DD HH:MM:SS' character")
  })
  df = df[!is.na(df[[datecol]]), ]

  cols = setdiff(names(df), datecol)
  cols = cols[vapply(df[cols], is.numeric, logical(1))]
  funs = rep(list(func), length(cols))
  if(use_suffix){
    suffix = gsub('.*_', '', cols)
    suffix[!(suffix %in% func_suffix)] = no_suffix_func
    funs = as.list(suffix)
  }

  days = sort(unique(df[[datecol]]))
  dfo = data.frame(days)
  names(dfo) = datecol
  for(i in seq_along(cols)){
    f = if(identical(funs[[i]], 'n')) function(x, na.rm) sum(!is.na(x)) else match.fun(funs[[i]])
    dfo[[cols[i]]] = as.vector(tapply(df[[cols[i]]], df[[datecol]], f, na.rm = na.rm))
  }
  return(dfo)
}
