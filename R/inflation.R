#library(tidyverse)

#load jic
# jic<-NULL
# try({
#   jic <- read.csv('../data/inflation.csv')
#   })
# if(is.null(jic) ){jic <- read.csv('data/inflation.csv')}
#
# #clean
# jic <- jic %>% gather(key="Year", value="Value",-(Version:Indice)) %>%
# 	pivot_wider(names_from=Type, values_from=Value)
# jic$Year <- substring(jic$Year, 2) %>% as.numeric()

#get tags
inflation_meta <- function(){
jic_ver <- jic %>% dplyr::select(Version) %>% unique() %>% c()
jic_indices <- jic %>% dplyr::select(Indice) %>% unique() %>% as.list()
jic_service <- list(Navy = "n", USMC="m", DOD = "d")
jic_tags <- jic %>% dplyr::select(tags) %>% unique()

return(list(jic_ver, jic_indices, jic_service, jic_tags))
}


#' Get copy of inflation tables
#' @param BY get year for inflation indice
#'
#' @return
#' @export
#'
#' @examples
jic <- function(BY=2020){
  return(change_by(2020))
}

#' Title
#'
#' @param BY: Base Year
#' @param inflation_table: default is to use latest jic
#'
#' @return data frame
#' @export
#'
#' @examples
change_BY <- function(BY=2020, inflation_table=jic){

  jic.by <- inflation_table %>%
    dplyr::filter(Year==BY) %>%
    dplyr::mutate(Raw.BY = Raw) %>%
    dplyr::select(-Raw, -Weighted, -Year)

  df <- inflation_table %>%
    dplyr::left_join(jic.by) %>%
    dplyr::mutate(
      Raw = Raw/Raw.BY,
      Weighted = 1 / Raw.BY * Weighted) %>%
    dplyr::select(-Raw.BY)

  return(df)
}

#' Master Inflation Function
#'
#' @param Index name of inflation indices
#' @param FromYR
#' @param ToYR
#' @param Cost
#' @param from_type
#' @param to_type
#' @param inflation_table
#'
#' @return
#' @export
#'
#' @examples
inflation <- function (Index, FromYR, ToYR, Cost,from_type ="BY", to_type = "BY",inflation_table = NULL){
  if(is.null(inflation_table)){inflation_table <- jic }

  #'nToYR = ToYR
  #If(ToYR > max(jic$Year)){ToYR <- max(jic$Year)}
  df = data.frame(Index = Index,FromYR = FromYR, ToYR =ToYR, Cost=Cost)

  df_from = df %>%
    dplyr::left_join(inflation_table, by=c("Index"="Indice", "FromYR" = "Year"))

  df_to = df %>%
    dplyr::left_join(inflation_table, by=c("Index"="Indice", "ToYR" = "Year"))


  div_by <- switch(from_type, BY = df_from$Raw, TY = df_from$Weighted)
  mult_by <- switch(to_type, BY = df_to$Raw, TY = df_to$Weighted)

  result = (df$Cost / div_by) * mult_by
  #result = result[[1]]

  return(result)

}

#' Title
#'
#' @param Index
#' @param FromYR
#' @param ToYR
#' @param Cost
#' @param inflation_table
#'
#' @return
#' @export
#'
#' @examples
BYtoBY <- function(Index, FromYR, ToYR, Cost, inflation_table = NULL){
  return(inflation(Index,FromYR,ToYR, Cost, "BY", "BY", inflation_table))
}

#' Title
#'
#' @param Index
#' @param FromYR
#' @param ToYR
#' @param Cost
#' @param inflation_table
#'
#' @return
#' @export
#'
#' @examples
BYtoTY <- function(Index, FromYR, ToYR, Cost, inflation_table = NULL){
  return(inflation(Index,FromYR,ToYR, Cost, "BY", "TY", inflation_table))
}

#' Title
#'
#' @param Index
#' @param FromYR
#' @param ToYR
#' @param Cost
#' @param inflation_table
#'
#' @return
#' @export
#'
#' @examples
TYtoBY <- function(Index, FromYR, ToYR, Cost, inflation_table = NULL){
  return(inflation(Index,FromYR,ToYR, Cost, "TY", "BY", inflation_table))
}

#' Title
#'
#' @param Index
#' @param FromYR
#' @param ToYR
#' @param Cost
#' @param inflation_table
#'
#' @return
#' @export
#'
#' @examples
TYtoTY <- function(Index, FromYR, ToYR, Cost, inflation_table = NULL){
  return(inflation(Index,FromYR,ToYR, Cost, "TY", "TY", inflation_table))
}
