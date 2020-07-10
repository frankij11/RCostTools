



#' Learning Curve Function
#'
#' @param T1 numeric value
#' @param LC decimal value
#' @param RC
#' @param Qty
#' @param rate
#'
#' @return
#' @export
#'
#' @examples
learn_curve <- function(T1, LC, RC, Qty, rate=1){
  return(T1*Qty^(log(LC)/log(2))* rate^(log(RC)/log(2)))
}


#' Title
#'
#' @param f first unit of lot
#' @param l last unit of lot
#' @param LC learning curve parameter (optional)
#' @param b learning curve coefficient (optional)
#'
#' @return
#' @export
#'
#' @examples
lc_midpoint <- function(f,l, LC=1, b=log(LC)){
  if(b==0){
    mid <- (f + l + 2 * (f*l) ^.5)/4
  }else{
    mid <- 1 # Asher's Lot midpoint not implemented
  }

  return( mid )
}



lc_prepare <- function(df, groups, priors, qty){

 lc <- df %>% group_by(groups) %>%
  summarise(shared_qty = sum(qty, na.rm=T),
            priors = mean(priors, na.rm=T)
            ) %>%
  mutate(Last = cumsum(shared_qty) + priors,
         First = Last - shared_qty + 1,
         midpoint = midpoint(First,Last)
  )


 return(lc)

}
