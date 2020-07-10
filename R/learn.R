



learn_curve <- function(T1, LC, RC, Qty, rate){
  return(T1*Qty^(log(LC)/log(2))* rate^(log(RC)/log(2)))
}


midpoint <- function(f,l, b=0){
  if(b==0){
    mid <- (f + l + 2 * (f*l) ^.5)/4
  }else{
    mid <- 1 # Asher's Lot midpoint not implemented
  }

  return( mid )
}



lc_prepare <- function(df, groups, priors, qty){

 lc = df %>% group_by(groups) %>%
  summarise(shared_qty = sum(qty, na.rm=T),
            priors = mean(X2018, na.rm=T)
            ) %>%
  mutate(Last = cumsum(shared_qty) + priors,
         First = Last - shared_qty + 1,
         midpoint = midpoint(First,Last)
  )


 return(lc)

}
