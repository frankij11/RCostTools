

#' Rayleigh Distribution
#'
#' @param StartDate
#' @param Duration
#' @param Cost
#' @param delta
#' @param df
#'
#' @return
#' @export
#'
#' @examples
phase_rayleigh <- function(StartDate, Duration, Cost, delta=0.03, return_df=T){
  s = (Duration ^ 2 / (abs(log(delta)) * 2)) ^ 0.5
  norm_cost = Cost / (1 - delta)

  EndDate = StartDate + Duration - 0.0000001
  yrs = floor(EndDate) - floor(StartDate) + 1

  results  = data.frame(
    FY = seq(floor(StartDate),length.out=yrs)) %>% mutate(

      StartDate = StartDate,
      EndDate=EndDate,
      T0 = seq(0, length.out=yrs) / Duration,
      T1 = lead(T0,n=1, default=1),

      CDF1 = 1 - exp(-((T1 * Duration) ^ 2) / (2 * s ^ 2)),
      CDF0 = 1 - exp(-((T0 * Duration) ^ 2) / (2 * s ^ 2)),
      PDF = CDF1 - CDF0,
      Estimate = PDF * norm_cost)

  if(return_df){return(results)}else{results$Estimate}


}
