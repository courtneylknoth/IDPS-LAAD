rescale <- function (df, digits = 2, na.rm = TRUE) {
  
  if(class(df) %in% 'data.frame') message('object df must be a vector')
  rng <- range(df, na.rm = TRUE)
  scaled <- (df-rng[1])/(rng[2]-rng[1])
  round(scaled, digits = digits)

}
