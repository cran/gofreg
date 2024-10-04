km_features <- function(z, delta, dist1 = FALSE) {
  # modification to create a proper distribution function with F(inf) = 1
  if(dist1) {
    delta[which.max(z)] <- 1
  }

  # compute KM survival function, distribution function and weights
  km <- survival::survfit(survival::Surv(z, delta) ~ 1)
  km.S <- km$surv
  km.D <- 1 - km.S
  km.W <- km.D - c(0, km.D[-length(km.D)])

  # get Z and delta vectors in the same order as the distribution function
  # (i.e. unique and ordered Z values with corresponding deltas)
  z.new <- km$time
  delta.new <- 1 * (km$n.event >= 1)

  # return results
  km_obj <- list(
    z = z.new, delta = delta.new,
    dkm = km.W,
    pkm = km.D,
    skm = km.S
  )
  return(km_obj)
}

rkm <- function(km_obj, n = 1) {
  # extract uncensored data
  cen_data <- data.frame(z = km_obj$z, pkm = km_obj$pkm, delta = km_obj$delta)
  uncens <- cen_data[cen_data$delta == 1, ]

  # create sample according to KM distribution function using Skorokhod 2
  smpl <- replicate(n, uncens$z[which(uncens$pkm >= stats::runif(1))[1]], simplify = "vector")
  return(smpl)
}
