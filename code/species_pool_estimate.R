species_pool_estimate <- function(data){
  
  require(tidyverse)
  require(vegan)
  
  #data is the rows (sites), columns (species)
  
  # Species accumulation using specaccum
  accumulation <- specaccum(data, method = "random")
  
  # Data for fitting
  accum_data <- data.frame(
    Sites = accumulation$sites,
    Richness = accumulation$richness,
    SD = accumulation$sd
  )
  
  # Fit a non-linear asymptotic model
  fit <- nls(
    Richness ~ SSasymp(Sites, Asym, R0, lrc),
    data = accum_data
  )
  
  # Extract parameters from the nls fit
  asymptote <- coef(fit)["Asym"]
  
  # Function to estimate number of sites needed to reach a percentage of the asymptote
  estimate_sites_for_threshold <- function(threshold, model, lrc) {
    # Solve for sites: Richness = threshold = Asym * (1 - exp(-exp(lrc) * Sites))
    solve_sites <- function(sites) {
      asymptote * (1 - exp(-exp(lrc) * sites)) - threshold
    }
    uniroot(solve_sites, c(0, 10000))$root  # Large upper bound for robust search
  }
  
  # Estimate sites needed for 99% of the asymptote
  threshold <- 0.99 * asymptote
  estimated_sites <- estimate_sites_for_threshold(threshold, fit, coef(fit)["lrc"])
  
  # Adjust extrapolated sites range based on estimated sites
  max_sites <- ceiling(estimated_sites)  # Ensure integer
  extrapolated_sites <- seq(1, max_sites, length.out = 300)
  
  # Predict richness for observed + extrapolated sites
  extrapolated_data <- data.frame(Sites = extrapolated_sites) %>%
    mutate(
      Fitted = predict(fit, newdata = list(Sites = extrapolated_sites)),
      SD = ifelse(Sites <= max(accum_data$Sites), 
                  approx(accum_data$Sites, accum_data$SD, xout = Sites)$y, 
                  NA),  # Extrapolate SD using observed data
      Upper = Fitted + SD,
      Lower = Fitted - SD
    )
  
  # Combine observed and extrapolated data for plotting
  plot_data <- extrapolated_data %>%
    mutate(Observed = ifelse(Sites <= max(accum_data$Sites), "Observed", "Extrapolated"),
           estimated_sites = estimated_sites,
           asymptote = asymptote)
  
  return(list(plot_data=plot_data,accum_data=accum_data))
  
}
