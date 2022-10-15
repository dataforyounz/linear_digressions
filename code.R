
rm( list = ls() )

require( tidyverse )
require( broom )
require( ggpubr )

sim_lm <- function( sim, n_obs, b0_true, b1_true, sd_error )
{
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Function for simulating regression models
  ##
  ## Arguments
  ## sim:      simulation index number 
  ## n_obs:    number of observations
  ## b0_true:  true intercept value
  ## b1_true:  true slope value
  ## sd_error: standard deviation for noise component
  ##
  ## Output
  ## tibble of coefficient estimates for each model
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Noise component
  skew_residuals <- ordinal::rgumbel( n_obs, 0, sd_error )
  norm_residuals <- rnorm( n_obs, 0, sd_error ) 
  
  # Simulated response variables for each model
  y_norm <- b0_true + b1_true * sin( t_step / 4 ) + norm_residuals
  y_skew <- b0_true + b1_true * sin( t_step / 4 ) + skew_residuals
  
  # Create a tibble for the simulated data
  data_sim <- tibble(
    time_step = t_step,
    y_norm = y_norm,
    y_skew = y_skew
  )
  
  # List output for model fits
  lm_mod <- list()
  lm_mod[["lm_norm"]] <- lm( y_norm ~ sin( I(time_step / 4) ), data = data_sim ) %>% tidy()
  lm_mod[["lm_skew"]] <- lm( y_skew ~ sin( I(time_step / 4) ), data = data_sim ) %>% tidy()
  
  # Output
  bind_rows( lm_mod, .id = "model" ) %>%
    mutate( sim = sim ) %>%
    relocate( sim )
  
}

## SETUP ----------------------------------------------------------------------

# Time step
t_step <- 0:100
n_obs  <- length( t_step ) 

# Ground truth parameter values
b0_true  <- 3
b1_true  <- .5
sd_error <- .3

# SIMPLE EXAMPLE --------------------------------------------------------------

# Linear model
model <- b0_true + b1_true * sin( t_step / 4 ) 

# Noise component
skew_noise <- ordinal::rgumbel( n_obs, 0, sd_error )
norm_noise <- rnorm( n_obs, 0, sd_error ) 

# Plots
model_comp <- ggplot() + 
              geom_line( aes(x = t_step, y = model), col = "black") + 
              ylim(2, 4) + 
              theme_bw() +
              labs( x = "Time Step", y = "Response", subtitle = "Model Component")

x_seq <- seq( -2, 2, len = 1001 )
noise_comp <- ggplot() + 
              geom_line( aes(x = x_seq, y = dnorm( x_seq, 0, sd_error) ), col = "blue") +
              geom_line( aes(x = x_seq, y = ordinal::dgumbel( x_seq, 0, sd_error) ), col = "red") +
              theme_bw() +
              labs( x = "Error", y = "Density", subtitle = "Noise Component")

dual_plot <- ggarrange( model_comp, noise_comp, nrow = 1 )
ggsave( dual_plot, file = "model_plot.png", units = "cm", height = 9, width = 16)

# Create data
data_sim <- tibble(
  time_step = t_step,
  y_norm = model + norm_noise,
  y_skew = model + skew_noise
)

# Fit linear model 
lm_norm <- lm( y_norm ~ sin( I(time_step / 4) ), data = data_sim )
lm_skew <- lm( y_skew ~ sin( I(time_step / 4) ), data = data_sim )

# Model outputs
tidy( lm_norm )
tidy( lm_skew )

# SIMUALTION -------------------------------------------------------------------
n_sim    <- 1:2000
sim_data <- n_sim %>% 
            map( sim_lm, n_obs = n_obs, b0_true = b0_true, b1_true = b1_true, sd_error = sd_error )

sim_results <- sim_data %>% 
               bind_rows() %>% 
               mutate( term = recode(term, "(Intercept)" = "Intercept",
                                           "sin(I(time_step/4))" = 'Slope') )
sim_plot <- sim_results %>% 
            ggplot( aes(x = estimate)) + 
            geom_histogram( fill = "light blue", col = "white" ) + 
            facet_grid( model ~ term, scale = "free") +
            theme_bw() +
            labs( y = "Frequency", x = "Estimate")

ggsave( sim_plot, file = "sim_plot.png", units = "cm", height = 9, width = 16)


# Simulation summary
sim_summary <- sim_results %>%
               group_by( model, term) %>% 
               summarise( mean = mean(estimate), sd = sd(estimate)) %>%
               ungroup()











