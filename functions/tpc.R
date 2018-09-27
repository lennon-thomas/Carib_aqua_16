##################################################
## Function purpose: Thermal Performance Curve
## Date: 09/24/2018
## Author: Tyler Clavelle
##################################################

## Set User (lennon/tyler)
user <- 'tyler'
if(user == 'lennon') { boxdir <- '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/'}
if(user == 'tyler')  { boxdir <-  '../../Box Sync/Carib_aqua_16/'}

# Plot theme
carib_theme <- function() {
  theme_minimal() +
    theme(text         = element_text(size = 6),
          title        = element_text(size = 10),
          axis.text    = element_text(size = 8),
          legend.text  = element_text(size = 8))
}

# Thermal Performance Curve
tpc <- function(t, tmin = 22, topt = 29, tmax = 32, 
                a1 = 0.0714, a2 = -0.1667, b1 = -1.5714, b2 = 5.333) {
  
  if(t > tmin & t <= tmax) {
    if (t < topt) {
      g = a1 * t + b1
    } else g = a2 * t + b2
  } else g = 0
  return(g)
  
}

# Run function on a range of temperatures
temps <- seq(from = 20, to = 33, by = 0.1)

# Dataframe for plot
df <- data_frame(t = temps)
df$g <- sapply(df$t, FUN = tpc)

# Plot TPC
ggplot(df, aes(x = t, y = g)) +
  geom_line(size = 1) +
  labs(x = 'Temperature (C)',
       y = 'Monthly Growth (kg/m)') +
  carib_theme()

ggsave(filename = paste0(boxdir, 'results/sst/tpc.pdf'),  width = 88, height = 88, units = 'mm')
 