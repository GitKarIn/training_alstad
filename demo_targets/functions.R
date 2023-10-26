# create_penguin_data ----
create_penguin_data <- function(out_path) {
  
  penguin_data <- palmerpenguins::penguins_raw %>% clean_names()
  
  write_csv(penguin_data, out_path)
  
  return(out_path)
  
} # EO penguin_data

# clean_data ----
clean_data <- function(file_path) {
  
  clean_data <- read_csv(file_path) %>% 
    # keep only common species name
    mutate(
      species = str_extract(string = species,
                            pattern = "Chinstrap|Adelie|Gentoo"),
      year = year(date_egg)
    ) %>% 
    # select cols of interest
    select(species,
           island,
           flipper_length_mm,
           body_mass_g, 
           sex) %>% 
    drop_na()
  
  return(clean_data)
}

# exploratory_plot ----
exploratory_plot <- function(clean_data) {
  
  ggplot(data = clean_data, aes(x = flipper_length_mm,
                                y = body_mass_g)) +
    geom_point(aes(color = species)) +
    scale_color_manual(values = c(
      "Adelie" = "purple2",
      "Chinstrap" = "orange",
      "Gentoo" = "cyan4"
    )) +
    labs(
      title = NULL,
      x = "Flipper Length (mm)",
      y = "Body Mass (g)",
      color = "Species"
    ) +
    theme_minimal()
  
  ggsave("figs/exploratory_plot.png", width = 5, height = 5)
}