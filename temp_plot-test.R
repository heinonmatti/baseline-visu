df <- data.frame(female = factor(sample(0:1, size = 500, replace = TRUE)),
                 intervention = factor(sample(0:1, size = 500, replace = TRUE))) %>% 
  dplyr::mutate(value = ifelse(female == "1", runif(n = 500, min = 0, max = 100), rnorm(n = 500, mean = 50, sd = 20)))

df %>% 
  ggplot2::ggplot(aes(y = intervention)) +
  ggridges::geom_density_ridges2(aes(x = value, 
                                     colour = "black", 
                                     fill = female),
                                 scale = .7,
                                 alpha = 0.6, 
                                 size = 0.25,
                                 jittered_points = TRUE, 
                                 point_shape = 21,
                                 point_size = 0.85,
                                 point_fill = "black")


df %>% 
  ggplot2::ggplot(aes(y = intervention)) +
  ggridges::geom_density_ridges2(aes(x = value, 
                                     point_color = female,
                                     point_fill = female,
                                     point_shape = female,
                                     fill = female),
                                 scale = .7,
                                 alpha = 0.6, 
                                 size = 0.25,
                                 jittered_points = TRUE, 
                                 point_size = 0.85,
                                 position = position_raincloud(width = 0.05, height = 0.15)) +
  ggplot2::scale_fill_manual(values = c("#A0FFA0", "#A0A0FF")) +
  ggridges::scale_discrete_manual(aesthetics = "point_color", values = c("#00BF00", "#0000BF")) +
  ggridges::scale_discrete_manual(aesthetics = "point_fill", values = c("#80FF80", "#8080FF")) +
  ggridges::scale_discrete_manual(aesthetics = "point_shape", values = c(22, 24))
