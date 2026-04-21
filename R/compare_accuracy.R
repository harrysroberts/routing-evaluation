library(tidyverse)
library(patchwork)
library(quantreg)

trips_attributes <- read_csv("input/processed/trips_attributes.csv")

#Create a dataset that only compares the chosen alternative's attributes
trips_comparison <- trips_attributes %>%
  
  #get rid of waiting time for bus and rail - not appropriate given apps
  mutate(
    bus_time_R5 = bus_time_R5 - bus_timewaiting_R5,
    bus_time_GR = bus_time_GR - bus_timewaiting_GR,
    rail_time_R5 = rail_time_R5 - rail_timewaiting_R5,
    rail_time_GR = rail_time_GR - rail_timewaiting_GR
    ) %>%
  
  #pivot to a long format table
  pivot_longer(
    cols = c(walk_distance_R5:rail_time_GR),
    names_to = c("estimated_mode", "attribute", "method"),
    names_sep = "_",
    values_to = "estimated_value"
  ) %>%
  
  #keep only the estimates of the chosen mode
  filter(mode == estimated_mode) %>%
  select(!estimated_mode) %>%
  
  #get rid of in-vehicle, transfer and waiting attributes
  filter(attribute %in% c("distance","time")) %>%
  
  #match attribute to decisions value
  mutate(
    recorded_value = if_else(
      attribute=="distance",
      distance_decisions,
      time_decisions
      )
    ) %>%
  select(!c(distance_decisions,time_decisions)) %>% 
  
  #convert to factors
  mutate(
    mode = factor(mode, levels = c("walk","bike","car","bus","rail")),
    attribute = factor(attribute, levels = c("distance","time")),
    method = factor(method, levels = c("GR","R5","DO"))
  )

#Create a quantile regression model for parking time corrections
parking_model <- trips_comparison %>%
  filter(attribute == "time") %>%
  mutate(
    y = recorded_value - estimated_value,
    I_bike = as.integer(mode == "bike"),
    I_car  = as.integer(mode == "car")
  ) %>%
  rq(y ~ 0 + I_bike + I_car,tau=0.5,data=.)

bike_parking <- if_else(
  summary(parking_model)$coefficients["I_bike", "Pr(>|t|)"]<0.05,
  summary(parking_model)$coefficients["I_bike","Value"],
  0
  )

car_parking <- if_else(
  summary(parking_model)$coefficients["I_car", "Pr(>|t|)"]<0.05,
  summary(parking_model)$coefficients["I_car","Value"],
  0
  )

write_csv(tibble(car_parking,bike_parking),"input/processed/parking_times.csv")

trips_comparison <- trips_comparison %>%
  
  mutate(
    
    #Introduce a parking adjustment
    parking_adjustment = 
      bike_parking * (mode == "bike") * (attribute == "time") +
      car_parking * (mode == "car") * (attribute == "time"),
    
    #Add key accuracy statistics
    ratio = recorded_value/(estimated_value+parking_adjustment),
    lr = log(ratio),
    alr = abs(lr)
  )

write_csv(trips_comparison,"output/accuracy/trips_comparison.csv")

### STATS ###

trips_comparison_stats <- trips_comparison %>%
  #removes the 5 cases where the ratio was infinite and the 295 cases where it was NA (because GR or R5 couldn't find bus/rail routes)
  filter(ratio != 0 & ratio != Inf & !is.nan(ratio)) %>% 
  group_by(mode,attribute,method) %>%
  summarise(
    geomean = exp(mean(lr)),
    geosd = exp(sd(lr)),
    skew_fisher = n()/((n()-1)*(n()-2))*sum((log(ratio/geomean)/log(geosd))^3),
    median = median(ratio),
    median_se = exp(sd(log(
        replicate(
          1000,
          median(sample(ratio,replace=TRUE),na.rm=TRUE)
          )
        ))),
    median_ci_lower = median*median_se^(-1.96),
    median_ci_upper = median*median_se^(1.96),
    lq = quantile(ratio,0.25,na.rm = TRUE),
    uq = quantile(ratio,0.75,na.rm = TRUE),
    qratio = uq/lq,
    kde_mode = exp(
      {
      d <- stats::density(lr)
      d$x[which.max(d$y)]
      }
    ),
    skew_bowley = log(uq*lq/median^2)/log(uq/lq),
    within_point05 = sum(between(ratio,1/1.05,1.05))/n(),
    within_point1 = sum(between(ratio,1/1.1,1.1))/n(),
    alr_median = median(alr),
    latex_string = str_c(
      round(geomean,3),
      " & ",
      round(median,3),
      " & (",
      round(median_ci_lower,3),
      ",",
      round(median_ci_upper,3),
      ") & ",
      round(geosd,3),
      " & ",
      round(qratio,3)
      )
  )

write_csv(trips_comparison_stats,"output/accuracy/trips_comparison_stats.csv")

### COMPARISON PLOTS ###

#distance comparison plot
distance_plot <- trips_comparison %>%
  filter(recorded_value>0) %>%
  filter(attribute == "distance") %>%
  ggplot(aes(x = estimated_value, y = recorded_value, colour = mode)) +
  geom_abline(intercept = 0, slope = 1, colour = "black", linewidth = 0.5) +
  geom_abline(intercept = log10(2), slope = 1, colour = "black", linewidth = 0.5, linetype = "dashed") +
  geom_abline(intercept = log10(1/2), slope = 1, colour = "black", linewidth = 0.5, linetype = "dashed") +
  geom_point(size=0.2,stroke=0.3,alpha=0.5,shape=16) +
  scale_x_log10(
    breaks = c(1,10, 100, 1000, 10000),
    labels = scales::label_number(),
    limits = c(50, 50000)
    ) +
  scale_y_log10(
    breaks = c(1,10, 100, 1000, 10000),
    labels = scales::label_number(),
    limits = c(50, 50000),
    ) +
  scale_color_manual(
    labels = c(
      "walk" = "Walk",
      "bike" = "Bicycle",
      "car"  = "Car",
      "bus"  = "Bus",
      "rail" = "Rail"
    ),
    values = c(
      walk = "limegreen",
      bike  = "blue",
      car  = "magenta",
      bus = "red",
      rail = "darkslategrey"
    )
  ) +
  labs(
    x = "Estimated distance (m)", 
    y = "Recorded distance (m)",
    colour = "Mode"
    ) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 1, alpha = 1, shape = 16, stroke = 0)
    )
  ) +
  facet_grid(~ method, scales = "fixed") +
  theme_bw(base_size = 8) +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(size = 8, colour = "black"),
    axis.text.y  = element_text(size = 8, colour = "black"),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    panel.grid = element_blank()
  )

#time comparison plot
time_plot <- trips_comparison %>%
  filter(recorded_value>0) %>%
  filter(attribute == "time") %>%
  ggplot(aes(x = estimated_value + parking_adjustment, y = recorded_value, colour = mode)) +
  geom_abline(intercept = 0, slope = 1, colour = "black", linewidth = 0.5) +
  geom_abline(intercept = log10(2), slope = 1, colour = "black", linewidth = 0.5, linetype = "dashed") +
  geom_abline(intercept = log10(1/2), slope = 1, colour = "black", linewidth = 0.5, linetype = "dashed") +
  geom_point(size=0.2,stroke=0.3,alpha=0.5,shape=16) +
  #geom_abline(aes(intercept = log10(best_fit), slope = 1), colour = "steelblue", linewidth = 0.5) +
  scale_x_log10(
    breaks = c(1,10, 100),
    labels = scales::label_number(),
    limits = c(0.5, 500)
    ) +
  scale_y_log10(
    breaks = c(1,10, 100),
    labels = scales::label_number(),
    limits = c(0.5, 500)
    ) +
  scale_color_manual(
    labels = c(
      "walk" = "Walk",
      "bike" = "Bicycle",
      "car"  = "Car",
      "bus"  = "Bus",
      "rail" = "Rail"
    ),
    values = c(
      walk = "limegreen",
      bike  = "blue",
      car  = "magenta",
      bus = "red",
      rail = "darkslategrey"
      )
    ) +
  labs(
    x = "Estimated time (min)", 
    y = "Recorded time (min)",
    colour = "Mode"
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 1, alpha = 1, shape = 16, stroke = 0)
    )
  ) +
  facet_grid(~ method, scales = "fixed") +
  theme_bw(base_size = 8) +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(size = 8, colour = "black"),
    axis.text.y  = element_text(size = 8, colour = "black"),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    panel.grid = element_blank()
  )

Fig1 <- (distance_plot / time_plot) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect")

ggsave("output/accuracy/plots/Fig1.tif", plot = Fig1, width = 174, height = 116, 
       units = "mm", dpi = 600, device = "tiff")

ggsave("output/accuracy/plots/Fig1.png", plot = Fig1, width = 174, height = 116, 
       units = "mm", dpi = 600, device = "png")

### LOG-RATIO DENSITY PLOTS ###

#distance
distance_density <- trips_comparison %>%
  mutate(mode = factor(
    mode,
    levels = c("walk", "bike", "car", "bus", "rail"),
    labels = c("Walk", "Bicycle", "Car", "Bus", "Rail")
  )) %>%
  filter(recorded_value>0) %>%
  filter(attribute == "distance") %>%
  ggplot(aes(x = lr)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  geom_vline(xintercept = 0, colour = "black",linetype = "dashed", linewidth = 0.3) +
  facet_grid(mode ~ method, scales = "free",space = "free_y") +
  scale_x_continuous(
    breaks = log(c(0.25,0.5, 1, 2, 4)),
    labels = c("-ln 4","-ln 2", 0,"ln 2","ln 4"),
    limits = log(c(0.25, 4))
  ) +
  scale_y_continuous(
    breaks = c(0, 2, 4),
    labels = c("0","2","4"),
    limits = c(0,5.2)
  ) +
  labs(x = "Distance Log-Ratio", y = "Density") +
  geom_text(
    data = tibble(
      method = factor(c("DO","DO"), levels = c("GR","R5","DO")),
      mode   = factor(c("Bus","Rail"), levels = c("Walk","Bicycle","Car","Bus","Rail")),
      x      = c(0,0),
      y      = c(2.6,2.6),
      label  = c("NA","NA")
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 8, fontface = "bold", colour = "grey40"
  ) +
  theme_bw(base_size = 8) +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(size = 8, colour = "black"),
    axis.text.y  = element_text(size = 8, colour = "black"),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0.4,"cm"),
    )

#time
time_density <- trips_comparison %>%
  mutate(mode = factor(
    mode,
    levels = c("walk", "bike", "car", "bus", "rail"),
    labels = c("Walk", "Bicycle", "Car", "Bus", "Rail")
    )) %>%
  filter(recorded_value>0) %>%
  filter(attribute == "time") %>%
  ggplot(aes(x = lr)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  facet_grid(mode ~ method, scales = "free") +
  scale_x_continuous(
    breaks = log(c(0.125,0.25,0.5, 1, 2, 4, 8)),
    labels = c("-ln 8","-ln 4","-ln 2", 0,"ln 2","ln 4","ln 8"),
    limits = log(c(0.125, 8))
  ) +
  scale_y_continuous(
    breaks = c(0, 1, 2),
    labels = c("0","1","2"),
    limits = c(0,2)
  ) +
  labs(x = "Time Log-Ratio", y = "Density") +
  geom_text(
    data = tibble(
      method = factor(c("DO","DO"), levels = c("GR","R5","DO")),
      mode   = factor(c("Bus","Rail"), levels = c("Walk","Bicycle","Car","Bus","Rail")),
      x      = c(0,0),
      y      = c(1,1),
      label  = c("NA","NA")
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 8, fontface = "bold", colour = "grey40"
  )  +
  theme_bw(base_size = 8) +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(size = 8, colour = "black"),
    axis.text.y  = element_text(size = 8, colour = "black"),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 8),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0.4,"cm"),
  )


Fig2 <- (distance_density / time_density) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect")

ggsave("output/accuracy/plots/Fig2.tif", plot = Fig2, width = 174, height = 232, 
       units = "mm", dpi = 600, device = "tiff")

ggsave("output/accuracy/plots/Fig2.png", plot = Fig2, width = 174, height = 232, 
       units = "mm", dpi = 600, device = "png")

### FRIEDMAN TESTS ###

friedman_tests <- trips_comparison %>%
  group_by(trip_id,mode,attribute) %>%
  mutate(x = sum(alr)) %>%
  filter(x!= Inf & !is.nan(x)) %>% 
  filter(mode %in% c("walk","bike","car")) %>%
  select(trip_id,mode,attribute,method,alr) %>%
  mutate(method_rank = rank(-alr,ties.method = "average")) %>%
  group_by(mode,attribute,method) %>%
  summarise(R = sum(method_rank), N = n()) %>%
  ungroup(method) %>%
  summarise(chi_square = sum(R^2)/mean(N) - 12*mean(N))

### WILCOXON TESTS ###

wilcoxon_tests <- trips_comparison %>%
  group_by(trip_id,mode, attribute) %>%
  mutate(x = sum(alr)) %>%
  filter(x!= Inf & !is.nan(x)) %>% 
  ungroup() %>%
  select(trip_id,mode,attribute,method,alr) %>%
  inner_join(., ., by = c("trip_id","mode","attribute"), relationship = "many-to-many") %>%
  filter(method.x!=method.y) %>%
  group_by(mode,attribute,method.x,method.y) %>%
  mutate(
    diff = abs(alr.x-alr.y),
    sgn = -sign(alr.x-alr.y),
    diff_rank = rank(diff,ties.method = "average"),
    signed_diff_rank = sgn*diff_rank
    ) %>%
  summarise(
    median_alr = median(alr.x),
    t = sum(signed_diff_rank),
    z = t/sqrt(n()*(n()+1)*(2*n()+1)/6),
    rc = t/(0.5*n()*(n()+1))
  )
