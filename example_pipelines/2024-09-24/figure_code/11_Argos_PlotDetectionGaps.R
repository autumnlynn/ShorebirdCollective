# TO USE IN CONJUNCTION WITH 11_Argos_Truncate.R
library(viridisLite)
library(ggnewscale)


# 1) PROJECT PLOTS LAT BY TIME WITH VERTICAL LINES ####
## 1a) MAKE FIGS ####
# GET DATASET NAMES FOR TITLE NAMES:
titles <- names(dat_argos_lst_gaps) 

# MAKE FIG:
figs_argos_lat_long_gaps <- map2(dat_argos_lst_gaps, titles, ~
       ggplot(data = .x %>% filter(sc.location.origin == "tag"),
              aes(x = timestamp, y = location.lat, color = argos.lc), alpha = 0.5) + 
       geom_point() +
       geom_point(data = .x %>% filter(sc.location.origin  %in% c("deploy_on_lat_long",
                                                              "deploy_off_lat_long")), 
                 aes(x = timestamp, y = location.lat, shape = sc.location.origin), 
                 color = "black", alpha = 0.5) +
       scale_shape_manual(values = c(3, 4)) +
       theme_bw() + 
       viridis::scale_fill_viridis(discrete = TRUE)+ 
       ggnewscale::new_scale_color() +
       # ADD VERTICAL LINES AT AREAS WITH DATA GAPS
       geom_vline(data = .x %>% filter(gap.marker == "short_gap"),
                  aes(xintercept = timestamp, colour = "gap 8 - 14 days"), linetype = "dashed",  linewidth = 0.7) + # gap 8 through 14 days long
       geom_vline(data = .x %>% filter(gap.marker == "medium_gap"),
                  aes(xintercept = timestamp, colour = "gap 15 - 30 days"), linetype = "dashed", linewidth = 0.7) + # gap 15 through 30 days
       geom_vline(data = .x %>% filter(gap.marker == "long_gap"),
                  aes(xintercept = timestamp, colour = "gap > 30 days"), linetype = "dashed", linewidth = 0.7) + # gap greater than 30 days
       scale_color_manual(name = "gap label", values = c("gap 8 - 14 days" = "green", 
                                                         "gap 15 - 30 days" = "blue",
                                                         "gap > 30 days" = "red" )) + 
       facet_wrap(~sc.deployment.id, scales = "free_x") +
       ggtitle(.y))

## 1b) MAKE FIGURE FILE PATHS ####
fig_names <- names(figs_argos_lat_long_gaps) %>% 
  map(~paste0("./Figures/ArgosTruncationFigs/", .x, "_lat_plot.png"))

## 1c) SAVE THE PLOTS ####
walk2(figs_argos_lat_long_gaps, fig_names,
      ~ggsave(plot = .x,
              filename = .y,
              device = "png",
              height = 8,
              width = 11,
              units = "in",
              dpi = 350))


# 2) PLOT SPECIFIC BIRDS OF INTEREST ######################
## 2a) PLOT 1 INDIVIDUAL TAG ####
ggplot() +
  geom_point(data = dat_argos_lst_gaps[[1]] %>% #FIRST DATASET IN LIST
         filter(deployment.id == "movebankdepid25" & sc.location.origin == "tag"), 
       aes(x = timestamp, y = location.lat, color = argos.lc)) + 
  geom_point(data = dat_argos_lst_gaps[[12]] %>% 
               filter(deployment.id == "movebankdepid25" & 
                        sc.location.origin  %in% c("deploy_on_lat_long", "deploy_off_lat_long")), 
             aes(x = timestamp, y = location.lat, shape = sc.location.origin), color = "black", alpha = 0.5) +
  scale_shape_manual(values = c(3, 4)) +
  theme_bw() + 
  viridis::scale_fill_viridis(discrete = TRUE)+ 
  ggnewscale::new_scale_color() +
  geom_vline(data = dat_argos_lst_gaps[[12]] %>% 
               filter(gap.marker == "short_gap" & deployment.id == "movebankdepid25"),
             aes(xintercept = timestamp, colour = "gap 8 - 14 days"), linetype = "dashed",  size = 0.7) + # gap 8 through 14 days long
  geom_vline(data = dat_argos_lst_gaps[[12]] %>% 
               filter(gap.marker == "medium_gap" & deployment.id == "movebankdepid25"),
             aes(xintercept = timestamp, colour = "gap 15 - 30 days"), linetype = "dashed", size = 0.7) + # gap 15 through 30 days
  geom_vline(data = dat_argos_lst_gaps[[12]] %>% 
               filter(gap.marker == "long_gap" & deployment.id == "movebankdepid25"),
             aes(xintercept = timestamp, colour = "gap > 30 days"), linetype = "dashed", size = 0.7) + # gap greater than 30 days
  scale_color_manual(name = "gap label", values = c("gap 8 - 14 days" = "green", 
                                                    "gap 15 - 30 days" = "blue",
                                                    "gap > 30 days" = "red" ))

