#----------------------------------------------------------#
#
#
#                    GRAD_Exclosures PNG
#
#         artropods size change due to treatment
#
#                     Katerina Sam
#                         2021
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 3. Import data and explore them -----
#----------------------------------------------------------#

dataset_grad_sizes_full <-  
  readxl::read_xlsx("data/input/Arthropod_Individuals_20210313.xlsx")
is.numeric(dataset_grad_sizes_full$Size) 

# there was a problem of soem of them not being numeric - so I did
dataset_grad_sizes_full$Size <-as.numeric(dataset_grad_sizes_full$Size)
summary(dataset_grad_sizes_full)

# remove unnecessary collumns
dataset_grad_sizes <-
  dataset_grad_sizes_full %>% 
  mutate_if(is.character,as.factor) %>% 
  dplyr::select(Experiment, Patrol, Elevation, Treatment, Code, Size, Guild) %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor)

# subset the data according to the experiments
dataset_grad_sizesVAC<-subset(dataset_grad_sizes, Experiment =="VAC", na.action = "omit")
summary(dataset_grad_sizesVAC)
is.numeric(dataset_grad_sizesVAC$Size) 

# summarize the data as the full dataset drwas crazy things 
dataset_mean_sizesVAC <-
  dataset_grad_sizesVAC %>%
  group_by (Code, Patrol, Elevation, Treatment) %>%
  summarise(
    .groups = "keep",
    Mean_size = mean(Size), na.action = "omit")

#  set colours
pallete_1 <-  brewer.pal(4,"Pastel1")
names(pallete_1) <-  
  dataset_grad_sizesVAC$Treatment %>% 
  unique()

#  plot the first graph
(ext_plot_01 <- 
  dataset_mean_sizesVAC %>% 
  ggplot(
    aes(
      x = Elevation,
      y = Mean_size,
      fill = Treatment,
      col = Treatment)) +
    
    geom_flat_violin(
      col = "gray30",
      alpha = 1/2,
      trim = TRUE,
      position = position_nudge(
        x = 0.2,
        y = 0)) +
    
   geom_boxplot(
    width=0.2,
    outlier.shape = NA,
    col = "gray30",
    alpha = 1) +
  
  labs(
    x = "Elevation", 
    y = "Mean size of arthropod") +
  scale_fill_manual(values = pallete_1) +
  scale_color_manual(values = pallete_1) +
  theme(
    text = element_text(size = text_size),
    legend.position = "top"))

ggsave(
  "fig/arthropod_size/ext_plot_01.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")

#  plot the second graph showing PATROL ONLY
(ext_plot_02 <- 
    dataset_mean_sizesVAC %>% 
    ggplot(
      aes(
        x = Patrol,
        y = Mean_size)) +
    
    geom_flat_violin(
      col = "gray30",
      alpha = 1/2,
      trim = TRUE,
      position = position_nudge(
        x = 0.2,
        y = 0)) +
    
    geom_boxplot(
      width=0.2,
      outlier.shape = NA,
      col = "gray30",
      alpha = 1) +
    
    labs(
      x = "Elevation", 
      y = "Mean size of arthropod") +
    scale_fill_manual(values = pallete_1) +
    scale_color_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "top"))

ggsave(
  "fig/arthropod_size/ext_plot_02_Patrol.pdf",
  ext_plot_01,
  width = PDF_width,
  height = PDF_height,
  units = "in")
# I actually do not see any difference here, but the model later on shows it as significant, there is no ecologicla explanation for it


#----------------------------------------------------------#
# 4. Model build -----
#----------------------------------------------------------#


# cretae full model with all interaction
glm_mean_sizeVAC <-
  glm(Mean_size ~ Elevation * Treatment *Patrol,
          data = dataset_mean_sizesVAC,
          family = Gamma(),
          na.action = "na.fail")

summary(glm_mean_sizeVAC)
check_model(glm_mean_sizeVAC) 
check_distribution(glm_mean_sizeVAC) # tweedie is suggested but problems with convergence
model_performance(glm_mean_sizeVAC)
qplot(residuals(glm_mean_sizeVAC))
check_heteroscedasticity(glm_mean_sizeVAC)

# compute all posible combinations
glm_mean_size_dd <- 
  MuMIn::dredge(
    glm_mean_sizeVAC,
    trace = T)

# save result table
glm_mean_size_dd %>% 
  as_tibble() %>% 
  write_csv("data/output/mean_size_VAC_model_result.csv")

# observe the best model
glm_mean_size_dd %>% 
  as_tibble() %>% 
  filter(delta < 2 ) %>% 
  View()


# fit the best model
glm_bodysizes_best <- 
  glmmTMB(Mean_size ~ Elevation + Treatment + Patrol + Elevation:Treatment + Elevation:Patrol,
          data = dataset_mean_sizesVAC,
          family = Gamma(),
          na.action = "na.fail")


#----------------------------------------------------------#
# 5. Model plot -----
#----------------------------------------------------------#

# calculate emmeans for full model
glm_mean_size_emmeans_VAC <-
  emmeans(
    glm_bodysizes_best,
    pairwise ~ Treatment*Elevation*Patrol,
    type = "response") 

(model_plot_01 <- 
    glm_mean_size_emmeans_VAC$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Elevation,
        y = response,
        col = Treatment)) + 
    
    facet_grid(Patrol ~ .) +
    
    ylim(0,20) +
    
    geom_point(
      data = dataset_mean_sizesVAC,
      aes(y = Mean_size),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      position = position_dodge(width = 0.5),
      size = 3) +
    
    labs(
      x = "Treatment",
      y = "Mean size of arthropod (mm)" ) +
    scale_color_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))


# save the pairwise test 
glm_mean_size_emmeans_VAC$contrasts %>% 
  as_tibble() %>% 
  arrange(p.value) %>% 
  write_csv("data/output/mean_size_pairwise_treat_contrast.csv")

glm_mean_size_emmeans_VAC$emmeans %>% 
  as_tibble() %>% 
  write_csv("data/output/mean_size_pairwise_treat_emmeans.csv")


# calculate emmeans for treatments*elevation ONLY - as the effect of Patrol seems to be weird (woudl have to clean the data fro elevations 3200 and 3700 for the final graph)
glm_mean_size_emmeans_treat <-
  emmeans(
    glm_bodysizes_best,
    pairwise ~ Treatment*Elevation,
    type = "response") 

(model_plot_02 <- 
    glm_mean_size_emmeans_VAC$emmeans %>% 
    as_tibble() %>% 
    ggplot(
      aes(
        x = Elevation,
        y = response,
        col = Treatment)) + 
    
    ylim(0,17) +
    
    geom_point(
      data = dataset_mean_sizesVAC,
      aes(y = Mean_size),
      alpha = 1,
      position = position_jitterdodge(
        dodge.width = 0.5,
        jitter.width = 0.15)) +
    
    geom_errorbar(
      aes(
        ymin =  lower.CL,
        ymax = upper.CL),
      width=0.2,
      position = position_dodge(width = 0.5, preserve = "single"),
      size = 1)+
    
    geom_point(
      shape = 0,
      position = position_dodge(width = 0.5),
      size = 3) +
    
    labs(
      x = "Treatment",
      y = "Mean size of arthropod (mm)" ) +
    scale_color_manual(values = pallete_1) +
    theme(
      text = element_text(size = text_size),
      legend.position = "right"))

