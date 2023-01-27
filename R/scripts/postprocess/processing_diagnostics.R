
start_time <- Sys.time()

# LIBRARIES AND DIRECTORIES -----------------------------------------------

library(ellipse)
library(progress)
library(cowplot)

# PULL GEODATA ------------------------------------------------------------

# Pull in geoid data
geodata <- read.csv("./data/geodata_2019_statelevel.csv")
if(pathogen == "flu"){
  states <- read.csv("./data/USPS_to_state_name.csv")
  geodata_states <- left_join(x = geodata,
                              y = states,
                              by = "USPS") %>%
    mutate(geoid = stringr::str_pad(geoid, width = 5, side = "left", pad = "0"))
}
if(pathogen == "covid19"){
  states <- read.delim("./data/states.txt", header = TRUE, sep = "\t")
  geodata_states <- left_join(x = geodata,
                              y = states,
                              by = c("USPS" = "State.Abbreviation")) %>%
    mutate(geoid = stringr::str_pad(geoid, width = 5, side = "left", pad = "0"))
}

# PULL OUTCOMES FROM S3 ---------------------------------------------------

# List of outcomes to pull
outcomes_list <-
  c("hnpi", "hpar", "llik", "seed", "seir", "snpi", "spar")

# Download all final outcomes from AWS buckets
for (i in 1:length(outcomes_list)) {
  sys_call_s3 <-
    paste0(
      'aws s3 cp --recursive s3://idd-inference-runs/USA-',
      scenario_s3_buckets,
      '/model_output/',
      outcomes_list[i],
      ' ',
      scenario_dir,
      '/',
      outcomes_list[i],
      ' --exclude="*" --include="*/final/*"'
    )
  system(sys_call_s3)
}

# Download intermediate likelihoods from AWS buckets
for (i in 1:length(outcomes_list)) {
  sys_call_s3 <-
    paste0(
      'aws s3 cp --recursive s3://idd-inference-runs/USA-',
      scenario_s3_buckets,
      '/model_output/',
      "llik",
      ' ',
      scenario_dir,
      '/',
      "llik",
      ' --exclude="*" --include="*/intermediate/*"'
    )
  system(sys_call_s3)
}

# FUNCTIONS ---------------------------------------------------------------

import_s3_outcome <- function(scn_dir, outcome, global_opt, final_opt){
  dir_ <- paste0(scn_dir, "/", outcome, "/USA/inference/med")
  subdir_ <- paste0(dir_, "/", list.files(dir_),
                    "/",
                    global_opt,
                    "/",
                    final_opt)
  subdir_list <- list.files(subdir_)
  
  out_ <- NULL
  total <- length(subdir_list)
  pb <- txtProgressBar(min=0, max=total, style = 3)
  
  print(paste0("Importing ", outcome, " files (n = ", total, "):"))

  for (i in 1:length(subdir_list)) {
    if(any(grepl("parquet", subdir_list))){
      dat <- arrow::read_parquet(paste(subdir_, subdir_list[i], sep = "/"))
    }
    if(any(grepl("csv", subdir_list))){
      dat <- read.csv(paste(subdir_, subdir_list[i], sep = "/"))
    }
    if(final_opt == "final"){
      dat$slot <- as.numeric(str_sub(subdir_list[i], start = 1, end = 9))
    }
    if(final_opt == "intermediate"){
      dat$slot <- as.numeric(str_sub(subdir_list[i], start = 1, end = 9))
      dat$block <- as.numeric(str_sub(subdir_list[i], start = 11, end = 19))
    }
    out_ <- rbind(out_, dat)
    
    # Increase the amount the progress bar is filled by setting the value to i.
    setTxtProgressBar(pb, value = i)
  }
  close(pb)
  return(out_)
}

# IMPORT OUTCOMES ---------------------------------------------------------

hnpi <- import_s3_outcome(scenario_dir, "hnpi", "global", "final") %>%
  full_join(geodata_states, by = "geoid")
hpar <- import_s3_outcome(scenario_dir, "hpar", "global", "final") %>%
  full_join(geodata_states, by = "geoid")
llik <- import_s3_outcome(scenario_dir, "llik", "global", "final") %>%
  full_join(geodata_states, by = "geoid")
global_int_llik <- import_s3_outcome(scenario_dir, "llik", "global", "intermediate") %>%
  full_join(geodata_states, by = "geoid")
chimeric_int_llik <- import_s3_outcome(scenario_dir, "llik", "chimeric", "intermediate") %>%
  full_join(geodata_states, by = "geoid")
seed <- import_s3_outcome(scenario_dir, "seed", "global", "final") %>%
  mutate(geoid = stringr::str_pad(place, width = 5, side = "left", pad = "0")) %>%
  full_join(geodata_states, by = "geoid")
snpi <- import_s3_outcome(scenario_dir, "snpi", "global", "final") %>%
  full_join(geodata_states, by = "geoid")
spar <- import_s3_outcome(scenario_dir, "spar", "global", "final")

# DERIVED OBJECTS ---------------------------------------------------------

bind_hnpi_llik <- full_join(x = hnpi, y = llik) %>%
  pivot_wider(names_from = npi_name, values_from = reduction)
names_hnpi <- unique(hnpi$npi_name)
if(all(!is.na(hnpi$npi_name))){
  var_hnpi_llik <- bind_hnpi_llik %>%
    dplyr::select(all_of(names_hnpi)) %>%
    summarize(across(everything(), ~var(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = all_of(names_hnpi)) %>%
    filter(value > 0.0001)
}

bind_hpar_llik <- full_join(x = hpar, y = llik) %>%
  filter(quantity == "probability") %>%
  pivot_wider(names_from = outcome, values_from = value)
names_hpar <- unique(hpar$outcome)
var_hpar_llik <- bind_hpar_llik %>%
  dplyr::select(all_of(names_hpar)) %>%
  summarize(across(everything(), ~var(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = all_of(names_hpar)) %>%
  filter(value > 0.0001)

int_llik <- rbind(global_int_llik %>%
                    mutate(type = "global"),
                  chimeric_int_llik %>%
                    mutate(type = "chimeric"))

bind_snpi_llik <- full_join(x = snpi, y = llik) %>%
  pivot_wider(names_from = npi_name, values_from = reduction)
names_snpi <- unique(snpi$npi_name)
var_snpi_llik <- bind_snpi_llik %>%
  dplyr::select(all_of(names_snpi)) %>%
  summarize(across(everything(), ~var(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = all_of(names_snpi)) %>%
  filter(value > 0.0001)

bind_spar_llik <- full_join(x = spar, y = llik) %>%
  pivot_wider(names_from = parameter, values_from = value)
names_spar <- unique(spar$parameter)
var_spar_llik <- bind_spar_llik %>%
  dplyr::select(all_of(names_spar)) %>%
  summarize(across(everything(), ~var(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = all_of(names_spar)) %>%
  filter(value > 0.0001)

# seed %>% filter(destination_vaccination_stage=="immune") %>% pull(amount) %>% sum() / 330000000

# ANALYSES ----------------------------------------------------------------

USPS <- sort(geodata_states$USPS)

# llik
all_ll_plot <- llik %>%
  ggplot(aes(x = USPS, y = ll)) +
  geom_violin(scale = "width") +
  geom_jitter(shape = ".", height = 0, width = 0.2) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "log likelihood", label = "Convergence of likelihood across slots")

accept_plot <- llik %>%
  ggplot(aes(x = slot, y = accept_avg)) +
  geom_point() +
  theme_bw(base_size = 10) +
  labs(y = "average acceptance rate", label = "Acceptance rate across slots")

hnpi_plot <- list()
hnpi_llik_plot <- list()
hpar_plot <- list()
hpar_llik_plot <- list()
int_llik_plot <- list()
seed_plot <- list()
snpi_plot <- list()
snpi_llik_plot <- list()
spar_plot <- list()
spar_llik_plot <- list()
state_plot1 <- list()
state_plot2 <- list()
state_plot3 <- list()
state_plot4 <- list()
state_plot5 <- list()

pb2 <- txtProgressBar(min=0, max=length(USPS), style = 3)

for(i in 1:length(USPS)){
  
  print(paste0("Preparing plots for ", USPS[i]))
  
  # hnpi
  if(all(is.na(hnpi$npi_name))){
    print("hnpi files are empty")
  } else {
    hnpi_plot[[i]] <- hnpi %>%
      filter(USPS == USPS[i]) %>%
      ggplot(aes(x = npi_name, y = reduction)) + 
      geom_violin(scale = "width") +
      geom_jitter(shape = ".", height = 0, width = 0.2) +
      theme_bw(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste0(USPS[i], " hnpi values"))
    
    hnpi_llik_plot[[i]] <- bind_hnpi_llik %>%
      filter(USPS == USPS[i]) %>%
      dplyr::select(ll, all_of(var_hnpi_llik$name)) %>%
      pivot_longer(cols = all_of(var_hnpi_llik$name)) %>%
      drop_na(value) %>%
      ggplot(aes(x = value, y = ll)) +
      geom_point(size = 0.5, alpha = 0.8) +
      facet_wrap(~name, scales = "free_x") +
      geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
      theme_bw(base_size = 10) +
      labs(y = "log likelihood", title = paste0(USPS[i], " hnpi correlation with likelihood"))
    }
  
  # hpar
  if(all(is_empty(var_hpar_llik$name))){
    print("no varying hpar outcomes to plot")
  } else {
    hpar_plot[[i]] <- bind_hpar_llik %>%
      filter(USPS == USPS[i]) %>%
      dplyr::select(ll, all_of(var_hpar_llik$name)) %>%
      pivot_longer(cols = all_of(var_hpar_llik$name)) %>%
      drop_na(value) %>%
      ggplot(aes(x = name, y = value)) +
      geom_violin(scale = "width") +
      geom_jitter(aes(group = name, color = ll), size = 0.5, height = 0, width = 0.2, alpha = 0.8) +
      theme_bw(base_size = 10) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 6)) +
      scale_color_viridis_c(option = "B", name = "log\nlikelihood") +
      labs(x = "parameter", title = paste0(USPS[i], " hpar probability values"))
    
    hpar_llik_plot[[i]] <- bind_hpar_llik %>%
      filter(USPS == USPS[i]) %>%
      dplyr::select(ll, all_of(var_hpar_llik$name)) %>%
      pivot_longer(cols = all_of(var_hpar_llik$name)) %>%
      drop_na(value) %>%
      ggplot(aes(x = value, y = ll)) +
      geom_point(size = 0.5, alpha = 0.8) +
      facet_wrap(~name, scales = "free_x") +
      geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
      theme_bw(base_size = 10) +
      theme(strip.text = element_text(size = 4)) +
      labs(y = "log likelihood", title = paste0(USPS[i], " hpar correlation with likelihood"))
  }
  
  # llik
  int_llik_plot[[i]] <- int_llik %>%
    filter(USPS == USPS[i],
           between(slot, 1, 8)) %>%
    ggplot() +
    geom_step(aes(x = block, y = ll, linetype = type, color = factor(slot))) +
    scale_color_brewer(palette = "Dark2", name = "slot") +
    theme_bw(base_size = 10) +
    labs(y = "log likelihood", title = paste0(USPS[i], " global and chimeric intermediate likelihoods"))
  
  # seed
  seed_plot[[i]] <- seed %>% 
    filter(destination_infection_stage == "E",
           USPS == USPS[i]) %>%
    ggplot(aes(x = as.Date(date), y = amount, color = destination_variant_type)) +
    geom_count(alpha = 0.8) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    scale_color_brewer(palette = "Dark2", name = "variant") +
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = "seeding date", title = paste0(USPS[i], " seeding across all slots"))
  
  # snpi
  snpi_plot[[i]] <- bind_snpi_llik %>%
    filter(USPS == USPS[i]) %>%
    dplyr::select(ll, all_of(var_snpi_llik$name)) %>%
    pivot_longer(cols = all_of(var_snpi_llik$name)) %>%
    drop_na(value) %>%
    ggplot(aes(x = name, y = value)) +
    geom_violin(scale = "width") +
    geom_jitter(aes(group = name, color = ll), size = 0.5, height = 0, width = 0.2, alpha = 0.8) +
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_color_viridis_c(option = "B", name = "log\nlikelihood") +
    labs(x = "reduction", title = paste0(USPS[i], " snpi reduction values"))
  
  snpi_llik_plot[[i]] <- bind_snpi_llik %>%
    filter(USPS == USPS[i]) %>%
    dplyr::select(ll, all_of(var_snpi_llik$name)) %>%
    pivot_longer(cols = all_of(var_snpi_llik$name)) %>%
    drop_na(value) %>%
    ggplot(aes(x = value, y = ll)) +
    geom_point(size = 0.5, alpha = 0.8) +
    facet_wrap(~name, scales = "free_x") +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
    theme_bw(base_size = 10) +
    labs(y = "log likelihood", title = paste0(USPS[i], " snpi correlation with likelihood"))
  
  # spar
  spar_plot[[i]] <- bind_spar_llik %>%
    filter(USPS == USPS[i]) %>%
    dplyr::select(ll, all_of(var_spar_llik$name)) %>%
    pivot_longer(cols = all_of(var_spar_llik$name)) %>%
    drop_na(value) %>%
    ggplot(aes(x = name, y = value)) +
    geom_violin(scale = "width") +
    geom_jitter(aes(group = name, color = ll), size = 0.5, height = 0, width = 0.2, alpha = 0.8) +
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_color_viridis_c(option = "B", name = "log\nlikelihood") +
    labs(x = "parameter", title = paste0(USPS[i], " spar parameter values"))
  
  spar_llik_plot[[i]] <- bind_spar_llik %>%
    filter(USPS == USPS[i]) %>%
    dplyr::select(ll, all_of(var_spar_llik$name)) %>%
    pivot_longer(cols = all_of(var_spar_llik$name)) %>%
    drop_na(value) %>%
    ggplot(aes(x = value, y = ll)) +
    geom_point(size = 0.5, alpha = 0.8) +
    facet_wrap(~name, scales = "free_x") +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
    theme_bw(base_size = 10) +
    labs(y = "log likelihood", title = paste0(USPS[i], " spar correlation with likelihood"))
  
  # Combined output plots
  state_plot1[[i]] <- plot_grid(int_llik_plot[[i]],
                                seed_plot[[i]],
                                nrow = 2, ncol = 1)
  if(all(is.na(hnpi$npi_name))){
    state_plot2[[i]] <- NA
  } else {
    state_plot2[[i]] <- plot_grid(hnpi_plot[[i]],
                                  hnpi_llik_plot[[i]],
                                  nrow = 2, ncol = 1)
  }
  if(all(is_empty(var_hpar_llik$name))){
    state_plot3[[i]] <- NA
  } else {
    state_plot3[[i]] <- plot_grid(hpar_plot[[i]],
                                  hpar_llik_plot[[i]],
                                  nrow = 2, ncol = 1)
  }
  state_plot4[[i]] <- plot_grid(snpi_plot[[i]],
                                snpi_llik_plot[[i]],
                                nrow = 2, ncol = 1)
  state_plot5[[i]] <- plot_grid(spar_plot[[i]],
                                spar_llik_plot[[i]],
                                nrow = 2, ncol = 1)
  
  # Increase the amount the progress bar is filled by setting the value to i.
  setTxtProgressBar(pb2, value = i)
}

# OUTPUT FILES ------------------------------------------------------------
pdf(file = paste0(round_directory, "/", fch_date, "_", pathogen, "_", smh_or_fch, "_R", round_num, "_", scenarios, "_", ymd(today()), ".pdf"),
    height = 12,
    width = 9)
all_ll_plot
accept_plot
for(i in 1:length(USPS)){
  print(paste0("Outputting plots for ", USPS[i]))
  plot(state_plot1[[i]])
  if(all(!is.na(state_plot2[[i]]))){
    plot(state_plot2[[i]])
  }
  if(all(!is.na(state_plot3[[i]]))){
    plot(state_plot3[[i]])
  }
  plot(state_plot4[[i]])
  plot(state_plot5[[i]])
}
dev.off()

end_time <- Sys.time()
print(end_time - start_time)
