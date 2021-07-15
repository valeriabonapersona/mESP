'
Analysis full model
'

# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
dat_files <- list.files(temp)[str_detect(list.files(temp), "dat")]
dat_complete <- lapply(dat_files, function(x) readRDS(paste0(temp, x))) %>% 
  bind_rows() 
write.csv(dat_complete, paste0(final, "structural_plasticity.csv"))

dat_complete <- dat_complete %>% 
  mutate(out_grouped = ifelse(str_detect(out_grouped, "dendrites"), "structure", out_grouped))


# General frequencies -----------------------------------------------------
# number of papers
length(unique(dat_complete$id))
length(unique(dat_complete$exp_id))


# Analysis ----------------------------------------------------------------
# Step 4: meta-analyze
mod <- rma.mv(
  yi, vi, 
  random = list(~1 | unique_id, ~1 | id),
  method = "REML",
  data = dat_complete, 
#  mods = ~ out_grouped:ba_main:trauma_presence - 1,
  mods = ~ out_grouped:ba_main - 1,
  slab = exp_id
)

# multivariate example from Viech. 
#https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate?s[]=multivariate#multilevel_models
mod <- rma.mv(
  yi, vi, 
  random = ~ unique_id|id,
  method = "REML",
  data = dat_complete, 
  #  mods = ~ out_grouped:ba_main:trauma_presence - 1,
  mods = ~ out_grouped:ba_main - 1,
  slab = exp_id, 
  struct = "UN" # unstructure varcov matrix for the true effects. In this way you allow for heterog to differ between outcomes
)
summary(mod)


# Calculate I2 ------------------------------------------------------------
# from https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate?s[]=multivariate#multilevel_models
W <- diag(1/dat_complete$vi)
X <- model.matrix(mod)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(mod$sigma2) / (sum(mod$sigma2) + (mod$k-mod$p)/sum(diag(P)))

# between and within cluster heterogeneity
100 * mod$sigma2 / (sum(mod$sigma2) + (mod$k-mod$p)/sum(diag(P)))

# RQ ----------------------------------------------------------------------
interactions <- names(data.frame(mod$X))

##R1: outcomes overall
out <- unique(dat_complete$out_grouped)
contr_out <- sapply(out, function(x) str_detect(interactions, x)) %>% t()
anova(mod, L = contr_out) 
summary(glht(mod, linfct = contr_out, df=df.residual(mod)), test=adjusted('holm'))


## RQ0: Overall effect >> redo with all ba togehter?
## RQ1: Does ELA decrease volume of hippocampal areas?
hip_ba <- unique(dat_complete$ba_main)
contr_ba <- sapply(hip_ba, function(x) str_detect(interactions, x)) %>% t()

anova(mod, L = contr_ba) 
summary(glht(mod, linfct = contr_ba, df=df.residual(mod)), test=adjusted('holm'))

## RQ2: Does the second hit influence the effects?
contr_hit <- rbind(
  str_detect(interactions, "trauma_presenceno"),
  str_detect(interactions, "trauma_presenceyes")
)

anova(mod, L = contr_hit) 
summary(glht(mod, linfct = contr_hit, df=df.residual(mod)), test=adjusted('holm'))


