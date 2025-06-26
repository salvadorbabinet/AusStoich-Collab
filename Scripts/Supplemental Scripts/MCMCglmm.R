library(ape)
library(MCMCglmm)
library(ggtree)
library(tictoc)
library(dplyr)

aus_data <- aus_data #from 02-Data-Import script
#note to self - this may be reason family column is maintained in memory
#despite removing it.. since its a virtual object, not an opened csv


#this is for tree with total resolved species, no uncertain nodes
ausdata_all_pos_sp_tree <- read.tree("Inputs/Trees/ausdata_all_pos_sp.tre")


#sanity check, inverseA requires ultrametric trees
is.ultrametric(ausdata_all_pos_sp_tree)#TRUE


#look at node labels
table(ausdata_all_pos_sp_tree$node.label)
#remove internal node labels ""
ausdata_all_pos_sp_tree$node.label <- NULL 


#inverted phylogenetic covariance matrix
phylo_inv <- inverseA(ausdata_all_pos_sp_tree, nodes ="TIPS",scale=TRUE)
#may compare results with inverse matrix using nodes = ALL later

#inverse wishart prior for phylogeny
prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 0.02), #for phylo
    G2 = list(V = 1, nu = 0.02)  #for species
  ),
  R = list(V = 1, nu = 0.02) #residual 
)


#get column for phylo tip labels, and another for species
aus_data$phylo <- aus_data$species_binom


#prune aus_data to include exclusively species in tree of choice
ausdata_all_pos_sp <- aus_data %>% 
  filter(species_binom %in% ausdata_all_pos_sp_tree$tip.label)


#ensure columns are factors
ausdata_all_pos_sp$phylo <- factor(ausdata_all_pos_sp$phylo)
ausdata_all_pos_sp$species_binom <- factor(ausdata_all_pos_sp$species_binom)


#not matching up, match manually
ausdata_all_pos_sp$phylo <- factor(ausdata_all_pos_sp$phylo,
                                   levels = rownames(phylo_inv$Ainv))
all(levels(ausdata_all_pos_sp$phylo) %in% rownames(phylo_inv$Ainv)) #TRUE


#family column causes issues
ausdata_all_pos_sp <- ausdata_all_pos_sp %>%
  rename(fam = family)
ausdata_all_pos_sp$family <- NULL

#could be that model doesnt work on tibble, from stack overflow
ausdata_all_pos_sp <- as.data.frame(ausdata_all_pos_sp)


#trait distributions
ggplot(data = aus_data) +
  geom_histogram(mapping = aes(x = log(leaf_C_per_dry_mass))) +
  theme_minimal()


#aim for 1000-2000 effective sample size
tic("model run")
model <- MCMCglmm(ln_NP_ratio ~  SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                  + CEC_total_0_30 + AP_total_0_30 +
                  + NPP + MAT + PPT + AET +
                  + precipitation_seasonality + temp_seasonality,
                  random = ~ phylo +species_binom, #columns in data
                  family = "gaussian",
                  ginverse = list(phylo = phylo_inv$Ainv), prior = prior_phylo,
                  data = ausdata_all_pos_sp, nitt = 100000, burnin = 1000, thin = 10)
toc()
summary(model)
summary(model)$solutions
plot(model$Sol)
autocorr.plot(model$Sol)

tic("Model run")




# To do:
#IMPLEMENT CATEGORICAL DATA AS FIXED PREDICTOR
#need to do a script that has all models at once
#need to do one for each trait, strong and weak priors
#model convergence checks, sensitivity analyses, diagnostic plots
#feature selection
#distinct trees
#save model outputs

#final script should have:
#checks of distribution (histograms), feature selection
#diagnostic plots and convergence checks, sensitivity analyses
#save model outputs somewhere as actual object, or save summary stats, diagnostic plots

tic("test run")
test <- MCMCglmm(ln_NP_ratio ~  SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                     + CEC_total_0_30 + AP_total_0_30 +
                     + NPP + MAT + PPT + AET +
                     + precipitation_seasonality + temp_seasonality,
                   random = ~ phylo +species_binom, #columns in data
                   family = "gaussian",
                   ginverse = list(phylo = phylo_inv$Ainv), prior = prior_phylo,
                   data = ausdata_all_pos_sp, nitt = 26000, burnin = 2000, thin = 200)
toc()
summary(test) 
#estimates show posterior distribution for estimates, with credible intervals 
#here is attempt at gettning diagnostic plots

#----plotting histograms of random effect
par(mfrow = c(1,2)) #to set plot parameters
#two plots for each random effect
hist(mcmc(test$VCV)[,"phylo"])
hist(mcmc(test$VCV)[,"species_binom"])
#for random effect to be significant want it not to be pressed up agaisnt 0

#---- assess convergence of fixed effects
plot(test$Sol)

#assess convergence of random effects
plot(test$VCV)