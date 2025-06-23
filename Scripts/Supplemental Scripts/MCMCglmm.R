#-- Reference
library(ape)
library(MCMCglmm)


phylo<-read.nexus("phylo.nex") 
data_repeat<-read.table("data_repeat.txt",header=TRUE)

head(data_repeat)
#200 species, many observations per species

#specific mean of cofactor
data_repeat$spec_mean_cf<-sapply(split(data$cofactor,data$phylo),mean)[data$phylo]
#calculating the mean of the cofactor for each species

phylo.inv<-inverseA(phylo,nodes="TIPS",scale=TRUE)
prior2<-list(G=list(G1=list(V=1,nu=0.02),G2=list(V=1,nu=0.02)),R=list(V=1,nu=0.02))

model_repeat1<-MCMCglmm(phen~spec_mean_cf,random=~phylo+species,
                        family="gaussian",ginverse=list(phylo=phylo.inv$Ainv),
                        prior=prior2,data=data_repeat,nitt=5000000,burnin=1000,thin=500)
summary(model_repeat1)
lambda <- model_simple$VCV[,'phylo']/(model_simple$VCV[,'phylo']+model_simple$VCV[,'units'])
mean(lambda) 
posterior.mode(lambda)
HPDinterval(lambda)

#to distinguish between within species and between species variance
#within species, get deviation
#subtract species mean from individual cofactor value
data_repeat$within_spec_cf<-data_repeat$cofactor-data_repeat$spec_mean_cf

model_repeat2<-MCMCglmm(phen~spec_mean_cf+within_spec_cf,
                        random=~phylo+species,family="gaussian",
                        ginverse=list(phylo=phylo.inv$Ainv),prior=prior2,data=data_repeat,
                        nitt=5000000,burnin=1000,thin=500)

# ---- End of reference
library(ape)
library(MCMCglmm)
library(ggtree)
#this is for tree with total resolved species, no uncertain nodes
ausdata_all_pos_sp_tree <- read.tree("Inputs/Trees/ausdata_all_pos_sp.tre")

#unsure what this is for but here it is
is.ultrametric(ausdata_all_pos_sp_tree)#TRUE

phylo_inv <- inverseA(ausdata_all_pos_sp_tree,nodes="TIPS",scale=TRUE)
#says tip or node labels aren't unique
length(unique(ausdata_all_pos_sp_tree[["tip.label"]])) #829
#so they are unique... its probably internal node labels ""
ausdata_all_pos_sp_tree$node.label <- NULL 
#try again
phylo_inv <- inverseA(ausdata_all_pos_sp_tree,nodes="ALL",scale=TRUE)
#yay works! used nodes = ALL bc bigger phylogeny but idk if it matters

#get inverse wishart prior for phylogeny
V = 1
nu = 0.002

prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 0.02), #for phylo
    G2 = list(V = 1, nu = 0.02)  #for species
  ),
  R = list(V = 1, nu = 0.02) #residual
)

Nnitt=13000
Nthin=100
Nburnin=1000

#MVN prior for fixed effects

#get column for phylo tip labels, and another for species
aus_data$phylo <- aus_data$species_binom

#need to prune ausdata such that its only the 829 species
#in ausdata_all_pos_sp_tree

ausdata_all_pos_sp <- aus_data %>% filter(species_binom %in% 
                                            ausdata_all_pos_sp_tree$tip.label)
#4808 observations

#ensure columns are factors
ausdata_all_pos_sp$phylo <- factor(ausdata_all_pos_sp$phylo)
ausdata_all_pos_sp$species_binom <- factor(ausdata_all_pos_sp$species_binom)

#not matching up
#match manually
ausdata_all_pos_sp$phylo <- factor(ausdata_all_pos_sp$phylo,
                                   levels = rownames(phylo_inv$Ainv))
all(levels(ausdata_all_pos_sp$phylo) %in% rownames(phylo_inv$Ainv)) #TRUE

#could be that model doesnt work on tibble, from stack overflow
ausdata_all_pos_sp <- as.data.frame(ausdata_all_pos_sp)

#family column causes issues
ausdata_all_pos_sp <- ausdata_all_pos_sp %>%
  rename(fam = family)
ausdata_all_pos_sp$family <- NULL

ausdata_all_pos_sp <- as.data.frame(ausdata_all_pos_sp)

model <- MCMCglmm(ln_NP_ratio ~  SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                  + CEC_total_0_30 + AP_total_0_30 +
                  + NPP + MAT + PPT + AET +
                  + precipitation_seasonality + temp_seasonality,
                  random = ~ phylo +species_binom, #columns in data
                  family = "gaussian",
                  ginverse = list(phylo = phylo_inv$Ainv), prior = prior_phylo,
                  data = ausdata_all_pos_sp, nitt = Nnitt, burnin = Nburnin, thin = Nthin)

#set R to never stop running