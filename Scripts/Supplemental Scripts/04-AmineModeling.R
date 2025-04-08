#Variance Partitioning Example

# Load the vegan library
library(vegan)
# Load the mite dataset
data(mite) #35 species, 70 sites
data(mite.env) #env variables for each of the 70 sites 
data(mite.xy) #geographical coordinates of 70 sites


# Create tables and consider a Hellinger transformation
Y <- decostand(mite, method="hellinger")#this takes square root of site proportions
                    #Y = species abundance matrix
X <- mite.env[,1:2] #X = env
W <- mite.xy        #W = spatial (geographical coordinates)

View(cbind(X,W))

#multivariate extension of lm
#if Y had a single variable, same result as lm()
rda_xw <- rda(Y, cbind(X, W)) #A + B + C
#cbind here just combines X and Y
rda_x <- rda(Y, X)            #A + B
rda_w <- rda(Y, W)            #B + C



# Compute the adjusted R2 values
abc_frac <- RsquareAdj(rda_xw)$adj.r.squared
ab_frac <- RsquareAdj(rda_x)$adj.r.squared
bc_frac <- RsquareAdj(rda_w)$adj.r.squared

# Compute the adjusted R2 values
a_frac <- abc_frac - bc_frac
c_frac <- abc_frac - ab_frac
b_frac <- ab_frac - a_frac
d_frac <- 1.0 - abc_frac
#
c(a_frac, c_frac, b_frac, d_frac) #manually

# Compute the adjusted R2 values
varpart(Y, X, W)$part$indfract[,3] #gives the same!
#stuff after $is just to clarify it for me... without it
#varpart summary is very complicated

# Plot the fractions of variation
plot(varpart(Y, X, W))

#in my case, replace spatial variables by phylogenetic PCS
#i.e. W = phylo PCs