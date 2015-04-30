#I'm going to make a PCA analysis of a dataset containing R.
# For this practice, I chose the dataset "diamonds" as I find quite interesting for the many
# Of information presented.

# Try to find out which variables of the 10 having the dataset, are the most relevant.

# The different indicators present in the dataset "diamonds" are:

# Price: Price in US Dollars ($ 326- $ 18.823)
# Carat: diamond weight (0.2-5.01)
# Cut: cutting quality (Fair, Good, Very Good, Premium, Ideal)
# Color: color diamond (from D to J the best worst)
# Clarity: measures how clear is the diamond (from the best SI1, SI2, VS1, VS2, VVS1, VVS2, IF, even the worst I1)
# X: length in mm (0-10.74)
# Y: width in mm (0-58.9)
# Z: depth in mm (0-31.8)
# Depth: percent total depth (total depth percentage = z / mean (x, y) = 2 * z / (x + y) (43-79))
# Table: width of the top of diamond relative to widest point (43-95)


# 0- library installation.

library ( ggplot2 )
library ( quantmod )

### To create 3D plots have to install the package RLG
library ( RGL )

### Packete RDRToolbox Reduction Dimensions Nonlinear
library ( RDRToolbox )
library ( dplyr )

# 1- We get the diamonds dataset.
data ( diamonds )

# 2- quickly analyze the data in the dataset.

summary ( diamonds ) # get an overview of the data

# 3 - Modification of data
# Because there are three very important variables that are not numerical, we perform a reclassification for inclusion
# The target variables will change the cut, color and clarity are values that factor to be passed to numeric values

# Create a new data frame to modify 

df.diamonds  <-  diamonds


# We change the values of the variable cut, and rename the 1 to 5, for better (Ideal) to worst (Fair):

df.diamonds $ cut2  <- as.numeric ( df.diamonds $ cut )

df.diamonds $ cut2 [ df.diamonds $ cut  ==  " Ideal " ] <-  1
df.diamonds $ cut2 [ df.diamonds $ cut  ==  " Premium " ] <-  2
df.diamonds $ cut2 [ df.diamonds $ cut  ==  " Very Good " ] <-  3
df.diamonds $ cut2 [ df.diamonds $ cut  ==  " Good " ] <-  4
df.diamonds $ cut2 [ df.diamonds $ cut  ==  " Fair " ] <-  5

# Do the same for the color variable:

df.diamonds $ color2  <- as.numeric ( df.diamonds$color )

df.diamonds $ color2 [ df.diamonds $ Color  ==  " D " ] <-  1
df.diamonds $ color2 [ df.diamonds $ Color  ==  " E " ] <-  2
df.diamonds $ color2 [ df.diamonds $ Color  ==  " F " ] <-  3
df.diamonds $ color2 [ df.diamonds $ Color  ==  " G " ] <-  4
df.diamonds $ color2 [ df.diamonds $ Color  ==  " H " ] <-  5
df.diamonds $ color2 [ df.diamonds $ Color  ==  " I " ] <-  6
df.diamonds $ color2 [ df.diamonds $ Color  ==  " J " ] <-  7


# Do the same for clarity variable:

df.diamonds $ Clarity2  <- as.numeric ( df.diamonds $ clarity )

df.diamonds $ Clarity2 [ df.diamonds $ clarity  ==  " IF " ] <-  1
df.diamonds $ Clarity2 [ df.diamonds $ clarity  ==  " VVS1 " ] <-  2
df.diamonds $ Clarity2 [ df.diamonds $ clarity  ==  " VVS2 " ] <-  3
df.diamonds $ Clarity2 [ df.diamonds $ clarity  ==  " VS1 " ] <-  4
df.diamonds $ Clarity2 [ df.diamonds $ clarity  ==  " VS2 " ] <-  5
df.diamonds $ Clarity2 [ df.diamonds $ clarity  ==  " SI1 " ] <-  6
df.diamonds $ Clarity2 [ df.diamonds $ clarity  ==  " SI2 " ] <-  7
df.diamonds $ Clarity2 [ df.diamonds $ clarity  ==  " I1 " ] <-  8


# Once we have all the variables as numeric, I created a data frame with the columns that interest me:
df.estudio  <-  df.diamonds [c ( 1 , 5 : 13 )]


#STUDY PCA ############################## ################## #############################

# Conduct a preliminary analysis to see means and variances of each variable:
apply ( df.estudio , 2 , mean )
apply ( df.estudio , 2 , var )

# Prcomp calculated with normalized data, with mean 0 and variance 1
pr.out  = prcomp ( df.estudio , scale = TRUE )   # with scale = True we get all the data have the same scale

# We make a biplot for the first 2 components and their values
biplot ( pr.out , scale  =  0 )

#Information from each compenentes that gives me pr.out We will get:
names ( pr.out ) # get each component.
pr.out $ SDEV  # I get the standard deviation
pr.out $ rotation  P # I get the base change matrix
pr.out $ x  # I get the array data and after the change of base

# To identify key addresses that I will stay, I can use two methods including:
# Criteria of the "elbow" which will look like the slope decreases in func of the main components that have
# Calculating the proportion of variance explained (PVE)

# First we calculate the PVE:
## Calculation of the variance.
pr.var  <-  pr.out $ SDEV ^ 2

## Calculation of proportion PVE 
pve  <-  pr.var / sum ( pr.var )
pve

library(base)
cumSum ( PVE ) *  100  # see the cumulative percentage of each major component.

## You can see that with the election of the first two components 64.4½ explains the variation in the data and 
## If I select the first 3 components, increased to 76.8%

# Represent the opinion of the elbow:
plot ( pve ,   xlab = " Major Component " , ylab = " Proportion of Variance Explained " , type  =  ' b ' )
plot (cumSum ( PVE ), xlab = " Major Component " , ylab = " Proportion of Variance Explained Acumulatica " , ylim = c ( 0 , 1 ), type = ' b ' )


## After observing the resulting graph, we see that clearly would explain choosing the first 2 components
## Perfectly variation of the data, while choosing three aunmente the PVE, however, figure in the "elbow" there are hardly any variation.

## As we know that the first 2 CP will be enough, I analyze correlations between variables, performing again:
pr.out $ rotation

# NOTES:
# You see something very curious with the information obtained. You can see a high correlation in respect PC1 variables
# "Carat", "price", "x", "y", "z", which can yield several conclusions:
#Clearly 1- # x, y, z, ie fully diamond size influence weight ("carat") thereof.
# 2- can deduce that what most affects the price of a diamond will be your weight and obviously its dimensions.
# 3- Unlike what I thought before the study, cut, clarity and color do not significantly influence the price.

# If we analyze the second component PC2, we observed was a top correlation in the variables "table" and "cut2" what we can deduce:
# 1- The variable "table" we indicated the width of the top of the diamond in relation to wider pnto,
# Is completely normal that this value depends on the type of diamond cut, because depending on the type of cut, shape
# Diamond changes completely.


STUDY ################################### ISOMAP ############# ##########################################

## Because the dataset "df.estudio" presents many records, my virtual machine can not process all the information
# So I decide to sample 500 records instead of the 53 940 it has.

# You can check the size of the data frame with which I can not run Isomap:
dim (as.matrix ( df.estudio )) # is 53940 rows and 10 columns, which are the variables

muestra.estudio  <-  df.estudio [ 1 : 500 ,]

# Estimate the dimension drawing residue with 4 neighbors.
IsomapIndicadores_1to10  = Isomap ( data  = as.matrix ( muestra.estudio ) dims = 1 : 10 , k = 4 , plotResiduals = TRUE )
# You can see a Isomap After this, as the dimension from the graph decays 2-3.


2 #Calculamos dimensions Isomap
IsomapIndicadores_2  = Isomap ( data  = as.matrix ( muestra.estudio ), dim  =  2 , k  =  4 )
# 2 Plot size
plotDR ( data = IsomapIndicadores_2 $ dim2 , axesLabels  = c ( " CP1 " , " CP2 ' ))


3 dimensions Isomap #Calculamos
IsomapIndicadores_3  = Isomap ( data  = as.matrix ( muestra.estudio ), dim  =  3 , k  =  4 )
# 3 Plot size
plotDR ( data = IsomapIndicadores_3 $ dim3 , axesLabels  = c ( " CP1 " , " CP2 ' , ' CP3 ' ))

# You can see that we do not lose a lot of information going from 3-2 dimensions.
# We were intrinsically dimension 2 for further analysis.
# You can observe a kind of parable that created the data.
# Data are grouped but when V2 <5 V1 <10

# Create a data filtered with intrinsic dimensions 2 above where the data are grouped:
Isomap_filtrado_v1  <- as.data.frame ( row.names  = rownames ( muestra.estudio ) IsomapIndicadores_2 $ dim2 )

# Indian values V1 and V2 where data is concentrated:
Isomap_filtrado_v2  <- rownames ( Isomap_filtrado_v1 [ Isomap_filtrado_v1 $ V1  <  10  &  filter_isomap $ V2  <  5 ,])

#Obtenemos One dataframe with these filtered data:
df.filtrado.isomap  <-  muestra.estudio [ Isomap_filtrado_v2 ,]

# Now if we compare the results with those obtained in the original dataset, we see that are very similar data:

summary ( df.filtrado.isomap )
summary ( df.estudio )

# You can infer that the study of intrinsic variables ISOMAP 2 is correct and lose virtually no information
# From the original dataset.
