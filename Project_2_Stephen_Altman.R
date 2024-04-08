install.packages("evaluate")
install.packages("factoextra")
install.packages("vtable")
install.packages("GGally")
library(vtable)
library(dbscan)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(DT)
library("GGally")

#######################################
#Data Preparation and Cleaning
#######################################

#read the data and store in allcases
allcases <- read_csv("COVID-19_cases_plus_census_wland.csv")
str(allcases)

#convert character values to factor
allcases <- allcases %>% mutate_if(is.character, factor)
dim(allcases)
str(allcases)

#create subset of data for Texas analysis
texascases <- allcases %>% filter(state == "TX")
dim(texascases)
summary(texascases)
str(texascases)
summary(texascases[,1:10])

#filter counties by those with more than 100 confirmed cases
#modify texascases to only include important variables
texascases <- texascases %>%
  filter(confirmed_cases > 100) %>%
  arrange(desc(confirmed_cases)) %>%
  select(county_fips_code, county_name,confirmed_cases,deaths,pop_density,median_income,
         poverty,commuters_by_public_transportation,worked_at_home,white_pop,
         black_pop,asian_pop,hispanic_pop,mobile_homes,employed_pop,unemployed_pop,
         high_school_including_ged,less_than_high_school_graduate,bachelors_degree,
         masters_degree,total_pop)
summary(texascases)
str(texascases)

#convert COVID data to per 1000 population
texascases <- texascases %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000,
  deaths_per_1000 = deaths/total_pop*1000,
  death_per_case = deaths/confirmed_cases)
summary(texascases)
str(texascases)

#scaling and normalization
texascases_scaled <- texascases %>%
  select(pop_density,median_income,poverty,commuters_by_public_transportation,
         worked_at_home,white_pop,black_pop,asian_pop,hispanic_pop,mobile_homes,
         employed_pop,unemployed_pop,high_school_including_ged,
         less_than_high_school_graduate,bachelors_degree,masters_degree) %>%
  scale() %>% as_tibble()
summary(texascases_scaled)

#create subsets from the unscaled dataset
# #create subset for the income and employment subset
# texascases_inc_emp <- texascases %>%
#   arrange(desc(confirmed_cases)) %>%
#   select(median_income,poverty,commuters_by_public_transportation,
#          worked_at_home,mobile_homes,employed_pop,unemployed_pop)
# summary(texascases_inc_emp)
# 
# #create subset for the income and employment subset
# texascases_dem_edu <- texascases %>%
#   arrange(desc(confirmed_cases)) %>%
#   select(white_pop,black_pop,asian_pop,hispanic_pop,high_school_including_ged,
#          less_than_high_school_graduate,bachelors_degree,masters_degree)
# summary(texascases_dem_edu)

ggplot(texascases, aes(county_name,less_than_high_school_graduate)) + geom_point()
boxplot(texascases$less_than_high_school_graduate)

#create subset from the scaled income and employment subset
texascases_inc_emp_scaled <- texascases_scaled %>%
  select(median_income,poverty,commuters_by_public_transportation,
         worked_at_home,mobile_homes,employed_pop,unemployed_pop)
summary(texascases_inc_emp_scaled)

#create subset for the income and employment subset
texascases_dem_edu_scaled <- texascases_scaled %>%
  select(white_pop,black_pop,asian_pop,hispanic_pop,high_school_including_ged,
         less_than_high_school_graduate,bachelors_degree,masters_degree)
summary(texascases_dem_edu_scaled)

#######################################
#Project Report Output
#######################################

#generate table of summary statistics
sumtable(texascases,col.breaks=6,
         summ=list(
           c('mean(x)','sd(x)','median(x)','min(x)','max(x)')),
         summ.names=list(
           c('Mean','SD','Median','Min','Max')
         ))

####################################
#DBSCAN Clustering
####################################

#Subset 1

kNNdistplot(texascases_inc_emp_scaled,k=4)
abline(h=.95,lty=2,col="red")

db_inc_emp <- dbscan(texascases_inc_emp_scaled,eps=.95,minPts=5)
db_inc_emp

fviz_cluster(db_inc_emp,texascases_inc_emp_scaled,geom="point")


#Subset 2

kNNdistplot(texascases_dem_edu_scaled,k=9)
abline(h=.75,lty=2,col="red")

db_dem_edu <- dbscan(texascases_dem_edu_scaled,eps=.75,minPts=10)
db_dem_edu

fviz_cluster(db_dem_edu,texascases_dem_edu_scaled,geom="point")

texascases_dem_edu_outliers <- texascases_dem_edu_scaled[db_inc_emp$cluster == 0, ]
texascases_dem_edu_outliers
texascases_dem_edu_cln <- texascases_dem_edu_scaled[db_inc_emp$cluster != 0, ]
texascases_dem_edu_cln

kNNdistplot(texascases_dem_edu_cln,k=20)
abline(h=.3,lty=2,col="red")

db_dem_edu_cln <- dbscan(texascases_dem_edu_cln,eps=.3,minPts=21)
db_dem_edu_cln

fviz_cluster(db_dem_edu_cln,texascases_dem_edu_cln,geom="point")


################################################################
#measuring outliers
ggpairs(texascases_scaled,progress=FALSE)
ggpairs(texascases_inc_emp_scaled,progress=FALSE)
ggpairs(texascases_dem_edu_scaled,progress=FALSE)

#running LOF (local outlier factor)
lof <- lof(texascases_scaled, minPts=10)
lof

ggplot(texascases_inc_emp_scaled |> add_column(lof = lof), aes(median_income,poverty, color = lof)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")

ggplot(tibble(index = seq_len(length(lof)), lof = sort(lof)), aes(index, lof)) +
  geom_line() +
  geom_hline(yintercept = 1.5, color = "red", linetype = 2)

ggplot(texascases_inc_emp_scaled |> add_column(outlier = lof >= 1.5), aes(median_income,poverty, color = outlier)) +
  geom_point()

str(texascases_inc_emp_scaled)

texascases_inc_emp_scaled_outliers <- texascases_inc_emp_scaled |> filter(lof > 1.5)
texascases_scaled_outliers
texascases_scaled

##################################################################
#Hierarchal Clustering
##################################################################

#Subset 1
##########################

#Set distance to variable 'd'
d <- dist(texascases_inc_emp_scaled)

hc_inc_emp <- hclust(d,method="complete")
plot(hc_inc_emp)

#build dendrogram
fviz_dend(hc_inc_emp,k=5)

#extract the different clusters and add the cluster id to the dataset
clusters <- cutree(hc_inc_emp, k = 5)
hc_inc_emp_complete <- texascases_inc_emp_scaled |>
  add_column(cluster = factor(clusters))
hc_inc_emp_complete

#plot the hierarchical cluster
fviz_cluster(list(data = texascases_inc_emp_scaled, cluster = cutree(hc_inc_emp, k = 5)), geom = "point")

#Subset 2
##########################

#Set distance to variable 'd'
d <- dist(texascases_dem_edu_scaled)

hc_dem_edu <- hclust(d,method="complete")
plot(hc_dem_edu)

#build dendrogram
fviz_dend(hc_dem_edu,k=3)

#extract the different clusters and add the cluster id to the dataset
clusters <- cutree(hc_dem_edu, k = 3)
hc_dem_edu_complete <- texascases_dem_edu_scaled |>
  add_column(cluster = factor(clusters))
hc_dem_edu_complete

#plot the hierarchical cluster
fviz_cluster(list(data = texascases_dem_edu_scaled, cluster = cutree(hc_dem_edu, k = 5)), geom = "point")



###################
#visualize on a map
###################

#Import the county dataset
counties <- as_tibble(map_data("county"))

#create new subset for Texas counties
texascounties <- counties %>% dplyr::filter(region == "texas") %>%
  rename(c(county = subregion))

#match the county names from COVID-19 dataset match the county dataset
texascases <- texascases %>% mutate(county = county_name %>%
                str_to_lower() %>% str_replace('\\s+county\\s*$',''))

#use leftjoin to create cluster dataset
texascounties_clust <- texascounties %>% left_join(texascases)

#plot on a map
ggplot(texascounties_clust,aes(long,lat)) +
  geom_polygon(aes(group=group,fill=deaths_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type="viridis") +
  labs(title="Clusters",subtitle="Only Counties Reporting 100+ Cases")

##############################################
#build datatable with the important values
datatable(texascases)
str(texascases)
datatable(texascases,filter = 'top') %>% formatRound(c(5,21,22),2) %>% formatPercentage(23)


####################################
#OLD DBSCAN Code
####################################

#Subset 1
#####################
#convert to tibble
texascases_inc_emp <- as_tibble(texascases_inc_emp) |>
  sample_frac()

summary(texascases_inc_emp)

#data cleaning
ggplot(texascases_inc_emp, aes(x=x,y=y)) + geom_point()

#scale the dataset
scale_numeric <- function(x) mutate_if(x, is.numeric, function (y) as.vector(scale(y)))

texascases_inc_emp_scaled <- texascases_inc_emp |>
  scale_numeric()
summary(texascases_inc_emp_scaled)

kNNdistplot(texascases_inc_emp_scaled,k=4)
abline(h=.15,col="red")

db_inc_emp <- dbscan(texascases_inc_emp_scaled,eps=.15,minPts=3)
db_inc_emp

ggplot(texascases_inc_emp_scaled |> add_column(cluster=factor(db_inc_emp$cluster)),
       aes(x,y,color=cluster)) + geom_point()




#Subset 2
#####################
#convert to tibble
texascases_dem_edu <- as_tibble(texascases_dem_edu) |>
  sample_frac()

summary(texascases_dem_edu)

#scale the dataset
scale_numeric <- function(x) mutate_if(x, is.numeric, function (y) as.vector(scale(y)))

texascases_dem_edu_scaled <- texascases_dem_edu |>
  scale_numeric()
summary(texascases_dem_edu_scaled)

kNNdistplot(texascases_dem_edu_scaled,k=6)
abline(h=0.7,col="red")

db_dem_edu <- dbscan(texascases_dem_edu_scaled,eps=.7,minPts=7)
db_dem_edu