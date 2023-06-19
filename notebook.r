
# Check the name of the current folder
current_dir <- getwd() 
print(current_dir)

# List all files in this folder
file_list <- list.files(current_dir) 
print(file_list)

# List files inside the datasets folder
file_list_ds <- list.files("/home/repl/datasets") 
print(file_list_ds)

# View the first 20 lines of road-accidents.csv in the datasets folder
accidents_head <- readLines("datasets/road-accidents.csv", n = 20) 
print(accidents_head)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

# Then follows one or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.

run_tests({
    test_that("the answer is correct", {
        expect_true(current_dir == getwd(), 
                    info = "Did you correctly assign the current_dir variable?")
        expect_true( 'road-accidents.csv'%in%file_list_ds, 
                    info = "Did you use the list.files function with the path argument to get the files inside the datasets folder?")
        expect_equal(file_list , list.files() , 
                     info = "Did you correctly assign the current_dir variable?")
        expect_true(length(accidents_head) == 20, 
                    info = "Make sure you read in the correct number of lines by using the n argument.")
    })
})

# Load the tidyverse library
library(tidyverse, quietly = TRUE)

# Read in road-accidents.csv and set the comment argument
car_acc <- read_delim("datasets/road-accidents.csv", 
                      delim = "|",
                     comment = "",
                      col_names = TRUE,
                      skip = 9
                     )

# Save the number of rows columns
rows_and_cols <- dim(car_acc)
print(rows_and_cols)

# Generate an overview of the data frame
str(car_acc)

# Display the last six rows of the data frame. 
tail(car_acc)


# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
    expect_is(car_acc, "tbl_df" , 
        info = "Did you use the read_delim() function to read the file? Did you set comment='#'?")
    expect_equal(nrow(car_acc), 51 , 
        info = "Did you tell R that in the file comments are indicated by # ?")
    expect_equal(ncol(car_acc), 5 , 
        info = "Did you tell R that in the file a new column is indicated by | ?")
    expect_equal( length(rows_and_cols), 2 , 
        info = "Did you use dim() to determine both the number of rows and columns?")
    })
    # You can have more than one test
})

# Compute summary statistics of all columns in the car_acc data frame
dat_summ <- summary(car_acc)
print(dat_summ)

# Deselect the state column and create a pairwise scatterplot
library(GGally)
car_acc %>% 
    select(-state) %>%
    ggpairs()

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.


run_tests({
    test_that("the answer is correct", {
        expect_is(dat_summ, "table", info = "Did you use `summary()` to obtain a summary of car_acc?")
        })
    p <- last_plot()
    test_that("the plot is correct", {
        expect_is(p, "ggmatrix", info = "Did you create a figure using ggpairs()?")    
    })
})

# Using pipes, remove the state column and then compute the correlation coefficient for all column pairs 
corr_col <- car_acc %>% 
select(-state) %>% 
cor()

# Print the correlation coefficient for all column pairs
print(corr_col)

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
        expect_is(corr_col, "matrix" , 
                  info = "Did you remove the 'state' column before computing the correlation?")
        expect_equal(prod(dim(corr_col)), 16 , 
                     info = "Did you use the cor function on the data frame after removing the 'state' column?")
    })
})

# Use lm to fit a multivariate linear regression model 
fit_reg <- lm(drvr_fatl_col_bmiles ~ perc_fatl_speed + perc_fatl_alcohol + perc_fatl_1st_time, data = car_acc)

# Retrieve the regression coefficients from the model fit
fit_coef <- coef(fit_reg)
print(fit_coef)

# One or more tests of the student's code.  
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
        expect_is(fit_reg, "lm" , 
                  info = "Did you use the lm function?")
        expect_true( near(fit_coef[1],9.06498048340333) , 
                    info = "Did you give lm the correct linear model formula containing all three predictors?")
    })
})

# Center and standardise the three feature columns
car_acc_standised <- car_acc %>% 
    mutate(perc_fatl_speed = scale(perc_fatl_speed),
           perc_fatl_alcohol = scale(perc_fatl_alcohol),
           perc_fatl_1st_time = scale(perc_fatl_1st_time))

# Perform PCA on standardized features
pca_fit <- princomp(car_acc_standised[, -c(1,2)])

# Obtain the proportion of variance explained by each principle component
pr_var <- pca_fit$sdev^2
pve <- pr_var / sum(pr_var)

# Plot the proportion of variance explained, draw a point plot connected with lines
data_frame( comp_id=1:length(pve) , pve ) %>%
ggplot( aes(x=comp_id , y=pve) ) + geom_point() + geom_line() +
coord_cartesian(ylim=c(0,1)) +
labs(x="Principal Component", 
     y="Proportion of Variance Explained")

# Compute the cumulative proportion of variance and extract the variance
# explained by the first two principal components
cve <- cumsum(pve)
cve_pc2 <- cve[1:2]
print(cve_pc2)

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
p <- last_plot()

run_tests({
    test_that("the computations are correct", {
        expect_true( near( mean(car_acc_standised$perc_fatl_speed)+
                      mean(car_acc_standised$perc_fatl_alcohol)+
                      mean(car_acc_standised$perc_fatl_1st_time),0) , 
                    info = "Did you use the scale function to create car_acc_standised?")
        expect_true( near(pr_var[1] , 1.34332588935974 ) , 
                info = "Did you compute the proportion of explained variance by taking the square of pca_fit$sdev?")
        expect_true( near(cve[2] ,  0.794697860810483 ) ,
                info = "Did you correctly compute the cumulative variance explained from pve by using the cumsum function?")
        })
    test_that("the plot is correct", {
        p <- last_plot()
        correct_geoms <- c(class(p$layers[[1]]$geom)[1], class(p$layers[[2]]$geom)[1])

        expect_true(class(p$layers[[1]]$geom)[1] %in% correct_geoms, 
                    info = "The plot is incorrect. Did you use a line and a point geom?")
        expect_true(class(p$layers[[2]]$geom)[1] %in% correct_geoms, 
                    info = "The plot is incorrect. Did you use a line and a point geom?")
    })
})

# Get the principle component scores from the PCA fit
pcomp1 <- pca_fit$scores[,"Comp.1"]
pcomp2 <- pca_fit$scores[,"Comp.2"]

# Plot the first 2 principle components in a scatterplot using ggplot
data_frame(pcomp1,pcomp2) %>%
ggplot(aes(pcomp1, pcomp2)) +
geom_point()

# One or more tests of the student's code.
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
p <- last_plot()

run_tests({
    test_that("the answer is correct", {
    expect_true( near(sum(pcomp1+pcomp2),-2.74780198594726e-15) , 
        info = "Did you extract the principle compenent scores from the PCA fit using pca_fit$scores[,1] and pca_fit$scores[,2]?") 
      })
    test_that("the plot is correct", {
        expect_is(p, "ggplot", info = "Did you create a ggplot figure?")
        expect_is(p$layers[[1]]$geom, "GeomPoint", info = "Did you create a plot with geom_point()?")
    })
})

# Create a vector of 1 to 10 
k_vec <- 1:10

# Initialise vector of inertias
inertias <- rep(NA, length(k_vec))

# Initialise empty list to save K-mean fits 
mykm <- list()

# Set the seed of random number generator 
set.seed(1)
for (k in k_vec) {
    # for each k, fit a K-mean model with k clusters and save it in the mykm list
    mykm[[k]] <- kmeans(car_acc_standised[,c(3,4,5)], centers = k, nstart=50)
    # for each k, get the within-cluster sum-of-squares and save
    inertias[k] <- mykm[[k]]$tot.withinss             
}

# Plot the within-cluster sum-of-squares against the number of clusters used
data_frame(k_vec,inertias) %>%
ggplot( aes(k_vec, inertias) ) +
geom_point() + geom_line() +
labs(x="Number of clusters", y="Intertias")

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
get_aesthetics <- function(p) {
    unlist(c(list(p$mapping), purrr::map(p$layers, "mapping")))
}
run_tests({
    test_that("the answer is correct", {
        expect_equal( sum(k_vec) , 55 , 
                     info = "Did you create a vector that goes from 1 to 10 by increments of 1?")
        expect_is( mykm[[1]] , 'kmeans' , 
                  info = "Did you use to kmeans function to populate mykm?")
        expect_equal( max(mykm[[9]]$cluster) , 9 , 
                     info = "Did you use of the 'centers' argument of the kmean function so that 
                            the number of centers changes with each iteration?") 
    })
    p <- last_plot()
    test_that("plot is correct", {
        expect_is(p, "ggplot", info = "Did you create a ggplot figure?")  
        expect_equal(length(p$layers), 2, info = "Did you create a plot with geom_line() and geom_point()?")
        aesthetics <- get_aesthetics(p)
        expect_equal(rlang::quo_name(aesthetics$x), "k_vec",
                     info = "Did you put k_vec on the x-axis?")
        expect_equal(rlang::quo_name(aesthetics$y), "inertias",
                     info = "Did you put inertias on the y-axis?")
    })
})

# Obtain cluster-ids from the kmeans fit with k=3
cluster_id <- as.factor(mykm[[3]]$cluster)

# Color the points of the principle component plot according to their cluster number
data_frame(pcomp1,pcomp2) %>%
ggplot(aes(x=pcomp1,y=pcomp2, color = cluster_id)) + geom_point() +
labs(x="Principle Component 1",
    y="Principle Component 2")

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
get_aesthetics <- function(p) {
    unlist(c(list(p$mapping), purrr::map(p$layers, "mapping")))
}

run_tests({
    test_that("the answer is correct", {
        expect_equal( length(unique(cluster_id)), 3 , 
                     info = "Did you access the cluster element of mykm?")
    })
    test_that("the plot is correct", {
         p <- last_plot()
         aesthetics <- get_aesthetics(p)
         expect_equal(rlang::quo_name(aesthetics$colour), "cluster_id",
                     info = "Did you map the cluster_id to the color using aes()?")
    })
})

# Add cluster_id to the original data frame
car_acc$cluster <- cluster_id

# Get the data into long format and plot
car_acc %>%
    select(-2) %>% 
    gather(key=feature, value=percent, -state, -cluster) %>%
    ggplot(aes(x=feature,y=percent, color = cluster)) +
    geom_violin() +
    coord_flip()

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("the answer is correct", {
    expect_equal( length(unique(car_acc$cluster)) , 3 , 
        info = "Did you add the cluster_id vector to the carr_acc data frame?")
        })
    test_that("the plot is correct", {
        p <- last_plot()
        expect_is(p, "ggplot", info = "Did you create a ggplot figure?")
        expect_equal(p$data, car_acc %>%  select(-drvr_fatl_col_bmiles) %>% 
                gather(key=feature,value=percent,-state,-cluster), 
                     info = "Did you create your plot out after renoming drvr_fatl_col_bmiles and after gathering your data?")
        expect_is(p$layers[[1]]$geom, "GeomViolin", info = "Did you create a plot with geom_violin()?")
    })
})

# Read in the miles-driven.csv file
miles_driven <- read_delim( file="datasets/miles-driven.csv", delim = '|' )

# Join miles_driven with car_acc and add num_drvr_fatl_col 
carr_acc_joined <- car_acc  %>% 
  left_join(miles_driven, by = "state") %>% 
  mutate(num_drvr_fatl_col = (drvr_fatl_col_bmiles * million_miles_annually) / 1000)

# Group the new data frame, select relevant variables, and summarise 
carr_acc_joined_summ <- carr_acc_joined %>%
    group_by(cluster) %>%
    select(cluster,num_drvr_fatl_col) %>%
    summarise(count=n(),
              mean=mean(num_drvr_fatl_col),
              sum=sum(num_drvr_fatl_col))
print(carr_acc_joined_summ)

# Compare the total fatal accident sum across clusters using a bar plot
carr_acc_joined_summ %>%
    ggplot(aes(x=cluster, y=sum)) +
    geom_bar(aes(fill = cluster), stat = "identity", show.legend = F)

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
soln_carr_acc_joined <- carr_acc_joined %>% 
    mutate( num_drvr_fatl_col=drvr_fatl_col_bmiles*million_miles_annually/1000 )

run_tests({
    test_that("the answer is correct", {
        expect_is(miles_driven, "tbl_df" , 
                  info = "Did you load the miles-driven.csv file using read_delim()?")
        expect_equal(sum(dim(carr_acc_joined)), 59 , 
                     info = "Did you use the left_join() function and joined the data frames by the state column?")
    })
    test_that("num_drvr_fatl_col was made correctly", {
        expect_equal(soln_carr_acc_joined$num_drvr_fatl_col, carr_acc_joined$num_drvr_fatl_col, 
                    info = "Did you correctly calculate the total number of deadly accidents? 
                            try: drvr_fatl_col_bmiles * million_miles_annually / 1000")
    })
    test_that("the plot is correct", {
        p <- last_plot()
        expect_is(p, "ggplot", info = "Did you create a ggplot figure?")
        expect_equal(p$data, carr_acc_joined_summ, info = "Did you create your plot out of carr_acc_joined_summ?")
        expect_is(p$layers[[1]]$geom, "GeomBar", info = "Did you create a plot with geom_bar()?")
    })
})

# Which cluster would you choose?
cluster_num <- 2

# One or more tests of the student's code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("Well done! Note that there is no definite correct answer here and there are a few ways to justify each cluster choice:'
           '\n1 (Red) = The highest number of people helped in total and the most states. Good if we can mobilize many resources right away.'
           '\n3 (Blue) = The lowest number of states and the highest number of people helped per state. Good for a focused pilot effort.'
           '\n2 (Green) = A good balance of the attributes from the two other clusters. This cluster also has the highest alcohol consumption'
           '\nwhich was the strongest correlated to fatal accidents.", {
        expect_true(cluster_num %in% c(1,2,3), 
                    info = "cluster_num must be either 1, 2, or 3")
    })
    # You can have more than one test
})

# Python test
# def test_cluster_choice():
#     assert cluster_num in range(3), \
#     'cluster_num must be either 0, 1, or 2'
#     print('Well done! Note that there is no definite correct answer here and there are a few ways to justify each cluster choice:'
#           '\n0 (Blue) = The lowest number of states and the highest number of people helped per state. Good for a focused pilot effort.'
#           '\n2 (Green) = The highest number of people helped in total and the most states. Good if we can mobilize many resources right away.'
#           '\n1 (Orange) = A good balance of the attributes from the two other clusters. This cluster also has the highest alcohol consumption'
#           '\nwhich was the strongest correlated to fatal accidents.')
