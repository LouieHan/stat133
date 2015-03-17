# Please load in the dataset included in the midterm1 directory. It will be
# required to perform the following tasks. The dataset includes data for countries in 2012.

# your code here
load('SummerOlympics2012Ctry.rda')

# calculate the mean and the maximum of GDP in the dataset. Store these as the
# variables <mean.GDP> and <max.GDP> respectively.

mean.GDP <- mean(SO2012Ctry$GDP)
max.GDP <- max(SO2012Ctry$GDP)



# For each country in the dataset, calculate the number of female athletes (Female) divided
# by the total number of athletes (Female + Male). Store this as the variable
# <female.prop>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset.

female.prop <- SO2012Ctry$Female/(SO2012Ctry$Female + SO2012Ctry$Male)




# Create the following two subsets of the dataset and store them as variables with the
# indicated names:
# 1) Countries with 0 bronze medals: <subset.nobronze>
# 2) Countries with more than or exactly 3 bronze medals: <subset.threebronze>

subset.nobronze <- subset(x = SO2012Ctry, Bronze == 0, select = Country)
subset.threebronze <- subset(x = SO2012Ctry, Bronze >= 3, select = Country)




# For each of your subsets, create a vector giving the population size. Store
# these as variables <subset.nobronze.pop> and <subset.threebronze.pop>.

subset.nobronze.pop <- c(subset(x= SO2012Ctry, Bronze == 0, select = pop))
subset.threebronze.pop <- c(subset(x = SO2012Ctry, Bronze >= 3, select = pop))


# Implement the function medpopByGDPPP. Your function should take the following
# arguments:
#
# <GDPPP.cutoff>: a numeric constant giving a cutoff to subset by
# <GDPPP>: a numeric vector of GDP per person
# <pop>: a numeric vector of populations
#   (this should be the same length as <GDPPP>)
#
# Your function should return the median of the populations of countries
# whose values in <GDPPP> are strictly less that <GDPPP.cutoff>.

medpopByGDPPP <- function(GDPPP.cutoff, GDPPP, pop){
  test <- GDPPP < GDPPP.cutoff
  return(median(pop[test]))
  
}


"
medpopByGDPPP <- function(GDPPP.cutoff, GDPPP, pop){
  SO2012Ctry$GDPPP.cutoff = GDPPP.cutoff
  GDPPP.cutoff.vec <- c(subset(x = SO2012Ctry, GDPPP < GDPPP.cutoff, select = pop)
                        tease <- c(subset(x= SO2012Ctry, GDPPP < GDPPP.cutoff, select = pop))
                        med.pop <- median(tease)
                        return(med.pop)
}
"

# Please create a plot of the proportion of female athletes (y-axis) 
# against the total number of athletes (x-axis). Your plot should include the following 
# features:
# 1) a title "Proportion of female athletes vs Total # athletes"
# 2) axis labels: "Proportion of female athletes" and "Total # athletes"
# 3) plotting character set to 10
# 4) a red horizontal line at female proportion of 0.50.

plot(x = SO2012Ctry$Female + SO2012Ctry$Male, y = female.prop, 
    main = "Proportion of female athletes vs Total # athletes",
    xlab = "Total # of Athletes", ylab = "Proportion of Female Atheletes",
    abline(h = 0.5, col = "red"))
