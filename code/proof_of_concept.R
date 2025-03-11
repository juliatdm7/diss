########################
### PROOF OF CONCEPT ###
########################

x <- c(1:8)  # Number of breeding attempts
y <- c(805, 197, 78, 36, 17, 0, 0, 1)  # I need to change this so that it directly references the database
brattempts_age <- data.frame(x, y)

plot(brattempts_age, type = "b", xlab="Number of breeding attempts recorded", ylab="Number of females")
