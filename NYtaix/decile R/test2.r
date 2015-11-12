
dat = read.csv("trip_fare_1.csv")
quantile(dat$total_amount - dat$tolls_amount, prob = 0.1)
