# Populate all the lists
first_pct = c(53.8, 52.5, 26.5, 44.1, 40, 42.2, 41.8, 53.4, 36.1, 43, 41.9, 45.3, 46.1, 42.5, 50.8, 48.4, 52.7, 55.6, 41.4, 45.2)
last_pct = c(75, 100, 34.2, 40.2, 58.3, 23.7, 42, 44.7, 38.1, 44.2, 42.5, 21.4, 72.7, 41.1, 47.7, 45.9, 39.0, 57.1, 51.9, 33.9)
avg_pct = c(51.1, 46.8, 37.2, 44.9, 40.7, 45.6, 44.1, 48.4, 38.5, 45.2, 44.8, 47.2, 54.0, 44.5, 53.4, 44.7, 46.7, 58.4, 48.6, 41.8)

# Calculate the mean of the lists
first_mean = mean(first_pct)
last_mean = mean(last_pct)
avg_mean = mean(avg_pct)

# Calculate the variance of the lists
first_var = var(first_pct)
last_var = var(last_pct)
avg_var = var(avg_pct)

# Calculate the standard deviation of the lists
first_sd = sd(first_pct)
last_sd = sd(last_pct)
avg_sd = sd(avg_pct)

### Print first FG % info ###
print('List of starting FG %:')
print(first_pct)
print(paste('Starting FG % mean:', first_mean))
print(paste('Starting FG % variance:', first_var))
print(paste('Starting FG % standard deviation:', first_sd))

jpeg("first_scatter", width = 350, height = 350)
plot(first_pct)
dev.off()

jpeg("first_boxplot", width = 350, height = 350)
boxplot(first_pct)
dev.off()

jpeg("first_histogram", width = 350, height = 350)
hist(first_pct)
dev.off()

cat("\n")

### Print ending FG % info ###
print('List of ending FG %:')
print(last_pct)
print(paste('Ending FG % mean:', last_mean))
print(paste('Ending FG % variance:', last_var))
print(paste('Ending FG % standard deviation:', last_sd))

jpeg("last_scatter", width = 350, height = 350)
plot(last_pct)
dev.off()

jpeg("last_boxplot", width = 350, height = 350)
boxplot(last_pct)
dev.off()

jpeg("last_histogram", width = 350, height = 350)
hist(last_pct)
dev.off()

cat("\n")

### Print average FG % info ###
print('List of average FG %:')
print(avg_pct)
print(paste('Average FG % mean:', avg_mean))
print(paste('Average FG % variance:', avg_var))
print(paste('Average FG % standard deviation:', avg_sd))

jpeg("avg_scatter", width = 350, height = 350)
plot(avg_pct)
dev.off()

jpeg("avg_boxplot", width = 350, height = 350)
boxplot(avg_pct)
dev.off()

jpeg("avg_histogram", width = 350, height = 350)
hist(avg_pct)
dev.off()

cat("\n")

hypothesis_test1_95 = t.test(first_pct, last_pct)
print(hypothesis_test1_95)

hypothesis_test1_99 = t.test(first_pct, last_pct, conf.level=.99)
print(hypothesis_test1_99)
