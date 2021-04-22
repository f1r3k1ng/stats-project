#install.packages("qqplotr")
library("qqplotr")

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

shapiro_first = shapiro.test(first_pct)
shapiro_last = shapiro.test(last_pct)
shapiro_avg = shapiro.test(avg_pct)

print("The Shapiro Wilks test results for first year %, last year %, and avg %, respectivly.")
print(shapiro_first)
print(shapiro_last)
print(shapiro_avg)

print("Hypothesis tests to check if the first year % and last year % are equal, first result is at a 95% CI and second is at 99% CI.")
hypothesis_test1_95 = t.test(first_pct, last_pct)
print(hypothesis_test1_95)

hypothesis_test1_99 = t.test(first_pct, last_pct, conf.level=.99)
print(hypothesis_test1_99)



data = data.frame(first_pct, last_pct, avg_pct)
my_data = data.frame(first_pct, last_pct, avg_pct)

jpeg("qqplot_first", width = 350, height = 350)
ggplot(data = data, mapping = aes(sample = first_pct)) + stat_qq_band() + stat_qq_line() + stat_qq_point()
dev.off()

jpeg("qqplot_last", width = 350, height = 350)
ggplot(data = data, mapping = aes(sample = last_pct)) + stat_qq_band() + stat_qq_line() + stat_qq_point()
dev.off()

jpeg("qqplot_avg", width = 350, height = 350)
ggplot(data = data, mapping = aes(sample = avg_pct)) + stat_qq_band() + stat_qq_line() + stat_qq_point()
dev.off()

names(my_data)[names(my_data) == "first_pct"] <- "First %"
names(my_data)[names(my_data) == "last_pct"] <- "Last %"
names(my_data)[names(my_data) == "avg_pct"] <- "Avg %"

print(my_data)
cat("\n")

bartlett = bartlett.test(data)
print("Bartlett test is used to test that variance is homogenous across the different samples")
print(bartlett)

print("An anova test was used to determine of all of the means were equal to each other")

all = c(first_pct, last_pct, avg_pct)
group = c(rep(1, 20), rep(2, 20), rep(3, 20))
anova = data.frame(all, group)
result = aov(all ~ group, data = anova)
print(result)
summary(result)

dc = c(53.4, 41.7, 49.0, 51.9, 49.7, 46.7, 48.6, 49.1, 47.5, 48.1, 46.1,37.9, 44.7)
tb = c(41.9, 47.8, 42.0, 44.8, 46.5, 43.8, 46.4, 41.8, 42.5, 40.9, 46.6, 45.1, 42.5)
kk = c(50.8, 46.8, 45.3, 50.0, 43.5, 59.9, 58.1, 49.5, 50.8, 53.2, 55.1, 57.1, 47.7)

# Calculate the mean of the lists
dc_mean = mean(dc)
tb_mean = mean(tb)
kk_mean = mean(kk)

# Calculate the variance of the lists
dc_var = var(dc)
tb_var = var(tb)
kk_var = var(kk)

# Calculate the standard deviation of the lists
dc_sd = sd(dc)
tb_sd = sd(tb)
kk_sd = sd(kk)

### Print first FG % info ###
print('List of Dave Corzine %:')
print(dc)
print(paste('Dave Corzine mean:', dc_mean))
print(paste('Dave Corzine variance:', dc_var))
print(paste('Dave Corzine standard deviation:', dc_sd))

jpeg("dc_scatter", width = 350, height = 350)
plot(dc)
dev.off()

jpeg("dc_boxplot", width = 350, height = 350)
boxplot(dc)
dev.off()

jpeg("dc_histogram", width = 350, height = 350)
hist(dc)
dev.off()

cat("\n")

### Print ending FG % info ###
print('List of Terrell Brandon %:')
print(tb)
print(paste('Terrell Brandon mean:', tb_mean))
print(paste('Terrell Brandon variance:', tb_var))
print(paste('Terrell Brandon standard deviation:', tb_sd))

jpeg("tb_scatter", width = 350, height = 350)
plot(tb)
dev.off()

jpeg("tb_boxplot", width = 350, height = 350)
boxplot(tb)
dev.off()

jpeg("tb_histogram", width = 350, height = 350)
hist(tb)
dev.off()

cat("\n")

### Print average FG % info ###
print('List of Kosta Koufas %:')
print(kk)
print(paste('Kosta Koufas mean:', kk_mean))
print(paste('Kosta Koufas variance:', kk_var))
print(paste('Kosta Koufas standard deviation:', kk_sd))

jpeg("kk_scatter", width = 350, height = 350)
plot(kk)
dev.off()

jpeg("kk_boxplot", width = 350, height = 350)
boxplot(kk)
dev.off()

jpeg("kk_histogram", width = 350, height = 350)
hist(kk)
dev.off()

cat("\n")
