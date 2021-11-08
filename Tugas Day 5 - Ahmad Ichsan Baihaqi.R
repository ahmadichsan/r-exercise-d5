### Homework Day 5

### Author: Ahmad Ichsan Baihaqi
### Email: ahmadichsanbaihaqi@gmail.com

set.seed(1234)
my_data = data.frame(name = paste0(rep("M_", 10), 1:10), weight = round(rnorm(10, 20, 2), 1))
# View(my_data)
# View(my_data[order(my_data$weight),])

# just to be sure, my_data value if written down into vector, it'll be looks like this:
# name = c('M_1", "M_2", "M_3", "M_4", "M_5", "M_6", "M_7", "M_8", "M_9", "M_10")
# weight = c(17.6, 20.6, 22.2, 15.3, 20.9, 21.0, 18.9, 18.9, 18.9, 18.2)

# save data for later use
mouse_weight = my_data$weight
total_data = length(mouse_weight)

# find the mean => 19.25
m_mean = mean(mouse_weight)
print(m_mean)

# find the median (Q2) => 18.9
m_median = median(mouse_weight)
print(m_median)

# visualize using boxplot
boxplot(mouse_weight,
        main = "Weight Distribution of 10 Mouses Sample",
        xlab = "Mouse Weight (Kg)",
        horizontal = TRUE)

# Interpretation based on boxplot visualization
# 1. Based on the data, after visualized using boxplot, we can conclude that the ten samples of mouse weight is Negatively Skewed
# 2. This means that most of the mouse weight are distributed to the right
# 3. The median of this data is 18.9 and we got 19.25 for the mean, which is bigger (on the right) than the median. This founding support our statement on point 2
# 4. The line inside the box represents the median, which placed around 19 (the actual median is 18.9) on the boxplot visualization
# 5. Is there any outliers? Based on the boxplot, no. But, let's calculate it!
# Q1 value => 18.375
m_q1 = quantile(mouse_weight, 0.25)
print(m_q1)

# Q3 value => 20.825
m_q3 = quantile(mouse_weight, 0.75)
print(m_q3)

# IQR value => 2.45
m_iqr = m_q3 - m_q1
print(m_iqr)

# lol, there's already a built in function to get the IQR. But, yeah, it's okay
# IQR(mouse_weight)

# Minimum value to define if the data is outliers or not => 14.7
min_m_value = m_q1 - (1.5 * m_iqr)
print(min_m_value)

# Now, find the minimum value in our data => 15.3. This means, the minimum data in our dataset is not lower than the minimum value defined as an outliers
min_m_data = min(mouse_weight)
print(min_m_data)

# Maximum value to define if the data is outliers or not => 24.5
max_m_value = m_q3 + (1.5 * m_iqr)
print(max_m_value)

# Now, find the maximum value in our data => 22.2. This means, the maximum data in our dataset is not bigger than the maximum value defined as an outliers
max_m_data = max(mouse_weight)
print(max_m_data)

# Now, check if there are any outliers => result is FALSE
is_has_outliers = (min_m_data < min_m_value) | (max_m_data > max_m_value)
print(is_has_outliers)

# visualize using histogram
hist(mouse_weight,
    main = "Weight Distribution of 10 Mouses Sample",
    xlab = "Mouse Weight (Kg)")

# Interpretation based on histogram visualization
# 1. Based on the data, after visualized using histogram, we can conclude that the ten samples of mouse weight is following Negative Skew
# 2. This means that most of the mouse weight are distributed to the right
# 3. Also, the median of this data is 18.9 and we got 19.25 for the mean, which is bigger (on the right) than the median. This founding support our statement on point 2
# 4. In short, both boxplot and histogram visualization displayed same information. But, in boxplot, we could gather more information immediately (such as the median value, outliers) rather than using histogram

# One sample t-test
# In this case, I'd like to use 2-tailed t-test

# our mu or H0 is 25
m_mu = 25

t_test_result = t.test(mouse_weight, mu = m_mu)
print(t_test_result)
# Based on the t_test_result, we obtained several information
# 1. t value equal to -9.0783
# 2. df (degree of freedom, which n - 1, where n is total data) equal to 9
# 3. p-value = 7.953e-06
# 4. alternate hypothesis: mouse weight mean not equal to 25
# 5. 95% confidence interval between 17.8172 and 20.6828
# 6. mean of data equal to 19.25

# Based on above information, we could determined if H0 rejected based on p-value.
# H0 rejected if p-value less than 0.05. Since our p-value is 7.953e-06, it means our
# p-value is less than 0.05. So, our H0 rejected, which means that the mean of mouse weight from our data
# is not equal to 25
p_value = t_test_result$p.value
is_mu_rejected = p_value < 0.05
print(is_mu_rejected)

# Another way to determine if H0 rejected or not is by using Statistic Test and t-score
# we are using alpha 5%

# t_score => -9.078319
t_score = t_test_result$statistic
print(t_score)

# Now, let's get critical area, using table
df_table = total_data - 1

# t_table_1 => -2.262157
t_table_1 = qt(p = 0.025, df = df_table)
print(t_table_1)

# t_table_2 => 2.262157
t_table_2 = qt(p = 0.975, df = df_table)
print(t_table_2)

# Now, define if our t_score is in critical area => result is TRUE
is_t_in_critical_area = (t_score < t_table_1) | (t_score > t_table_2)
print(is_t_in_critical_area)

# Based on above calculation and comparison using t-score, we can conclude:
# 1. Our t_score value is in critical area
# 2. Based on point 1, it means our H0 is rejected. So, the mean of mouse weight from our data
# is not equal to 25

