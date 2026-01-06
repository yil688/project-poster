# title: "703 R PART"

# Required R packages ---- 
packages_needed <- c(
  "GGally", "ggplot2", "cowplot", "tidymodels", "readr", "dplyr",
  "cluster", "themis", "MASS", "VineCopula", "factoextra", "ggExtra"
)

# Install missing packages
for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# In this section, you may need to change the file path.
df_customer_csv <- read_csv("Shopping trend main table.csv") # table transformed from Power BI. Table imported from Power BI via DaxStudio.
glimpse(df_customer_csv)

df_diliver_data = read.csv("Realistic_Shipping_Delivery_Times_Dataset.csv") # Supplementary datset from Kaggle.
glimpse(df_diliver_data)


# Generating new data ----

# Clients are categorised into those from the US mainland and those in Alaska and Hawaii, 
# according to the references (see appendix in the powerBI section for details).
# And transformed the original columns by creating new data for each of them using methods that conform to normal and non-normal distributions.

# Customers with addresses in the contiguous United States (shipping time obey normal distribution)
mainland_customer <- df_customer_csv |>
  filter(!State %in% c('Alaska', 'Hawaii'))

# Create pivot table, transform long data type to wide data type. 
supply_type_time <- df_diliver_data |>
  pivot_wider(
    names_from = Shipping.Method, 
    values_from = Delivery.Time..days.
  ) |>
  unnest(cols = everything())

# The shipping type we want to generate new data.
shipping_type_time <- supply_type_time[, c("Free Shipping", "Standard")]
print(shipping_type_time)

p_values <- sapply(shipping_type_time, function(column) shapiro.test(column)$p.value)
print(p_values) 

means <- colMeans(shipping_type_time) #Calculate the mean of each column (variable)
print(means)
cov_matrix <- cov(shipping_type_time) #Calculate the covariance of each column (variable)
print(cov_matrix)

# Determine the number of data we want to generate
count_Standard <- sum(df_customer_csv$'Shipping Type' == "Standard")
print(count_Standard) # 654 rows

count_Free_Shipping <- sum(df_customer_csv$'Shipping Type' == "Free Shipping")
print(count_Free_Shipping) # 675 rows

#For each mode of transport, we already have 100 sample data and only need to generate 575 data（675-100 = 575）
set.seed(100)  #Ensure reproducibility of results
n_samples <- 575  # Number of samples we want to generate
sample <- mvrnorm(n_samples, mu = means, Sigma = cov_matrix) #multivariate normal random sampling


df_time_sample <- as.data.frame(round(sample)) #into an integer(day) and store it in the dataframe
head(df_time_sample) 

shipping_type_time$origin <- 'real'
df_time_sample$origin <- 'fake'
df_time_all <- rbind(shipping_type_time,df_time_sample) #Combining false and real data
ggpairs(df_time_all, aes(color = origin, alpha = 0.3)) 

set.seed(101) # Set seed for reproducibility
df_time_all <- subset(df_time_all[sample(nrow(df_time_all)), ], # sample() is used to break the row order of the data.
                      select = -origin)  
head(df_time_all) 
summary(df_time_all)

mainland_customer <- mainland_customer |>
  mutate(
    Shipping_Value = case_when(
      `Shipping Type` == "Express" ~ 1,
      `Shipping Type` == "2-Day Shipping" ~ 2,
      `Shipping Type` == "Store Pickup" ~ 0,
      `Shipping Type` == "Next Day Air" ~ 1,
      TRUE ~ NA_real_  
    )
  )

# Export the generated dataset of transit times for different distribution modes in the mainland United States.----
write.csv(mainland_customer, 'mainland_time_data.csv', row.names = FALSE)

## Customers with a Shipping Type of Free Shipping are filtered to replace their `Shipping Type` column.
filtered_ids <- mainland_customer[mainland_customer$`Shipping Type` == "Free Shipping", "Customer ID"]
filtered_rows <- mainland_customer[mainland_customer$`Customer ID` %in% filtered_ids$`Customer ID`, ]

# Select only the first 675 rows
replacement_values <- df_time_all$`Free Shipping`[1:nrow(filtered_rows)]

# Replaces the Shipping Type column in the filtered_rows data.
filtered_rows$`Shipping_Value` <- replacement_values

## Customers with a Shipping Type of Standard are filtered to replace their `Shipping Type` column.
# Take care to check the actual column names
filtered_ids1 <- mainland_customer[mainland_customer$`Shipping Type` == "Standard", "Customer ID"]
filtered_rows1 <- mainland_customer[mainland_customer$`Customer ID` %in% filtered_ids1$`Customer ID`, ]

# Select only the first 654 rows
replacement_values1 <- df_time_all$`Standard`[1:nrow(filtered_rows1)]

# Replaces the Shipping Type column in the filtered_rows data.
filtered_rows1$`Shipping_Value` <- replacement_values1

## Merge dataset
# Initialisation: Create a new column to store the Shipping_Value merge value
mainland_customer$Shipping_Value_Merged <- mainland_customer$Shipping_Value

# Merge filtered_rows values
mainland_customer$Shipping_Value_Merged <- ifelse(
  mainland_customer$`Customer ID` %in% filtered_rows$`Customer ID`,
  filtered_rows$Shipping_Value[match(mainland_customer$`Customer ID`, filtered_rows$`Customer ID`)],
  mainland_customer$Shipping_Value_Merged
)

# Merge the values of filtered_rows1
mainland_customer$Shipping_Value_Merged <- ifelse(
  mainland_customer$`Customer ID` %in% filtered_rows1$`Customer ID`,
  filtered_rows1$Shipping_Value[match(mainland_customer$`Customer ID`, filtered_rows1$`Customer ID`)],
  mainland_customer$Shipping_Value_Merged
)

# View Results
head(mainland_customer)

# Customers with an address in Alaska or Hawaii (skewed distribution)
other_customer <- df_customer_csv |>
  filter(State %in% c('Alaska', 'Hawaii'))

shipping_type_time2 <- supply_type_time[, c("Free Shipping", "Standard")]
print(shipping_type_time2)

round(sapply(shipping_type_time2, function(column) shapiro.test(column)$p.value),3)

shipping_type_time_scaled <- as.data.frame(lapply(shipping_type_time2,
                                            function(column) rank(column) / (length(column) + 1)))

## Fit a copula to the transformed data
copula_fit <- RVineStructureSelect(shipping_type_time_scaled, type = "RVine")

## Generate samples from the copula
set.seed(102)
sample_data <- as.data.frame(RVineSim(27, copula_fit))

## Rescaling samples generated from Copula back to the original data range
variable_sample <- mapply(function(original, sample) { 
  quantile(original, probs = sample)
},
shipping_type_time2, sample_data)

df_variable_sample <- as.data.frame(round(variable_sample))
rownames(df_variable_sample) <- NULL # Reset Row Index
head(df_variable_sample) # Show top 6 records
summary(df_variable_sample) # Quick Stats
cor(df_variable_sample) # Correlation

## Add origin column to distinguish real and fake data
shipping_type_time2$origin <- 'real'
df_variable_sample$origin <- 'fake'
df_variable_all <- rbind(shipping_type_time2, df_variable_sample)

p <- ggplot(df_variable_all,
            aes(x = 'Free Shipping', y = Standard, color = origin)) +
  geom_point(alpha = 0.3)
p
ggMarginal(p, type = "density")

set.seed(103) # Set seed for reproducibility
df_variable_all <- subset(df_variable_all[sample(nrow(df_variable_all)), ]
                          , select = -origin)

other_customer <- other_customer |>
  mutate(
    Shipping_Value = case_when(
      `Shipping Type` == "Express" ~ 1,
      `Shipping Type` == "2-Day Shipping" ~ 2,
      `Shipping Type` == "Store Pickup" ~ 0,
      `Shipping Type` == "Next Day Air" ~ 1,
      TRUE ~ NA_real_  
    )
  )

category_counts <- table(other_customer$`Shipping Type`)
print(category_counts) # Means we need to generate at least 27 new data.

## Customers with a Shipping Type of Free Shipping are filtered to replace their `Shipping Type` column.
# Take care to check the actual column names
filtered_ids_other <- other_customer[other_customer$`Shipping Type` == "Free Shipping", "Customer ID"]
filtered_rows_other <- other_customer[other_customer$`Customer ID` %in% filtered_ids_other$`Customer ID`, ]

# Select only the first 24 rows
replacement_values_other <- df_variable_all$`Free Shipping`[1:nrow(filtered_rows_other)]

# Replace Shipping_Value column in filtered_rows_other data
filtered_rows_other$`Shipping_Value` <- replacement_values_other


## Customers with a Shipping Type of standard are filtered to replace their `Shipping Type` column.
# Take care to check the actual column names
filtered_ids_other1 <- other_customer[other_customer$`Shipping Type` == "Standard", "Customer ID"]
filtered_rows_other1 <- other_customer[other_customer$`Customer ID` %in% filtered_ids_other1$`Customer ID`, ]

# Select only the first 27 rows
replacement_values_other1 <- df_variable_all$`Standard`[1:nrow(filtered_rows_other1)]

# Replaces the Shipping_Value column in the filtered_rows data.
filtered_rows_other1$`Shipping_Value` <- replacement_values_other1


#merge the dataset ----

# Initialise: create a new column Shipping_Value_Merged to allocate Merge_Value
other_customer$Shipping_Value_Merged <- other_customer$Shipping_Value

# Merge the values of filtered_rows_other
other_customer$Shipping_Value_Merged <- ifelse(
  other_customer$`Customer ID` %in% filtered_rows_other$`Customer ID`,
  filtered_rows_other$Shipping_Value[match(other_customer$`Customer ID`, filtered_rows_other$`Customer ID`)],
  other_customer$Shipping_Value_Merged
)

# Merge the values of filtered_rows_other1
other_customer$Shipping_Value_Merged <- ifelse(
  other_customer$`Customer ID` %in% filtered_rows_other1$`Customer ID`,
  filtered_rows_other1$Shipping_Value[match(other_customer$`Customer ID`, filtered_rows_other1$`Customer ID`)],
  other_customer$Shipping_Value_Merged
)

# View Results 
head(other_customer)


# Merging two datasets(mainlandcustomer & other customer)
combined_customer <- rbind(mainland_customer, other_customer)

# Sort by Customer ID in ascending order
combined_customer <- combined_customer[order(combined_customer$`Customer ID`), ]

# View Results
head(combined_customer)

# Transforming a categorical variable for clustering ----

##One-Hot code substitution for non-binary category variables
state_code <- data.frame(
  ID = 1:4,
  Region = c("South", "Northeast", "West", "Midwest")
)

# Unicode using model.matrix()
one_hot_encoded <- model.matrix(~ Region - 1, data = state_code)

# Converting solo heat coding results into dataframes
one_hot_df <- as.data.frame(one_hot_encoded)

# Merge the results of solo thermal coding into the original dataset
matrix_with_encoding <- cbind(state_code, one_hot_df)

# Splicing the results of the solo thermal encoding into a new column as a comma-separated string
matrix_with_encoding$Region_encoded <- apply(one_hot_encoded, 1, function(x) paste(x, collapse = ","))

# View Results
print(matrix_with_encoding)

## Consolidated those final dataset.
combined_customer <- merge(
  combined_customer,           
  matrix_with_encoding,        
  by = "Region",               
  all.x = TRUE  
)

# View Results
print(combined_customer)


# Standardise the names of the rows in each column and remove redundant symbols
colnames(combined_customer) <- gsub("[^[:alnum:] ]", "", colnames(combined_customer))  
# Remove non-alphanumeric and space characters
colnames(combined_customer) <- gsub(" ", "_", colnames(combined_customer))  
# Replace spaces with underscores

print(colnames(combined_customer))

# Clustering stage ----

## We will focus on 6 columns for our analysis (Include a target variable)
df_state <- combined_customer[, c(
  "ShippingValueMerged",
  "Personal_income_in_millions",
  "Purchase_Amount_USD",
  "RegionMidwest",
  "RegionNortheast",
  "RegionSouth",
  "RegionWest"
)]

glimpse(df_state)


# Normalisation of numerical variables ----
df_state_std <- as.data.frame(scale(df_state)) 

# Clustering: k-Means
set.seed(104) # Set seed for reproducibility
## Perform k-means clustering with 4 clusters (Randomly set number of groups)
kmeans_result <- kmeans(df_state_std, centers = 4)
print(kmeans_result) # View the clustering result

# Compute silhouette score 
silhouette_scores <- silhouette(kmeans_result$cluster, dist(df_state_std))
silhouette_scores
# Calculate the average silhouette score (3rd column)
avg_silhouette_score <- mean(silhouette_scores[, 3]) #All rows, third column
## Print the average silhouette score
print(avg_silhouette_score) #The clustering effect is average, next choose the appropriate K value to re-cluster

#Optimising the choice of K
fviz_nbclust(df_state_std, kmeans, method = "wss") ##choose K = 5


set.seed(105) # Set seed for reproducibility 
## Perform k-means clustering with 5 clusters
kmeans_result_best <- kmeans(df_state_std, centers = 5)
print(kmeans_result_best) 

# Generate a new column for the storage group category
df_state_std$cluster <- factor(kmeans_result_best$cluster)
df_state$cluster <- factor(kmeans_result_best$cluster)

silhouette_scores <- silhouette(kmeans_result_best$cluster, dist(df_state_std))
silhouette_scores
# Calculate the average silhouette score (3rd column)
avg_silhouette_score <- mean(silhouette_scores[, 3]) 

# Print the average silhouette score
print(avg_silhouette_score) 

# Visualising the silhouette score with 5 clusters ----
# Convert silhouette object to a data frame
silhouette_df <- data.frame(
  cluster = factor(silhouette_scores[, "cluster"]),
  silhouette_width = silhouette_scores[, "sil_width"]
)

# Calculate average silhouette width for each cluster
avg_silhouette_per_cluster <- aggregate(silhouette_width ~ cluster, data = silhouette_df, mean)

# Create the bar chart 
ggplot(avg_silhouette_per_cluster, aes(x = cluster, y = silhouette_width, fill = cluster)) +
  geom_bar(stat = "identity", color = "black", fill = "gray70") +
  geom_text(aes(label = round(silhouette_width, 2)), vjust = -0.5) +
  labs(
    title = "Average Silhouette Score per Cluster",
    x = "Cluster",
    y = "Average Silhouette Width",
    subtitle = paste("Overall Average Silhouette Score:", round(mean(silhouette_scores[, "sil_width"]), 2))
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Visualisation of cluster results (target variable'Purchase_Amount_USD' as Y-axis) ----
fits <- list() 
model <- kmeans(df_state_std, centers = 5) 
fits[[5]] <- model 

# Box plot on original values
ggplot(df_state,
       aes(x = factor(fits[[5]]$cluster), y = Purchase_Amount_USD)) +
  geom_boxplot() +
  labs(title = "Box Plot ",
       x = "Cluster",
       y = "Purchase_Amount_USD")


# Use k=5 from the list of models built already to create new column.
combined_customer$cluster <- factor(fits[[5]]$cluster) 

# Write to CSV file - remove row identifiers
write.csv(combined_customer, 'customer_cluster_R.csv', row.names = FALSE)

#| NOTE: The name of the transformed dataset from R we uploaded to Power BI for visualising clusters is '703_marketing_dataset.csv'.
#| For the purpose of this R code demo, we decided to export the file as 'customer_cluster_R.csv'. 
