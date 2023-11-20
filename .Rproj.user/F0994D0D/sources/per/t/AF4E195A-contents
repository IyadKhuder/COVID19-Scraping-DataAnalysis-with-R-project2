
# Imports
library(dplyr)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)

# ====================== Step 1  ====================== 
# Get a `COVID-19 pandemic` Wiki page using HTTP request
# First, let's write a function to use HTTP request to get a public COVID-19 Wiki page.
# 
# Before you write the function, you can open this public page from this URL: 
# 
#  https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country
#  using a web browser.
# 
# The goal of task 1 is to get the html page using HTTP request (`httr` library)



get_wiki_covid19_page <- function() {
  
  # Our target COVID-19 wiki page URL is: https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country  
  # Which has two parts: 
  # 1) base URL `https://en.wikipedia.org/w/index.php  
  # 2) URL Query string: `title=Template:COVID-19_testing_by_country`
  
  # Wiki page base
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  # You will need to create a List which has an element called `title` to specify which page you want to get from Wiki
  # in our case, it will be `Template:COVID-19_testing_by_country`
  query_string <- list(title = "Template:COVID-19_testing_by_country")
  # - Use the `GET` function in httr library with a `url` argument and a `query` argument to get a HTTP response
  response <- GET(wiki_base_url, query = query_string)
  # Use the `return` function to return the response
  return(response)
}

# Call the `get_wiki_covid19_page` function to get a http response with the target html page

response = get_wiki_covid19_page()

print(response)

# ====================== Step 2  ====================== 
# Extract COVID-19 testing data table (the top table on the above page) from the wiki HTML page

# Get the root html node from the http response in task 1
wiki_node <- read_html(get_wiki_covid19_page())
wiki_node

# Get the table node from the root html node
table_node <- html_nodes(wiki_node, "table")
table_node



# From the output of the above command, we notice that there are 4 tables.
# Which one is the desired table? We find out this info using the inspect feature in the browser
# We notice the class of our table starts with "wikitable plainrowheaders ...",
# which corresponds to the 2nd table in the above output
# Note that the count of the table_node index starts from 1. Therefore, index=2
# Read the table node and convert it into a data frame, and print the data frame for review
data_frame <- as.data.frame(html_table(table_node[2]))
head(data_frame)


# ====================== Step 3  ====================== 
# Pre-process and export the extracted data frame and export it as a csv file

# Print the summary of the data frame
summary(data_frame)

# As you can see from the summary, the columns names are little bit different to understand and some column data types are not correct. 
# For example, the `Tested` column identifies as `character`. 
# 
# Therefore, the data-frame extracted from HTML table needs some pre-processing
# such as removing irrelevant columns, renaming columns, and convert columns into proper data types.

preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
  # Remove the World row
  data_frame <- data_frame[!(data_frame$`Country.or.region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We don't need the Units and Ref columns, so they can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units.b."] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}

column_names <- names(data_frame)
print(column_names)


df_preprocess <- function(data_frame) {
  data_frame %>%
    filter(`Country.or.region` != "World") %>%
    slice(1:172) %>%
    select(-Ref., -Units.b.) %>%
    rename(
      country = `Country.or.region`,
      date = `Date.a.`,
      tested = `Tested`,
      confirmed = `Confirmed.cases.`,
      `confirmed_tested_ratio` = `Confirmed..tested..`,
      `tested_population_ratio` = `Tested..population..`,
      `confirmed_population_ratio` = `Confirmed..population..`
    ) %>%
    mutate(
      country = as.factor(country),
      date = as.factor(date),
      tested = as.numeric(gsub(",", "", tested)),
      confirmed = as.numeric(gsub(",", "", confirmed)),
      `confirmed_tested_ratio` = as.numeric(gsub(",", "", `confirmed_tested_ratio`)),
      `tested_population_ratio` = as.numeric(gsub(",", "", `tested_population_ratio`)),
      `confirmed_population_ratio` = as.numeric(gsub(",", "", `confirmed_population_ratio`))
    )
}

# call df_preprocess() function and assign it to a new data frame
df_preprocessed <- df_preprocess(data_frame)
head(df_preprocessed)


# Print the summary of the processed data frame again
summary(df_preprocessed)
# 
# The data frame has following columns:
#   
#   - **country** - The name of the country
# - **date** - Reported date
# - **tested** - Total tested cases by the reported date
# - **confirmed** - Total confirmed cases by the reported date
# - **confirmed.tested.ratio** - The ratio of confirmed cases to the tested cases
# - **tested.population.ratio** - The ratio of tested cases to the population of the country
# - **confirmed.population.ratio** - The ratio of confirmed cases to the population of the country



# Export the data frame into a csv file
write.csv(df_preprocessed, file='Data/COVID19_tests_by_country.csv',row.names=FALSE)


# ====================== Step 4  ====================== 

# Calculate worldwide COVID testing positive ratio

# Get the total confirmed cases worldwide
confirmed_total <- sum(df_preprocessed[,'confirmed'])

# Get the total tested cases worldwide
tested_total <- sum(df_preprocessed[,'tested'])

# Get the positive ratio (confirmed / tested)
positive_ratio <- confirmed_total/tested_total
round(positive_ratio, 2)


# ====================== Step 5  ====================== 

# Scatter plot: Tests vs Confirmed Cases
ggplot(df_preprocessed, aes(x = tested, y = confirmed)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "COVID-19 Tests vs Confirmed Cases by Country",
       x = "Number of Tests Conducted",
       y = "Number of Confirmed Cases") +
  theme_minimal()


# The output graph shows some linearity in the relation between Tests vs Confirmed Cases by Country


# ====================== Step 6  ====================== 
# Histogram: Confirmed cases to the Population Ratio
ggplot(df_preprocessed, aes(x = confirmed_population_ratio)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Confirmed Population Ratio",
       x = "Confirmed Population Ratio",
       y = "Frequency") +
  theme_minimal()


# Calculate quartiles
q1 <- quantile(df_preprocessed$confirmed_population_ratio, 0.25)
q2 <- quantile(df_preprocessed$confirmed_population_ratio, 0.50)  # Median
q3 <- quantile(df_preprocessed$confirmed_population_ratio, 0.75)

# Print the results
cat("Q1 (25th percentile):", q1, "\n")
# Output: Q1 (25th percentile): 0.425

cat("Q2 (50th percentile - Median):", q2, "\n")
# Output: Q2 (50th percentile - Median): 6.1

cat("Q3 (75th percentile):", q3, "\n")
# Output: Q3 (75th percentile): 16.25

# Bear in mind that the confirmed_population_ratio values (from the original source ) are percentages,
# That means that the true quantiles are:
# Q1: 0.425%, that is, 0.00425
# Q2: 6.1%, that is, 0.061
# Q3: 16.25%, that is, 0.01625
# Interpretation: 75% of the countries have a confirmed_population_ratio less than or equal to 0.01625. 
# indicating a relatively low prevalence of confirmed cases relative to their population.














