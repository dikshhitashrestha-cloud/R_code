# ============================================================
# PFDA Assignment: Retail Transactional Data Analysis
# and Ratings Classification
# ============================================================
# Rojan Maharjan, NP070512
# Dikshita Shrestha, NP070501
# Matina Shrestha, NP070505
# Roshan Nepal, NP070513
# ============================================================

# ============================================================
# LOAD REQUIRED LIBRARIES
# ============================================================
# Install packages if not already installed
packages <- c("dplyr", "ggplot2", "tidyr", "lubridate", "stringr",
              "corrplot", "caret", "randomForest", "e1071",
              "scales", "RColorBrewer", "gridExtra")

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(corrplot)
library(caret)
library(randomForest)
library(e1071)
library(scales)
library(RColorBrewer)
library(gridExtra)


# ============================================================
# SECTION 1: DATA IMPORT
# ============================================================

# Set working directory to the folder containing the CSV file

retail_raw <- read.csv("E:/Dikshhita/3rd sem/PFDA/6. retail_data.csv", stringsAsFactors = FALSE)

cat("=== Dataset Dimensions ===\n")
cat("Rows:", nrow(retail_raw), "\n")
cat("Columns:", ncol(retail_raw), "\n\n")

cat("=== Column Names ===\n")
print(names(retail_raw))

cat("\n=== First 5 Rows ===\n")
print(head(retail_raw, 5))

cat("\n=== Data Types ===\n")
print(str(retail_raw))

cat("\n=== Summary Statistics ===\n")
print(summary(retail_raw))


# ============================================================
# SECTION 2: DATA CLEANING & PRE-PROCESSING
# ============================================================
cat("\n========== DATA CLEANING ==========\n")

# --- 2.1 Check for missing values ---
cat("Missing values per column:\n")
missing_counts <- colSums(is.na(retail_raw) | retail_raw == "")
print(missing_counts[missing_counts > 0])

# --- 2.2 Remove duplicate rows ---
cat("\nDuplicate rows before removal:", sum(duplicated(retail_raw)), "\n")
retail_clean <- retail_raw[!duplicated(retail_raw), ]
cat("Duplicate rows after removal:", sum(duplicated(retail_clean)), "\n")
cat("Rows remaining after deduplication:", nrow(retail_clean), "\n")

# --- 2.3 Type Conversions ---
# Convert Date column
retail_clean$Date <- as.Date(retail_clean$Date, format = "%m/%d/%Y")

# Ensure numeric columns are numeric
retail_clean$Age           <- as.integer(retail_clean$Age)
retail_clean$Total_Purchases <- as.integer(retail_clean$Total_Purchases)
retail_clean$Amount        <- as.numeric(retail_clean$Amount)
retail_clean$Total_Amount  <- as.numeric(retail_clean$Total_Amount)
retail_clean$Year          <- as.integer(retail_clean$Year)

# --- 2.4 Standardise categorical text fields ---
retail_clean$Gender           <- str_trim(str_to_title(retail_clean$Gender))
retail_clean$Income           <- str_trim(str_to_title(retail_clean$Income))
retail_clean$Customer_Segment <- str_trim(str_to_title(retail_clean$Customer_Segment))
retail_clean$Feedback         <- str_trim(str_to_title(retail_clean$Feedback))
retail_clean$Ratings          <- str_trim(str_to_title(retail_clean$Ratings))

# --- 2.5 Validate & filter logical ranges ---
cat("\nAge range before filter:", range(retail_clean$Age, na.rm = TRUE), "\n")
cat("Total_Amount range before filter:", range(retail_clean$Total_Amount, na.rm = TRUE), "\n")

retail_clean <- retail_clean %>%
  filter(!is.na(Age) & Age >= 18 & Age <= 100,
         !is.na(Total_Amount) & Total_Amount > 0,
         !is.na(Total_Purchases) & Total_Purchases > 0,
         Ratings %in% c("Low", "Medium", "High"),
         Gender %in% c("Male", "Female"),
         !is.na(Date))

cat("Rows remaining after validation:", nrow(retail_clean), "\n")

# --- 2.6 Factor encoding for categorical columns ---
# Check which Ratings values actually exist after filtering
cat("\nUnique Ratings values found in data:", unique(retail_clean$Ratings), "\n")

retail_clean$Ratings <- factor(retail_clean$Ratings,
                               levels = intersect(c("Low", "Medium", "High"),
                                                  unique(retail_clean$Ratings)),
                               ordered = TRUE)

retail_clean$Income  <- factor(retail_clean$Income,
                               levels = intersect(c("Low", "Medium", "High"),
                                                  unique(retail_clean$Income)),
                               ordered = TRUE)
retail_clean$Gender           <- factor(retail_clean$Gender)
retail_clean$Customer_Segment <- factor(retail_clean$Customer_Segment)
retail_clean$Feedback         <- factor(retail_clean$Feedback)
retail_clean$Payment_Method   <- factor(retail_clean$Payment_Method)
retail_clean$Product_Category <- factor(retail_clean$Product_Category)
retail_clean$Shipping_Method  <- factor(retail_clean$Shipping_Method)
retail_clean$Order_Status     <- factor(retail_clean$Order_Status)
retail_clean$Country          <- factor(retail_clean$Country)

# --- 2.7 Derive new useful features ---
retail_clean$Age_Group <- cut(retail_clean$Age,
                              breaks = c(17, 25, 35, 45, 55, 65, 100),
                              labels = c("18-25", "26-35", "36-45",
                                         "46-55", "56-65", "65+"))

retail_clean$Avg_Purchase_Amount <- retail_clean$Total_Amount /
  retail_clean$Total_Purchases

cat("\n=== Cleaned Dataset Summary ===\n")
print(summary(retail_clean[, c("Age", "Total_Purchases", "Total_Amount",
                                "Ratings", "Gender", "Customer_Segment")]))


# ============================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS
# ============================================================
cat("\n========== EXPLORATORY DATA ANALYSIS ==========\n")

# --- 3.1 Distribution of Ratings ---
ratings_dist <- retail_clean %>%
  count(Ratings) %>%
  mutate(Percentage = n / sum(n) * 100)

cat("Ratings Distribution:\n")
print(ratings_dist)

p_ratings_dist <- ggplot(ratings_dist, aes(x = Ratings, y = n, fill = Ratings)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Low" = "#E74C3C",
                               "Medium" = "#F39C12",
                               "High" = "#27AE60")) +
  labs(title = "Distribution of Customer Ratings",
       x = "Rating Level", y = "Number of Transactions") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(p_ratings_dist)

# --- 3.2 Gender distribution ---
p_gender <- ggplot(retail_clean, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E91E8C")) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(p_gender)

# --- 3.3 Age histogram ---
p_age <- ggplot(retail_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#8E44AD", color = "white") +
  labs(title = "Age Distribution of Customers",
       x = "Age", y = "Count") +
  theme_minimal(base_size = 13)

print(p_age)

# --- 3.4 Total Amount distribution ---
p_amount <- ggplot(retail_clean, aes(x = Total_Amount)) +
  geom_histogram(bins = 50, fill = "#16A085", color = "white") +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribution of Total Transaction Amount",
       x = "Total Amount", y = "Count") +
  theme_minimal(base_size = 13)

print(p_amount)


# ============================================================
# MEMBER 1 (Rojan Maharjan, NP070512)
# ============================================================
# Hypothesis 1a: Customer Segment and Payment Method jointly
#   influence product ratings, with premium segments using
#   digital payment methods giving higher ratings.
# Independent Variables : Customer_Segment, Payment_Method
# Dependent Variable    : Ratings
# Objective 1a: To examine whether customer segment and payment
#   method together have a statistically significant association
#   with product rating level, identifying which combinations
#   drive the highest customer satisfaction.
#   --> Rojan Maharjan, NP070512
#
# Hypothesis 1b: Shipping method and country of purchase
#   significantly affect total transaction amount, with
#   same-day delivery in high-income countries generating
#   the highest spend per transaction.
# Independent Variables : Shipping_Method, Country
# Dependent Variable    : Total_Amount
# Objective 1b: To determine whether shipping method and
#   country (independent variables) significantly influence
#   total transaction amount (dependent variable) using
#   non-parametric group comparison and visualisation.
#   --> Rojan Maharjan, NP070512
# ============================================================
cat("\n========== HYPOTHESIS 1a: Customer Segment + Payment Method vs Ratings ==========\n")

# Analysis 1a-1: Ratings distribution by Customer Segment
cat("Analysis 1a-1: Cross-tabulation – Customer Segment vs Ratings\n")
ct1 <- table(retail_clean$Customer_Segment, retail_clean$Ratings)
print(ct1)
cat("\nProportions (row-wise):\n")
print(round(prop.table(ct1, margin = 1) * 100, 2))

# Analysis 1a-2: Chi-square test – Customer Segment vs Ratings
cat("\nAnalysis 1a-2: Chi-Square Test – Customer Segment vs Ratings\n")
ct1_clean <- ct1[rowSums(ct1) > 0, colSums(ct1) > 0, drop = FALSE]
chi_test1 <- tryCatch(
  chisq.test(ct1_clean, simulate.p.value = TRUE, B = 2000),
  error = function(e) chisq.test(ct1_clean)
)
print(chi_test1)
if (!is.na(chi_test1$p.value) && chi_test1$p.value < 0.05) {
  cat("Result: Significant association between Customer Segment and Ratings (p <0.05)\n")
} else {
  cat("Result: No significant association found (p >=0.05)\n")
}

# Visualisation 1a-i: Stacked bar – Segment vs Ratings
seg_rating <- retail_clean %>%
  count(Customer_Segment, Ratings) %>%
  group_by(Customer_Segment) %>%
  mutate(Pct = n / sum(n) * 100)

p_obj1a <- ggplot(seg_rating, aes(x = Customer_Segment, y = Pct, fill = Ratings)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_manual(values = c("Low" = "#E74C3C",
                               "Medium" = "#F39C12",
                               "High" = "#27AE60")) +
  labs(title = "Ratings Distribution by Customer Segment",
       x = "Customer Segment", y = "Percentage (%)", fill = "Rating") +
  theme_minimal(base_size = 13)
print(p_obj1a)

# Analysis 1a-3: Payment Method vs Ratings
cat("\nAnalysis 1a-3: Cross-tabulation – Payment Method vs Ratings\n")
ct1b <- table(retail_clean$Payment_Method, retail_clean$Ratings)
print(ct1b)
print(round(prop.table(ct1b, margin = 1) * 100, 2))

# Chi-square: Payment Method vs Ratings
ct1b_clean <- ct1b[rowSums(ct1b) > 0, colSums(ct1b) > 0, drop = FALSE]
chi_test1b <- tryCatch(
  chisq.test(ct1b_clean, simulate.p.value = TRUE, B = 2000),
  error = function(e) chisq.test(ct1b_clean)
)
cat("\nChi-Square Test – Payment Method vs Ratings:\n")
print(chi_test1b)
if (!is.na(chi_test1b$p.value) && chi_test1b$p.value < 0.05) {
  cat("Result: Significant association between Payment Method and Ratings (p <0.05)\n")
} else {
  cat("Result: No significant association found (p >=0.05)\n")
}

# Visualisation 1a-ii: Payment Method vs Ratings heatmap
pay_rating <- retail_clean %>%
  count(Payment_Method, Ratings) %>%
  group_by(Payment_Method) %>%
  mutate(Pct = n / sum(n) * 100)

p_obj1b <- ggplot(pay_rating, aes(x = Ratings, y = Payment_Method, fill = Pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(Pct, 1), "%")), size = 3.8) +
  scale_fill_gradient(low = "#FDFEFE", high = "#2980B9",
                      name = "% within\nPayment") +
  labs(title = "Ratings Distribution by Payment Method (Heatmap)",
       x = "Rating", y = "Payment Method") +
  theme_minimal(base_size = 13)
print(p_obj1b)

# Analysis 1a-4: Combined – Segment + Payment vs High Rating rate
cat("\nAnalysis 1a-4: High Rating Rate by Segment and Payment Method\n")
seg_pay_rating <- retail_clean %>%
  group_by(Customer_Segment, Payment_Method) %>%
  summarise(High_Rate = mean(Ratings == "High", na.rm = TRUE) * 100,
            Count = n(), .groups = "drop") %>%
  arrange(desc(High_Rate))
print(head(seg_pay_rating, 10))


# ============================================================
# HYPOTHESIS 1b: Shipping Method + Country vs Total Amount
# ============================================================
cat("\n========== HYPOTHESIS 1b: Shipping Method + Country vs Total Amount ==========\n")

# Analysis 1b-1: Average Total Amount by Shipping Method
cat("Analysis 1b-1: Average Total Amount by Shipping Method\n")
ship_amount <- retail_clean %>%
  group_by(Shipping_Method) %>%
  summarise(Avg_Amount    = mean(Total_Amount, na.rm = TRUE),
            Median_Amount = median(Total_Amount, na.rm = TRUE),
            Count         = n(), .groups = "drop") %>%
  arrange(desc(Avg_Amount))
print(ship_amount)

p_obj1c <- ggplot(retail_clean,
                  aes(x = reorder(Shipping_Method, Total_Amount, FUN = median),
                      y = Total_Amount, fill = Shipping_Method)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Amount Distribution by Shipping Method",
       x = "Shipping Method", y = "Total Amount") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_obj1c)

# Analysis 1b-2: Kruskal-Wallis – Shipping Method vs Total Amount
cat("\nAnalysis 1b-2: Kruskal-Wallis – Shipping Method vs Total Amount\n")
kw_ship <- kruskal.test(Total_Amount ~ Shipping_Method, data = retail_clean)
print(kw_ship)
if (!is.na(kw_ship$p.value) && kw_ship$p.value < 0.05) {
  cat("Result: Significant difference in Total Amount across Shipping Methods (p <0.05)\n")
} else {
  cat("Result: No significant difference found (p >=0.05)\n")
}

# Analysis 1b-3: Kruskal-Wallis – Country vs Total Amount
cat("\nAnalysis 1b-3: Kruskal-Wallis – Country vs Total Amount\n")
kw_country <- kruskal.test(Total_Amount ~ Country, data = retail_clean)
print(kw_country)
if (!is.na(kw_country$p.value) && kw_country$p.value < 0.05) {
  cat("Result: Significant difference in Total Amount across Countries (p <0.05)\n")
} else {
  cat("Result: No significant difference found (p >=0.05)\n")
}

# Visualisation 1b: Average Total Amount by Country
country_amount <- retail_clean %>%
  group_by(Country) %>%
  summarise(Avg_Amount = mean(Total_Amount, na.rm = TRUE),
            SE = sd(Total_Amount, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

p_obj1d <- ggplot(country_amount,
                  aes(x = reorder(Country, Avg_Amount),
                      y = Avg_Amount, fill = Country)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Avg_Amount - 1.96 * SE,
                    ymax = Avg_Amount + 1.96 * SE),
                width = 0.3) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Average Total Amount by Country with 95% CI",
       x = "Country", y = "Average Total Amount") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_obj1d)


# ============================================================
# MEMBER 2 (Dikshhita Shrestha, NP070501)
# ============================================================
# Hypothesis 2a: Customers with higher income levels spend
#   significantly more per transaction and are more likely
#   to assign a High product rating than low-income customers.
# Independent Variables : Income
# Dependent Variable    : Total_Amount, Ratings
# Objective 2a: To measure the effect of income level on total
#   transaction amount and product rating using non-parametric
#   statistical testing and visual group comparison.
#  --> Dikshita Shrestha, NP070501
#
# Hypothesis 2b: Customers who make more total purchases
#   generate significantly higher total transaction amounts,
#   indicating a positive relationship between purchase
#   frequency and overall spend.
# Independent Variables : Total_Purchases
# Dependent Variable    : Total_Amount
# Objective 2b: To analyse the relationship between number of
#   purchases (independent variable) and total transaction
#   amount (dependent variable) using correlation and
#   regression analysis.
#  --> Dikshhita Shrestha NP070501
# ============================================================
cat("\n========== HYPOTHESIS 2a: Income Level vs Total Amount + Ratings ==========\n")

# Analysis 2a-1: Average spending by Income
cat("Analysis 2a-1: Average Spending by Income Level\n")
income_spending <- retail_clean %>%
  group_by(Income) %>%
  summarise(Avg_Amount    = mean(Total_Amount, na.rm = TRUE),
            Median_Amount = median(Total_Amount, na.rm = TRUE),
            Count         = n(), .groups = "drop")
print(income_spending)

p_obj2a <- ggplot(retail_clean, aes(x = Income, y = Total_Amount, fill = Income)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_fill_manual(values = c("Low" = "#AED6F1",
                               "Medium" = "#3498DB",
                               "High" = "#1A5276")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Amount Spent by Income Level",
       x = "Income Level", y = "Total Amount") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_obj2a)

# Analysis 2a-2: Kruskal-Wallis – Income vs Total Amount
cat("\nAnalysis 2a-2: Kruskal-Wallis Test – Income vs Total Amount\n")
kw_test2 <- kruskal.test(Total_Amount ~ Income, data = retail_clean)
print(kw_test2)
if (!is.na(kw_test2$p.value) && kw_test2$p.value < 0.05) {
  cat("Result: Significant difference in spending across income levels (p <0.05)\n")
} else {
  cat("Result: No significant difference found (p >=0.05)\n")
}

# Analysis 2a-3: Ratings distribution by Income
cat("\nAnalysis 2a-3: Ratings Distribution by Income Level\n")
ct2 <- table(retail_clean$Income, retail_clean$Ratings)
print(ct2)

p_obj2b_plot <- ggplot(retail_clean %>%
                    count(Income, Ratings) %>%
                    group_by(Income) %>%
                    mutate(Pct = n / sum(n) * 100),
                  aes(x = Income, y = Pct, fill = Ratings)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Low" = "#E74C3C",
                               "Medium" = "#F39C12",
                               "High" = "#27AE60")) +
  labs(title = "Ratings Distribution by Income Level",
       x = "Income Level", y = "Percentage (%)", fill = "Rating") +
  theme_minimal(base_size = 13)
print(p_obj2b_plot)


# ============================================================
# HYPOTHESIS 2b: Total Purchases vs Total Amount
# ============================================================
cat("\n========== HYPOTHESIS 2b: Total Purchases vs Total Amount ==========\n")

# Analysis 2b-1: Scatter plot – Total Purchases vs Total Amount
cat("Analysis 2b-1: Scatter Plot – Total Purchases vs Total Amount\n")
p_obj2c <- ggplot(retail_clean,
                  aes(x = Total_Purchases, y = Total_Amount)) +
  geom_point(alpha = 0.15, color = "#2980B9", size = 0.8) +
  geom_smooth(method = "lm", color = "#E74C3C", se = TRUE) +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Purchases vs Total Amount (with Linear Trend)",
       x = "Total Purchases", y = "Total Amount") +
  theme_minimal(base_size = 13)
print(p_obj2c)

# Analysis 2b-2: Pearson Correlation
cat("\nAnalysis 2b-2: Pearson Correlation – Total Purchases vs Total Amount\n")
cor_test2b <- cor.test(retail_clean$Total_Purchases,
                       retail_clean$Total_Amount,
                       method = "pearson")
print(cor_test2b)
cat(sprintf("Correlation coefficient: %.4f\n", cor_test2b$estimate))
if (!is.na(cor_test2b$p.value) && cor_test2b$p.value < 0.05) {
  cat("Result: Significant positive correlation between purchases and amount (p <0.05)\n")
} else {
  cat("Result: No significant correlation found (p >=0.05)\n")
}

# Analysis 2b-3: Simple Linear Regression
cat("\nAnalysis 2b-3: Simple Linear Regression – Purchases predicting Amount\n")
lm_model2b <- lm(Total_Amount ~ Total_Purchases, data = retail_clean)
print(summary(lm_model2b))

p_obj2d <- ggplot(retail_clean %>%
                    group_by(Total_Purchases) %>%
                    summarise(Avg_Amount = mean(Total_Amount, na.rm = TRUE),
                              .groups = "drop"),
                  aes(x = Total_Purchases, y = Avg_Amount)) +
  geom_bar(stat = "identity", fill = "#1A5276", alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  labs(title = "Average Total Amount by Number of Purchases",
       x = "Total Purchases", y = "Average Total Amount") +
  theme_minimal(base_size = 13)
print(p_obj2d)


# ============================================================
# MEMBER 3 (Matina Shrestha, NP070505)
# ============================================================
# Hypothesis 3a: Customers who leave positive feedback
#   (Excellent or Good) are significantly more likely to
#   assign a High product rating than those with negative
#   feedback, indicating feedback as a key satisfaction signal.
# Independent Variables : Feedback
# Dependent Variable    : Ratings
# Objective 3a: To determine whether feedback type can predict
#   rating level and quantify the strength of association
#   between feedback category and rating outcome.
#   --> Matina Shrestha, NP070505
#
# Hypothesis 3b: Product category significantly affects the
#   total transaction amount, with premium categories such
#   as Electronics generating higher average spend per
#   transaction compared to other categories.
# Independent Variables : Product_Category
# Dependent Variable    : Total_Amount
# Objective 3b: To compare total transaction amounts across
#   product categories using non-parametric testing, and
#   identify which categories contribute most to revenue.
#  --> Matina Shrestha, NP070505
# ============================================================
cat("\n========== HYPOTHESIS 3a: Feedback vs Ratings ==========\n")

# Analysis 3a-1: Cross-tabulation – Feedback vs Ratings
cat("Analysis 3a-1: Feedback vs Ratings Cross-tabulation\n")
ct3 <- table(retail_clean$Feedback, retail_clean$Ratings)
print(ct3)
print(round(prop.table(ct3, margin = 1) * 100, 2))

# Analysis 3a-2: Chi-square test
cat("\nAnalysis 3a-2: Chi-Square Test – Feedback vs Ratings\n")
ct3_clean <- ct3[rowSums(ct3) > 0, colSums(ct3) > 0, drop = FALSE]
chi_test3 <- tryCatch(
  chisq.test(ct3_clean, simulate.p.value = TRUE, B = 2000),
  error = function(e) chisq.test(ct3_clean)
)
print(chi_test3)
if (!is.na(chi_test3$p.value) && chi_test3$p.value < 0.05) {
  cat("Result: Significant association between Feedback and Ratings (p <0.05)\n")
} else {
  cat("Result: No significant association found (p >=0.05)\n")
}

# Visualisation 3a: Heatmap – Feedback vs Ratings
feedback_rating <- retail_clean %>%
  count(Feedback, Ratings) %>%
  group_by(Feedback) %>%
  mutate(Pct = n / sum(n) * 100)

p_obj3a <- ggplot(feedback_rating, aes(x = Ratings, y = Feedback, fill = Pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(Pct, 1), "%")), size = 4) +
  scale_fill_gradient(low = "#FDFEFE", high = "#2ECC71") +
  labs(title = "Feedback vs Ratings Heatmap",
       x = "Rating", y = "Feedback Type",
       fill = "% within Feedback") +
  theme_minimal(base_size = 13)
print(p_obj3a)

# Analysis 3a-3: Average Amount by Feedback type
cat("\nAnalysis 3a-3: Average Spending per Feedback Type\n")
feedback_amount <- retail_clean %>%
  group_by(Feedback) %>%
  summarise(Avg_Amount = mean(Total_Amount, na.rm = TRUE),
            Count = n(), .groups = "drop") %>%
  arrange(desc(Avg_Amount))
print(feedback_amount)

p_obj3b_plot <- ggplot(feedback_amount,
                       aes(x = reorder(Feedback, Avg_Amount),
                           y = Avg_Amount, fill = Feedback)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Average Transaction Amount by Feedback Type",
       x = "Feedback", y = "Average Total Amount") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_obj3b_plot)


# ============================================================
# HYPOTHESIS 3b: Product Category vs Total Amount
# ============================================================
cat("\n========== HYPOTHESIS 3b: Product Category vs Total Amount ==========\n")

# Analysis 3b-1: Revenue and avg amount per Product Category
cat("Analysis 3b-1: Total Revenue and Average Amount by Product Category\n")
cat_revenue <- retail_clean %>%
  group_by(Product_Category) %>%
  summarise(Total_Revenue = sum(Total_Amount, na.rm = TRUE),
            Avg_Amount    = mean(Total_Amount, na.rm = TRUE),
            Count         = n(), .groups = "drop") %>%
  arrange(desc(Total_Revenue))
print(cat_revenue)

p_obj3c <- ggplot(cat_revenue,
                  aes(x = reorder(Product_Category, Avg_Amount),
                      y = Avg_Amount, fill = Total_Revenue)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276",
                      name = "Total\nRevenue") +
  labs(title = "Average Transaction Amount by Product Category",
       x = "Product Category", y = "Average Total Amount") +
  theme_minimal(base_size = 13)
print(p_obj3c)

# Analysis 3b-2: Kruskal-Wallis – Product Category vs Total Amount
cat("\nAnalysis 3b-2: Kruskal-Wallis – Product Category vs Total Amount\n")
kw_cat <- kruskal.test(Total_Amount ~ Product_Category, data = retail_clean)
print(kw_cat)
if (!is.na(kw_cat$p.value) && kw_cat$p.value < 0.05) {
  cat("Result: Significant difference in Total Amount across Product Categories (p <0.05)\n")
} else {
  cat("Result: No significant difference found (p >=0.05)\n")
}

# Analysis 3b-3: Box plot – Product Category vs Total Amount
p_obj3d <- ggplot(retail_clean,
                  aes(x = reorder(Product_Category, Total_Amount, FUN = median),
                      y = Total_Amount, fill = Product_Category)) +
  geom_boxplot(outlier.alpha = 0.15) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Amount Distribution by Product Category",
       x = "Product Category", y = "Total Amount") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_obj3d)


# ============================================================
# MEMBER 4 (Roshan Nepal, NP070513)
# ============================================================
# Hypothesis 4a: Younger customers (aged 18-35) make more
#   frequent purchases and are more likely to give High ratings
#   compared to older age groups, with rating patterns
#   differing significantly between male and female customers.
# Independent Variables : Age_Group, Gender
# Dependent Variable    : Ratings
# Objective 4a: To analyse how age group and gender influence
#   the distribution of product ratings, and identify key
#   demographic segments that drive customer satisfaction.
#   --> Roshan Nepal, NP070513
#
# Hypothesis 4b: Order fulfilment status significantly affects
#   the number of total purchases, with customers whose orders
#   are delivered completing more purchases than those with
#   cancelled or pending orders.
# Independent Variables : Order_Status
# Dependent Variable    : Total_Purchases
# Objective 4b: To determine whether order status has a
#   statistically significant effect on total number of
#   purchases using non-parametric testing and descriptive
#   comparison across order status groups.
#   --> Roshan Nepal, NP070513
# ============================================================
cat("\n========== HYPOTHESIS 4a: Age Group + Gender vs Ratings ==========\n")

# Analysis 4a-1: Ratings by Age Group
cat("Analysis 4a-1: Ratings Distribution by Age Group\n")
age_rating <- retail_clean %>%
  count(Age_Group, Ratings) %>%
  group_by(Age_Group) %>%
  mutate(Pct = n / sum(n) * 100)
print(age_rating)

p_obj4a <- ggplot(age_rating, aes(x = Age_Group, y = Pct, fill = Ratings)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Low" = "#E74C3C",
                               "Medium" = "#F39C12",
                               "High" = "#27AE60")) +
  labs(title = "Ratings Distribution by Age Group",
       x = "Age Group", y = "Percentage (%)", fill = "Rating") +
  theme_minimal(base_size = 13)
print(p_obj4a)

# Analysis 4a-2: Ratings by Gender
cat("\nAnalysis 4a-2: Ratings Distribution by Gender\n")
gender_rating <- retail_clean %>%
  count(Gender, Ratings) %>%
  group_by(Gender) %>%
  mutate(Pct = n / sum(n) * 100)
print(gender_rating)

p_obj4b <- ggplot(gender_rating,
                  aes(x = Gender, y = Pct, fill = Ratings)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Low" = "#E74C3C",
                               "Medium" = "#F39C12",
                               "High" = "#27AE60")) +
  labs(title = "Ratings Distribution by Gender",
       x = "Gender", y = "Percentage (%)", fill = "Rating") +
  theme_minimal(base_size = 13)
print(p_obj4b)

# Analysis 4a-3: Chi-square – Age Group vs Ratings
cat("\nAnalysis 4a-3: Chi-Square Test – Age Group vs Ratings\n")
ct4a <- table(retail_clean$Age_Group, retail_clean$Ratings)
ct4a_clean <- ct4a[rowSums(ct4a) > 0, colSums(ct4a) > 0, drop = FALSE]
chi_test4a <- tryCatch(
  chisq.test(ct4a_clean, simulate.p.value = TRUE, B = 2000),
  error = function(e) chisq.test(ct4a_clean)
)
print(chi_test4a)
if (!is.na(chi_test4a$p.value) && chi_test4a$p.value < 0.05) {
  cat("Result: Significant association between Age Group and Ratings (p <0.05)\n")
} else {
  cat("Result: No significant association found (p >=0.05)\n")
}

# Analysis 4a-4: Average Purchases by Age Group and Gender
cat("\nAnalysis 4a-4: Average Purchases by Age Group and Gender\n")
purchases_demo <- retail_clean %>%
  group_by(Age_Group, Gender) %>%
  summarise(Avg_Purchases = mean(Total_Purchases, na.rm = TRUE),
            Avg_Amount    = mean(Total_Amount, na.rm = TRUE),
            .groups = "drop")
print(purchases_demo)

p_obj4c <- ggplot(purchases_demo,
                  aes(x = Age_Group, y = Avg_Purchases, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Male" = "#3498DB", "Female" = "#E91E8C")) +
  labs(title = "Average Purchases by Age Group and Gender",
       x = "Age Group", y = "Average Purchases", fill = "Gender") +
  theme_minimal(base_size = 13)
print(p_obj4c)


# ============================================================
# HYPOTHESIS 4b: Order Status vs Total Purchases
# ============================================================
cat("\n========== HYPOTHESIS 4b: Order Status vs Total Purchases ==========\n")

# Analysis 4b-1: Average Total Purchases by Order Status
cat("Analysis 4b-1: Average Total Purchases by Order Status\n")
order_purchases <- retail_clean %>%
  group_by(Order_Status) %>%
  summarise(Avg_Purchases    = mean(Total_Purchases, na.rm = TRUE),
            Median_Purchases = median(Total_Purchases, na.rm = TRUE),
            Count            = n(), .groups = "drop") %>%
  arrange(desc(Avg_Purchases))
print(order_purchases)

p_obj4d <- ggplot(order_purchases,
                  aes(x = reorder(Order_Status, Avg_Purchases),
                      y = Avg_Purchases, fill = Order_Status)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Avg_Purchases, 2)),
            hjust = -0.1, size = 3.8) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Average Total Purchases by Order Status",
       x = "Order Status", y = "Average Total Purchases") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_obj4d)

# Analysis 4b-2: Kruskal-Wallis – Order Status vs Total Purchases
cat("\nAnalysis 4b-2: Kruskal-Wallis – Order Status vs Total Purchases\n")
kw_order <- kruskal.test(Total_Purchases ~ Order_Status, data = retail_clean)
print(kw_order)
if (!is.na(kw_order$p.value) && kw_order$p.value < 0.05) {
  cat("Result: Significant difference in purchases across Order Status groups (p <0.05)\n")
} else {
  cat("Result: No significant difference found (p >=0.05)\n")
}

# Analysis 4b-3: Box plot – Order Status vs Total Purchases
p_obj4e <- ggplot(retail_clean,
                  aes(x = reorder(Order_Status, Total_Purchases, FUN = median),
                      y = Total_Purchases, fill = Order_Status)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_flip() +
  labs(title = "Total Purchases Distribution by Order Status",
       x = "Order Status", y = "Total Purchases") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_obj4e)


# ============================================================
# Extra feature 1
# Random Forest classification model to predict customer Ratings
# from all available variables. This improves analysis by
# quantifying which variables most influence ratings, enabling
# data-driven prioritisation for stakeholders. It goes beyond
# descriptive statistics by providing a predictive capability
# with measurable accuracy (confusion matrix + overall accuracy).
# ============================================================
cat("\n========== EXTRA FEATURE 1: Random Forest Classification ==========\n")

# Prepare modelling dataset
model_data <- retail_clean %>%
  select(Ratings, Age, Gender, Income, Customer_Segment,
         Total_Purchases, Total_Amount, Avg_Purchase_Amount,
         Feedback, Payment_Method, Product_Category,
         Shipping_Method) %>%
  na.omit()

# Drop any empty factor levels that survived filtering
model_data <- droplevels(model_data)

cat("Ratings levels in model data:", levels(model_data$Ratings), "\n")
cat("Class counts:\n")
print(table(model_data$Ratings))

set.seed(42)

# Train / Test split (70:30)
train_idx <- createDataPartition(model_data$Ratings, p = 0.70, list = FALSE)
train_set <- model_data[train_idx, ]
test_set  <- model_data[-train_idx, ]

cat("Training set size:", nrow(train_set), "\n")
cat("Test set size:    ", nrow(test_set), "\n")

cat("\nTraining Random Forest model ...\n")
rf_model <- randomForest(Ratings ~ ., data = train_set,
                         ntree = 100, importance = TRUE)
print(rf_model)

rf_pred  <- predict(rf_model, newdata = test_set)
conf_mat <- confusionMatrix(rf_pred, test_set$Ratings)
print(conf_mat)
cat(sprintf("\nOverall Accuracy: %.2f%%\n", conf_mat$overall["Accuracy"] * 100))

var_imp    <- importance(rf_model)
var_imp_df <- data.frame(Variable         = rownames(var_imp),
                         MeanDecreaseGini = var_imp[, "MeanDecreaseGini"]) %>%
  arrange(desc(MeanDecreaseGini))

p_varimp <- ggplot(var_imp_df,
                   aes(x = reorder(Variable, MeanDecreaseGini),
                       y = MeanDecreaseGini, fill = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276") +
  labs(title = "Variable Importance – Random Forest",
       x = "Variable", y = "Mean Decrease in Gini Index") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_varimp)

cat("\nTop 5 most important variables for predicting Ratings:\n")
print(head(var_imp_df, 5))


# ============================================================
# Extra feature 2
# Numeric correlation matrix using corrplot to identify linear
# relationships between quantitative variables. This improves
# retrieval by showing at a glance which numeric factors move
# together with spending and ratings, guiding which variables
# to include in further predictive models.
# ============================================================
cat("\n========== EXTRA FEATURE 2: Correlation Analysis ==========\n")

retail_corr <- retail_clean %>%
  mutate(Ratings_Num = as.integer(Ratings),
         Income_Num  = as.integer(Income))

corr_vars <- retail_corr %>%
  select(Age, Total_Purchases, Amount, Total_Amount,
         Avg_Purchase_Amount, Ratings_Num, Income_Num) %>%
  na.omit()

corr_matrix <- cor(corr_vars)
cat("Correlation Matrix:\n")
print(round(corr_matrix, 3))

# Widen margins and reduce font sizes to prevent label overlap
par(mar = c(0, 0, 3, 0))
corrplot(corr_matrix,
         method        = "color",
         type          = "upper",
         tl.cex        = 0.8,     # variable name size
         tl.col        = "black",
         tl.srt        = 45,      # rotate x-axis labels 45 degrees
         addCoef.col   = "black",
         number.cex    = 0.65,    # smaller coefficient numbers
         number.digits = 2,       # 2 decimal places keeps numbers short
         cl.cex        = 0.8,     # colour legend text size
         title         = "Correlation Matrix of Numeric Variables",
         mar           = c(0, 0, 3, 0))


# ============================================================
# Extra feature 3
# Monthly time-series trend analysis of total sales and average
# rating score with LOESS smoothing. This improves results by
# revealing seasonal purchasing patterns and rating fluctuations
# that static cross-sectional analysis cannot detect, supporting
# inventory planning and targeted marketing decisions.
# ============================================================
cat("\n========== EXTRA FEATURE 3: Monthly Sales Trend ==========\n")

monthly_trend <- retail_clean %>%
  group_by(Year, Month) %>%
  summarise(Total_Sales = sum(Total_Amount, na.rm = TRUE),
            Avg_Rating  = mean(as.integer(Ratings), na.rm = TRUE),
            Count       = n(), .groups = "drop") %>%
  mutate(Month_Num  = match(Month,
                            c("January","February","March","April","May","June",
                              "July","August","September","October","November",
                              "December")),
         Date_Label = as.Date(paste(Year, Month_Num, "01"), "%Y %m %d")) %>%
  arrange(Date_Label)

p_trend <- ggplot(monthly_trend, aes(x = Date_Label, y = Total_Sales)) +
  geom_line(color = "#2E86C1", linewidth = 1) +
  geom_point(color = "#1A5276", size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(title = "Monthly Total Sales Trend",
       x = "Month", y = "Total Sales Amount") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_trend)

p_rating_trend <- ggplot(monthly_trend, aes(x = Date_Label, y = Avg_Rating)) +
  geom_line(color = "#27AE60", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE,
              color = "#1D8348", fill = "#A9DFBF") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(title = "Average Rating Trend Over Time",
       x = "Month", y = "Average Rating (1=Low, 2=Med, 3=High)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_rating_trend)


# ============================================================
# SUMMARY FINDINGS
# ============================================================
cat("\n========== SUMMARY FINDINGS ==========\n")

cat("Total transactions analysed:", nrow(retail_clean), "\n")

cat("\nOverall Ratings distribution:\n")
print(table(retail_clean$Ratings))

cat("\nTop 5 most important predictors of Ratings (Random Forest):\n")
print(head(var_imp_df$Variable, 5))

cat("\nHypothesis 1a – Customer Segment vs Ratings",
    "(Chi-square p =", round(chi_test1$p.value, 4), ")\n")
cat("Hypothesis 1a – Payment Method vs Ratings",
    "(Chi-square p =", round(chi_test1b$p.value, 4), ")\n")
cat("Hypothesis 1b – Shipping Method vs Total Amount",
    "(Kruskal-Wallis p =", round(kw_ship$p.value, 4), ")\n")
cat("Hypothesis 1b – Country vs Total Amount",
    "(Kruskal-Wallis p =", round(kw_country$p.value, 4), ")\n")
cat("Hypothesis 2a – Income vs Total Amount",
    "(Kruskal-Wallis p =", round(kw_test2$p.value, 4), ")\n")
cat("Hypothesis 2b – Purchases vs Amount",
    "(Pearson r =", round(cor_test2b$estimate, 4), ")\n")
cat("Hypothesis 3a – Feedback vs Ratings",
    "(Chi-square p =", round(chi_test3$p.value, 4), ")\n")
cat("Hypothesis 3b – Product Category vs Total Amount",
    "(Kruskal-Wallis p =", round(kw_cat$p.value, 4), ")\n")
cat("Hypothesis 4a – Age Group vs Ratings",
    "(Chi-square p =", round(chi_test4a$p.value, 4), ")\n")
cat("Hypothesis 4b – Order Status vs Total Purchases",
    "(Kruskal-Wallis p =", round(kw_order$p.value, 4), ")\n")

cat("\n========== ANALYSIS COMPLETE ==========\n")
