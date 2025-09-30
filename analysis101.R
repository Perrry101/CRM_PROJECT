library(DBI)
library(ggplot2)
library(dplyr)
library(scales)
library(RMariaDB)
library(lubridate)
library(openxlsx)
library(gridExtra)
library(ggplot2)
library(plotly)
library(treemap)
library(RColorBrewer)
library(tidyr)
dbt <- dbConnect(
  RMariaDB::MariaDB(),
  host ="localhost",
  dbname="online_retail",
  user="root",
  password="root",
  port="3306"
)

sales <- dbGetQuery(dbt , "select * from clean_sales")
dbGetQuery(dbt, "desc clean_sales")
colnames(sales)

sales <- dbGetQuery(dbt, "
  SELECT *,
         STR_TO_DATE(InvoiceDate, '%d/%m/%Y %H:%i') as CleanInvoiceDate
  FROM clean_sales
  WHERE InvoiceDate IS NOT NULL AND InvoiceDate != ''
")
View(sales)
str(sales)

  sales <-sales %>% 
    mutate(InvoiceDate = as.POSIXct(CleanInvoiceDate, format = "%Y-%m-%d %H:%i:%s"),
           YearMonth   = floor_date(InvoiceDate, "month")) 
  head(sales)
  
  str(sales $CleanInvoiceDate)
  
  sales %>% filter(is.na(InvoiceDate)) %>% head(20)
  
############################# SALES BY MONTH ############################################
  #### HANDELING THE MISSING VALUES##############################
  cat("Missing InvoiceDate values:", sum(is.na(sales$InvoiceDate)), "\n")
  
  monthly <- sales %>%
    filter(!is.na(InvoiceDate), !is.na(Revenue)) %>%  # Removing the missing values
    mutate(YearMonth = floor_date(InvoiceDate, "month")) %>%  # Ensuring proper monthly grouping
    group_by(YearMonth) %>%
    summarise(
      Revenue = sum(Revenue, na.rm = TRUE),
      TransactionCount = n(),
      AvgRevenue = mean(Revenue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(YearMonth) %>%
   
    complete(YearMonth = seq.Date(min(YearMonth), max(YearMonth), by = "month"),
             fill = list(Revenue = 0, TransactionCount = 0, AvgRevenue = 0))
  
  ########### Summarising the  outcomes#####################
  cat("\nTime series summary:\n")
  cat("Date range:", format(min(monthly$YearMonth), "%Y-%m"), "to", 
      format(max(monthly$YearMonth), "%Y-%m"), "\n")
  cat("Total months:", nrow(monthly), "\n")
  cat("Mean monthly revenue:", scales::dollar(mean(monthly$Revenue)), "\n")
  

  monthly <- monthly %>%
    mutate(YearMonth = as.Date(paste0(YearMonth, "-01"))) 
  
  
 
  plot_1<- ggplot(monthly, aes(x = YearMonth, y = Revenue)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "darkred", size = 2) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    scale_y_continuous(labels = scales::label_comma(prefix = "$")) +
    labs(
      title = "Monthly Revenue Over Time Online Retail",
      subtitle = paste0("Time Series Analysis (", 
                        format(min(monthly$YearMonth), "%b %Y"), " - ", 
                        format(max(monthly$YearMonth), "%b %Y"), ")"),
      x = "Month",
      y = "Revenue (USD)",
      caption = "Note: Trend line shows LOESS smoothing"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  print(plot_1)
  
  


##############################################################
################################################################################
max_date <- max(sales$InvoiceDate, na.rm = TRUE)

rfm <- sales %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.integer(difftime(max_date, max(InvoiceDate), units = "days")),
    Frequency = n_distinct(StockCode),                 # simple proxy
    Monetary = sum(Revenue, na.rm = TRUE)
  ) %>%
  mutate(
    R_Score = ntile(-Recency, 5),             # lower recency -> higher score
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5),
    RFM_Score = R_Score + F_Score + M_Score
  ) %>%
  arrange(desc(RFM_Score))

View(rfm)
################################################################################
#COUNTRY SALES #
sales_by_country_a <- sales %>%
  group_by(Country)%>%
  summarise(total_sales= sum(Revenue , na.rm = TRUE))%>%
  arrange(desc(total_sales))%>%
  sales_by_country_a<-slice_max(total_sales , n=10)%>%
  slice_max(order_by = total_sales, n = 10)
  

View(sales_by_country_a)
###########################PLOT FOR TOP 10 COUNTRY##############################

plot_2<-ggplot(sales_by_country_a, aes(x=reorder(Country , -total_sales),y=total_sales) )+
  labs(x="Country" , y="total_sales" ,title = "top 10 Country with highest sales")+
  geom_col(fill="orange")+
  theme_minimal()

print(plot_2)
################################################################################
### as UK has the most sales so we are removing it to see the other countries####

sales_by_country <- sales %>%
  filter(Country != "United Kingdom") %>%
  group_by(Country) %>%
  summarise(total_sales = sum(Revenue, na.rm = TRUE)) %>%
  arrange(desc(total_sales))



plot_3<-ggplot(sales_by_country, aes(x = reorder(Country, -total_sales), y = total_sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Sales by Country",
       x = "Country",
       y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 print(plot_3)

######################################################################################

#########################TOP 10 STOCK CODE in united kingdom  ########################
# 90% of data in the data set contains of the united kingdom sales 
mutate(sales , Quantity == as.numeric(Quantity))

uk_sales <- sales %>%
  mutate (Quantity = as.numeric(Quantity))%>%
  filter(Country == "United Kingdom") %>%
  select(StockCode, Quantity)%>%
  filter(!is.na(Quantity)) %>%
  group_by(StockCode)%>%
  summarise(
    TotalQuantity = sum(Quantity, na.rm = TRUE)
  ) %>%
  arrange(desc(TotalQuantity))

uk_sales
nrow(uk_sales)
unique(uk_sales $StockCode)
n_distinct(uk_sales $StockCode)
  
View(uk_sales)

top10 <- uk_sales %>% slice_max(TotalQuantity, n = 10)

plot_4<-ggplot(top10, aes(x = reorder(StockCode, TotalQuantity), y = TotalQuantity)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Top 10 StockCodes by Quantity (UK Sales)",
       x = "StockCode", y = "Total Quantity")
print(plot_4)

################################################################################

max_date <- max(sales$InvoiceDate, na.rm = TRUE)

rfm <- sales %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.integer(difftime(max_date, max(InvoiceDate), units = "days")),
    Frequency = n_distinct(StockCode),                 # simple proxy
    Monetary = sum(Revenue, na.rm = TRUE)
  ) %>%
  mutate(
    R_Score = ntile(-Recency, 5),                      # lower recency -> higher score
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5),
    RFM_Score = R_Score + F_Score + M_Score
  ) %>%
  arrange(desc(RFM_Score))



View(rfm)
write.csv(rfm ,file="RFM_ANALYSIS.csv")

################################################################################

p1 <- ggplot(rfm, aes(x = Recency)) +
  geom_histogram(fill = "orange", bins = 30) +
  theme_minimal() + labs(title = "Recency Distribution")

p2 <- ggplot(rfm, aes(x = Frequency)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  theme_minimal() + labs(title = "Frequency Distribution")

p3 <- ggplot(rfm, aes(x = Monetary)) +
  geom_histogram(fill = "green", bins = 30) +
  theme_minimal() + labs(title = "Monetary Distribution")

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)


################################################################################
rfm_segments <- rfm %>%
  mutate(
    Customer_Segment = case_when(
      RFM_Score >= 13 ~ "Champions",
      RFM_Score >= 11 ~ "Loyal Customers",
      RFM_Score >= 9 ~ "Potential Loyalists",
      RFM_Score >= 7 ~ "New Customers",
      RFM_Score >= 5 ~ "At Risk",
      TRUE ~ "Lost Customers"
    )
  )

View(rfm_segments)
segment_summary <- rfm_segments %>%
  group_by(Customer_Segment) %>%
  summarise(
    Customer_Count = n(),
    Avg_Monetary = mean(Monetary, na.rm = TRUE),
    Total_Revenue = sum(Monetary, na.rm = TRUE),
    Avg_Frequency = mean(Frequency, na.rm = TRUE),
    Avg_Recency = mean(Recency, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Revenue))
View(segment_summary)

###############################################################################
######################PLOTTING THE RFM SEGMENTS ###############################

p_revenue_segment <- ggplot(segment_summary, 
                            aes(x = reorder(Customer_Segment, Total_Revenue), 
                                y = Total_Revenue, 
                                fill = Customer_Segment)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0("£", round(Total_Revenue/1000, 1), "K")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(labels = dollar_format(prefix = "£", scale = 1e-3, suffix = "K")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Total Revenue by Customer Segment",
    subtitle = "Champions and Loyal Customers drive the majority of revenue",
    x = "Customer Segment",
    y = "Total Revenue (£K)",
    caption = "Source: RFM Analysis - UK Customers"
  )


print(p_revenue_segment)

################################################################################
######################CUSTOMER COUNT SEGMENT ###################################

p_customer_count <- ggplot(segment_summary, aes(x = reorder(Customer_Segment, Customer_Count), 
                                                y = Customer_Count, 
                                                fill = Customer_Segment)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = Customer_Count), hjust = -0.1, size = 4, fontface = "bold") +

  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Customer Count by Segment",
    subtitle = "Understanding the size of each customer group",
    x = "Customer Segment",
    y = "Number of Customers"
  )

print(p_customer_count)

################################################################################
##########################DISTRIBUTION PLOT#####################################

p_rfm_distribution <- ggplot(rfm, aes(x = RFM_Score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = mean(RFM_Score)), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean(rfm$RFM_Score) + 0.5, y = max(table(rfm$RFM_Score)) * 0.8, 
           label = paste("Mean:", round(mean(rfm$RFM_Score), 1)), color = "red", size = 4) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Distribution of Customer Value Scores",
    subtitle = "Higher scores indicate more valuable customers",
    x = "RFM Score (3-15 scale)",
    y = "Number of Customers"
  )


print(p_rfm_distribution)



################################################################################


################################################################################

ggplot(rfm, aes(x = RFM_Score)) +
  geom_bar(fill = "purple") +
  theme_minimal() +
  labs(title = "Distribution of RFM Scores", x = "RFM Score", y = "Number of Customers")


##########################################

key <- sales %>%
  filter(Country == "United Kingdom") %>%
  select(StockCode, Description)

View(key)
nrow(key)

################################################################################
################################ RFM SALES ANALYSIS FOR UK #####################
RFM_UK<- sales%>%
  filter(Country =="United Kingdom" )

write.csv(RFM_UK, file="RFM_UK.csv" , row.names = FALSE )

max_date <- max(RFM_UK $ InvoiceDate, na.rm = TRUE)

rfm <- RFM_UK %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.integer(difftime(max_date, max(InvoiceDate), units = "days")),
    Frequency = n_distinct(StockCode),                 # simple proxy
    Monetary = sum(Revenue, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    R_Score = ntile(-Recency, 5),                      # lower recency -> higher score
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5),
    RFM_Score = R_Score + F_Score + M_Score
  ) %>%
  arrange(desc(RFM_Score))

####################################################################################################
################################RFM SEGMENTS FOR UK SALES###############################

rfm_segments <- rfm %>%
  mutate(
    Customer_Segment = case_when(
      RFM_Score >= 13 ~ "Champions",
      RFM_Score >= 11 ~ "Loyal Customers",
      RFM_Score >= 9 ~ "Potential Loyalists",
      RFM_Score >= 7 ~ "New Customers",
      RFM_Score >= 5 ~ "At Risk",
      TRUE ~ "Lost Customers"
    )
  )
segment_summary <- rfm_segments %>%
  group_by(Customer_Segment) %>%
  summarise(
    Customer_Count = n(),
    Avg_Monetary = mean(Monetary, na.rm = TRUE),
    Total_Revenue = sum(Monetary, na.rm = TRUE),
    Avg_Frequency = mean(Frequency, na.rm = TRUE),
    Avg_Recency = mean(Recency, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Total_Revenue))



p_revenue_segment <- ggplot(segment_summary, aes(x = reorder(Customer_Segment, Total_Revenue), 
                                                 y = Total_Revenue, 
                                                 fill = Customer_Segment)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0("£", round(Total_Revenue/1000, 1), "K")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  
  coord_flip() +
  
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(labels = dollar_format(prefix = "£", scale = 1e-3, suffix = "K")) +
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()) +
  
  
  labs(
    title = "Total Revenue by Customer Segment",
    subtitle = "Champions and Loyal Customers drive the majority of revenue",
    x = "Customer Segment",
    y = "Total Revenue (£K)",
    caption = "Source: RFM Analysis - UK Customers"
  )
print(p_revenue_segment)

########################################################################################################

####################################################################################################

p1 <- ggplot(rfm, aes(x = Recency)) +
  geom_histogram(fill = "orange", bins = 30) +
  theme_minimal() +
  labs(title = "Recency Distribution",
       x = "Days Since Last Purchase",
       y = "Number of Customers")

p2 <- ggplot(rfm, aes(x = Frequency)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  theme_minimal() +
  scale_x_continuous(labels = comma) +  
  labs(title = "Frequency Distribution",
       x = "Number of Transactions",
       y = "Number of Customers")

p3 <- ggplot(rfm, aes(x = Monetary)) +
  geom_histogram(fill = "green", bins = 30) +
  theme_minimal() +
  scale_x_continuous(labels = dollar_format(prefix = "£", big.mark = ",")) + 
  labs(title = "Monetary Distribution",
       x = "Total Spend (£)",
       y = "Number of Customers")

p2 <- p2 + scale_x_log10(labels = comma) +
  labs(subtitle = "Log scale for better visibility")

p3 <- p3 + scale_x_log10(labels = dollar_format(prefix = "£", big.mark = ",")) +
  labs(subtitle = "Log scale for better visibility")


RFM_GRID<-gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
RFM_GRID

View(RFM_UK)

###########################################################################
################### SAVING THE GRAPHS ####################################

dir.create("SavedPlots")

ggsave("SavedPlots/01_monthly_revenue.png", 
       plot = plot_1, 
       width = 12, height = 6, dpi = 300, bg = "white")


ggsave("SavedPlots/Top 10 Country.png", 
       plot = plot_2, 
       width = 12, height = 6, dpi = 300, bg = "white")


ggsave("SavedPlots/Sales by country excluding UK.png", 
       plot = plot_3, 
       width = 12, height = 6, dpi = 300, bg = "white")


ggsave("SavedPlots/Top 10 StockCodes(UK).png", 
       plot = plot_4, 
       width = 12, height = 6, dpi = 300, bg = "white")


ggsave("SavedPlots/Revenue Segment(UK Customers).png", 
       plot = p_revenue_segment, 
       width = 12, height = 6, dpi = 300, bg = "white")


ggsave("SavedPlots/p_customer_count(UK).png", 
       plot = p_customer_count, 
       width = 12, height = 6, dpi = 300, bg = "white")


ggsave("SavedPlots/p_rfm_distribution.png", 
       plot = p_rfm_distribution, 
       width = 12, height = 6, dpi = 300, bg = "white")

ggsave("SavedPlots/p_revenue_segment(UK).png", 
       plot = p_revenue_segment, 
       width = 19, height = 6, dpi = 300, bg = "white")


ggsave("SavedPlots/RFM GRID.png", 
       plot = RFM_GRID, 
       width = 13, height = 6, dpi = 300, bg = "white")


############################################################################
####################### CREATING WORKBOOK##################################

workbook_project <- createWorkbook()

addWorksheet(workbook_project, "Project Report")

writeData(workbook_project, "Project Report", 
          "Online Retail Analytics - Report", 
          startRow = 1, startCol = 1)

titleStyle <- createStyle(fontSize = 18, textDecoration = "bold")
addStyle(workbook_project, "Project Report", titleStyle, rows = 1, cols = 1)

insertImage(workbook_project, "Project Report", 
            "SavedPlots/01_monthly_revenue.png", 
            startRow = 4, startCol = 1, 
            width = 10, height = 5)

insertImage(workbook_project, "Project Report", 
            "SavedPlots/p_revenue_segment(UK).png", 
            startRow = 4, startCol = 1, 
            width = 10, height = 5)
#####################################SHEET2#################################
addWorksheet(workbook_project, "Revenue Analysis")

writeData(workbook_project, "Revenue Analysis", "Monthly Revenue Breakdown", 
          startRow = 1, startCol = 1)
addStyle(workbook_project, "Revenue Analysis", titleStyle, rows = 1, cols = 1)

writeDataTable(workbook_project, "Revenue Analysis", monthly, 
               startRow = 3, startCol = 1, tableStyle = "TableStyleMedium2")

insertImage(workbook_project, "Revenue Analysis", 
            "SavedPlots/01_monthly_revenue.png", 
            startRow = nrow(monthly) + 6, startCol = 1, 
            width = 12, height = 6)

##############################SHEET 3 ######################################

addWorksheet(workbook_project, "Geographic Analysis")

writeData(workbook_project, "Geographic Analysis", "Sales by Country", 
          startRow = 1, startCol = 1)
addStyle(workbook_project, "Geographic Analysis", titleStyle, rows = 1, cols = 1)


insertImage(workbook_project, "Geographic Analysis", 
            "Savedplots/Top 10 Country.png", 
            startRow = 3, startCol = 1, 
            width = 10, height = 6)

insertImage(workbook_project, "Geographic Analysis", 
            "Savedplots/Sales by country excluding UK.png", 
            startRow = 3, startCol = 1, 
            width = 13, height = 6)

writeDataTable(workbook_project, "Geographic Analysis", sales_by_country_a, 
               startRow = 3, startCol = 12, tableStyle = "TableStyleMedium2")

################################ SHEET 4 ################################
addWorksheet(workbook_project, "Product Analysis")

writeData(workbook_project, "Product Analysis", "Top Performing Products (UK)", 
          startRow = 1, startCol = 1)
addStyle(workbook_project, "Product Analysis", titleStyle, rows = 1, cols = 1)

# Add top 10 stock codes plot
insertImage(workbook_project, "Product Analysis", 
            "Savedplots/Top 10 StockCodes(UK).png", 
            startRow = 3, startCol = 1, 
            width = 10, height = 6)

# Add product data table
writeDataTable(workbook_project, "Product Analysis", top10, 
               startRow = 3, startCol = 12, tableStyle = "TableStyleMedium2")

##############################SHEET 5#######################################
addWorksheet(workbook_project, "Customer Segmentation")

writeData(workbook_project, "Customer Segmentation", "RFM Analysis & Customer Segments", 
          startRow = 1, startCol = 1)
addStyle(workbook_project, "Customer Segmentation", titleStyle, rows = 1, cols = 1)

# Segment summary table
writeData(workbook_project, "Customer Segmentation", "Segment Performance Summary", 
          startRow = 3, startCol = 1)
writeDataTable(workbook_project, "Customer Segmentation", segment_summary, 
               startRow = 4, startCol = 1, tableStyle = "TableStyleMedium2")

# Revenue by segment plot
insertImage(workbook_project, "Customer Segmentation", 
            "Savedplots/Revenue Segment(UK Customers).png", 
            startRow = 15, startCol = 1, 
            width = 10, height = 6)

# Customer count plot
insertImage(workbook_project, "Customer Segmentation", 
            "Savedplots/p_customer_count(UK).png", 
            startRow = 45, startCol = 1, 
            width = 10, height = 6)

####################################SHEET 6####################################
addWorksheet(workbook_project, "RFM Details")

writeData(workbook_project, "RFM Details", "RFM Score Distribution & Metrics", 
          startRow = 1, startCol = 1)
addStyle(workbook_project, "RFM Details", titleStyle, rows = 1, cols = 1)

# RFM distributions plot
insertImage(workbook_project, "RFM Details", 
            "Savedplots/p_rfm_distribution.png", 
            startRow = 3, startCol = 1, 
            width = 14, height = 5)

# RFM score distribution
insertImage(workbook_project, "RFM Details", 
            "Savedplots/RFM GRID.png", 
            startRow = 30, startCol = 1, 
            width = 10, height = 6)

# Add RFM data sample (top 100 customers)
writeData(workbook_project, "RFM Details", "Top 100 Customers by RFM Score", 
          startRow = 60, startCol = 1)
writeDataTable(workbook_project, "RFM Details", head(rfm_segments, 100), 
               startRow = 61, startCol = 1, tableStyle = "TableStyleMedium2")

################################ SHEET 7###############################
addWorksheet(workbook_project, "Data Summary")

writeData(workbook_project, "Data Summary", "Complete RFM Customer Data", 
          startRow = 1, startCol = 1)
addStyle(workbook_project, "Data Summary", titleStyle, rows = 1, cols = 1)

writeDataTable(workbook_project, "Data Summary", rfm_segments, 
               startRow = 3, startCol = 1, tableStyle = "TableStyleMedium2")

###########################finally#########################################

saveWorkbook(workbook_project, "online_retail_report.xlsx", overwrite = TRUE)


##########################################################################

on.exit(dbDisconnect(dbt))
