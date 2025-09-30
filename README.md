# CRM_PROJECT
Used R studio to Perform RFM analysis on the Online Retail Data Available on  Kaggle
Online Retail Customer Segmentation & RFM Analysis

A data analytics project using R and SQL to segment customers with the RFM model, analyze online retail transactions, and deliver insights through dashboards.

Overview

Analyzed 111K+ transactions from a UK-based online retailer to:

Identify high-value customers

Track revenue and seasonal trends

Find top-performing products

Build executive dashboards

Business Problem

The company needed to:

Understand customer behavior

Optimize marketing spend

Predict revenue patterns

Improve retention

Dataset

Source: UCI Online Retail Dataset

Period: Dec 2010 – Dec 2011

Records: 111,528 (after cleaning)

Features: InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country

Issues fixed: Missing dates, null IDs, negative quantities, invalid prices

Methodology

Data Cleaning (SQL) – Filtered invalid records, fixed date formats

Processing (R) – Revenue calculation, feature engineering

RFM Analysis – Scored Recency, Frequency, Monetary (1–5 scale)

Segmentation – Champions, Loyal, New, At Risk, Lost Customers

Trend Analysis – Monthly revenue, seasonal peaks

Insights – Top countries, top UK products, segment revenue share

Key Findings

Champions & Loyal Customers = 65% of revenue

UK contributes 90% of sales; top international: Netherlands, EIRE, Germany

Q4 shows peak revenue (holiday season)

Top 10 products = 40% of UK sales

Tech Stack

Languages: R, SQL (MariaDB/MySQL)

Libraries: dplyr, ggplot2, lubridate, RMariaDB, tidyr, openxlsx

Visualization: ggplot2, Excel dashboards

Business Recommendations

Prioritize Champions with VIP programs

Launch win-back offers for At-Risk customers

Diversify product range beyond top 10 items

Expand into Netherlands & EIRE markets

Plan inventory ahead of Q4 demand

Future Enhancements

Predict Customer Lifetime Value

Cohort analysis for retention

Interactive Shiny dashboards

Market Basket Analysis

Automated monthly reports
by:
Pranay Asopa

