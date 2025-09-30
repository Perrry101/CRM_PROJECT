# Online Retail Customer Segmentation & RFM Analysis

A data analytics project using **R** and **SQL** to segment customers with the RFM model, analyze online retail transactions, and deliver insights through dashboards.

---

##  Overview  
Analyzed **111K+ transactions** from a UK-based online retailer to:  
- Identify high-value customers  
- Track revenue and seasonal trends  
- Find top-performing products  
- Build executive dashboards  

---

##  Business Problem  
The company needed to:  
1. Understand customer behavior  
2. Optimize marketing spend  
3. Predict revenue patterns  
4. Improve retention  

---

##  Dataset  
**Source:** [UCI Online Retail Dataset](https://archive.ics.uci.edu/dataset/352/online+retail)  

- **Period:** Dec 2010 – Dec 2011  
- **Records:** 111,528 (after cleaning)  
- **Features:** InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country  
- **Data Issues Fixed:** Missing dates, null IDs, negative quantities, invalid prices  

---

##  Methodology  
1. **Data Cleaning (SQL):** Filtered invalid records, fixed date formats  
2. **Processing (R):** Revenue calculation, feature engineering  
3. **RFM Analysis:** Scored Recency, Frequency, Monetary (1–5 scale)  
4. **Segmentation:** Champions, Loyal, New, At Risk, Lost Customers  
5. **Trend Analysis:** Monthly revenue, seasonal peaks  
6. **Insights:** Country & product performance, segment revenue share  

---

##  Key Findings  
- **Champions & Loyal Customers** = 65% of revenue  
- UK contributes **90% of sales**; top international: Netherlands, EIRE, Germany  
- **Q4 shows peak revenue** (holiday season)  
- Top 10 products = 40% of UK sales  

---

##  Tech Stack  
- **Languages:** R, SQL (MariaDB/MySQL)  
- **R Libraries:** dplyr, ggplot2, lubridate, RMariaDB, tidyr, openxlsx  
- **Visualization:** ggplot2, Excel dashboards  

---

##  Business Recommendations  
- Prioritize **Champions** with VIP programs  
- Launch **win-back offers** for At-Risk customers  
- Diversify product range beyond top 10 items  
- Expand into **Netherlands & EIRE** markets  
- Plan **inventory ahead of Q4 demand**  

---

##  Future Enhancements  
- Predict Customer Lifetime Value (CLV)  
- Cohort analysis for retention  
- Interactive Shiny dashboards  
- Market Basket Analysis  
- Automated monthly reports  

---

## Contact  
**Pranay Asopa**  
- LinkedIn: https://www.linkedin.com/in/pranay-asopa-584891281/  
- Email:pranay.asopa117@gmail.com  


---

> ⚡ *This is a portfolio project for educational purposes. Business recommendations would require further context and stakeholder input.*
