show DATABASES;
USE online_retail;
show tables;
select * from transactions;

-- CREATING VIEW  FOR BETTER UNDERSTANDING--

create or replace view clean_sales as 
select    id, InvoiceNo, StockCode, Description,
  Quantity, InvoiceDate, UnitPrice, CustomerID, Country,
  (Quantity * UnitPrice) AS Revenue
  
from transactions
where 
InvoiceNo NOT LIKE 'C%' 
and Quantity  > 0
and UnitPrice > 0 
 AND CustomerID IS NOT NULL; 
 
 select * from clean_sales;
 
describe clean_sales;
-- ------------------------------------------------MONTHLY SALES DATA---------------------------------------------------- --
select date_format(str_to_date(InvoiceDate , '%m/%d/%Y %H:%i') , '%b  %Y' ) as month,
		round(sum(Revenue) , 2) as revenue
from clean_sales
where InvoiceDate is not null 
group by month
order by month;
-- ---------------------------END OF QUERY--------------------------------------------------------------------------------    --


-- ---------------------------------------------REVENUE OF THE TOP 10 COUNTRIES--------------------------------------------- --

select Country , round(sum(Revenue),2) as revenue
from clean_sales
GROUP BY Country
order by revenue desc
limit 10;

-- ------------------------------------------------END OF THE ANALYSIS---------------------------------------------------------- --

-- ------------------------------------------------TOP 10 PRODUCTS---------------------------------------------------------- --

SELECT StockCode, COALESCE(Description,'') AS Description, SUM(Quantity) AS total_qty
FROM clean_sales
GROUP BY StockCode, Description
ORDER BY total_qty DESC
LIMIT 10;
-- ------------------------------------------------END OF THE ANALYSIS---------------------------------------------------------- --

-- --------------------------------------------------CUSTOMER ANALYIS---------------------------------------------------------- --

SELECT CustomerID, ROUND(SUM(Revenue),2) AS total_spend, COUNT(DISTINCT InvoiceNo) AS orders
FROM clean_sales
GROUP BY CustomerID
ORDER BY total_spend DESC
LIMIT 10;
-- ------------------------------------------------END OF THE ANALYSIS---------------------------------------------------------- --
select * from clean_sales


