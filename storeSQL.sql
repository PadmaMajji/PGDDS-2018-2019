# Task 1
# Question A-  Describe the data in hand in your own words

# superstoredb contains Sales details of transaction of a superstore. 
# The superstoredb has 5 tables, namely cust_dimen (containing details about 
# customer and their respective locations), prod_dimen (containing 
# product category and their subcategories), orders_dimen (containing order ID,
# date and priority), shipping_dimen (ship date, order and shipping 
# mode), and market_fact (orderwise customerwise marketwise orderquantity, 
# sales value, discount profit and shipping cost details).
# Upon proper database design these tables will get informationn handy upon 
# querying. These are having dimensions and has facts releated to it. Using 
# market_fact, we can analyze the data in various insights which will help in taking decisions 
# based on orderwise, productwise, shippingwise, customerwise profitability etc.

# Question B- Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.) 
# 1. cust_dimen - Cust_id is Primary Key, there is no foreign key
# 2. prod_dimen - Prod_id is Primary Key,there is no foreign key 
# 3. orders_dimen - Ord_id is Primary Key, Order_ID is foreign key 
# 4. shipping_dimen - Ship_id is primary key and Order_ID is foreign key.
# 5. market_fact - Ord_id, Prod_id, Ship_id and Cust_id are foreign key. There is No Primary Key.

# Task 2
# Question A - Find the total and the average sales (display total_sales and avg_sales) 

select sum(Sales) as Total_sales, avg(Sales) as Avg_sales from market_fact;
#(or)
select round(sum(Sales)) as Total_Sales, round(avg(Sales)) as Avg_Sales from market_fact;

#Question B - Display the number of customers in each region in decreasing order of no_of_customers.
# The result should contain columns Region, no_of_customers 
select Region, count(*) as no_of_customers from cust_dimen group by Region order by no_of_customers desc;

#Question C - Find the region having maximum customers (display the region name and max(no_of_customers)
select Region, count(*) as no_of_customers from cust_dimen group by Region order by no_of_customers desc limit 1;

#Question D - Find the number and id of products sold in decreasing order of products sold 
# (display product id, no_of_products sold)
select Prod_id as Product_ID, sum(Order_Quantity) as No_of_products_sold 
from market_fact 
group by Product_ID 
order by no_of_products_sold desc;

#Question E - Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer name, no_of_tables purchased) 
select cust_dimen.Customer_Name, cust_dimen.Region, prod_dimen.Product_Sub_Category, sum(market_fact.Order_Quantity) as No_of_Tables_Purchased 
from market_fact
join cust_dimen on market_fact.Cust_id = cust_dimen.Cust_id
join prod_dimen on market_fact.Prod_id = prod_dimen.Prod_id
where cust_dimen.Region = "Atlantic" and prod_dimen.Product_Sub_Category = "Tables"
group by cust_dimen.Customer_Name
order by sum(market_fact.Order_Quantity) desc;

# Task 3
#Question A - Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)? 
select prod_dimen.Product_Category, round(sum(market_fact.Profit),1) as Profits
from market_fact
inner join prod_dimen
on prod_dimen.Prod_id= market_fact.Prod_id 
group by prod_dimen.Product_Category
order by round(sum(market_fact.Profit),1) desc;

#Question B - Display the product category, product sub-category and the profit within each subcategory in three columns. 
select product.Product_Category, product.Product_Sub_Category, round(sum(market.Profit),1) as Profit
from market_fact market
join prod_dimen product
on market.Prod_id = product.Prod_id
group by product.Product_Sub_Category, product.Product_Category
order by Profit desc;

#Question C - Where is the least profitable product subcategory shipped the most? 
#For the least profitable product sub-category, display the  region-wise no_of_shipments and 
#the profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region) 
# Note: You can hardcode the name of the least profitable product subcategory 

select cust_dimen.Region, count(market_fact.Ship_id) as No_of_Shipments, round(sum(market_fact.Profit),1) as Profit_in_Each_Region
from market_fact
join cust_dimen on market_fact.Cust_id = cust_dimen.Cust_id 
join prod_dimen on market_fact.Prod_id = market_fact.Prod_id
where Product_Sub_Category = (select prod_dimen.Product_Sub_Category from prod_dimen
								join market_fact on prod_dimen.Prod_id = market_fact.Prod_id
                                group by prod_dimen.Product_Sub_Category
                                order by sum(market_fact.Profit) limit 1)
group by cust_dimen.Region
order by round(sum(market_fact.Profit),1) desc;


## Below is the Command with hardcoded least profitable product subcateogry

select cust_dimen.Region, count(market_fact.Ship_id) as No_of_Shipments, round(sum(market_fact.Profit),1) as Profit_in_Each_Region
from market_fact
join cust_dimen on market_fact.Cust_id = cust_dimen.Cust_id 
join prod_dimen on market_fact.Prod_id = market_fact.Prod_id
where Product_Sub_Category = "TABLES"
group by cust_dimen.Region
order by round(sum(market_fact.Profit),1) desc;

