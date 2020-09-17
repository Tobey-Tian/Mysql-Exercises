# Create DATABASE db_Group_Project;
USE db_Group_Project;

#a. How many:
# Store shopping trips are recorded in your database? 7596145
SELECT count(Distinct TC_id) FROM dta_at_TC;

# Households appear in your database? 39577
SELECT count(Distinct hh_id) FROM dta_at_hh;

# Stores of different retailers appear in our data base? 863
SELECT TC_retailer_code, count(Distinct TC_retailer_code_store_code) FROM dta_at_TC group by TC_retailer_code;

# Different products are recorded? 4230759
SELECT count(Distinct prod_id) FROM dta_at_prod_id;

# i. Products per category
SELECT group_at_prod_id, count(Distinct prod_id) FROM dta_at_prod_id group by group_at_prod_id;

# and products per module
SELECT module_at_prod_id, count(Distinct prod_id) FROM dta_at_prod_id group by module_at_prod_id;

# ii. Plot the distribution of products and modules per department
SELECT department_at_prod_id, count(Distinct prod_id)/1000 FROM dta_at_prod_id group by department_at_prod_id;
SELECT department_at_prod_id, module_at_prod_id, count(Distinct prod_id)/1000 FROM dta_at_prod_id group by department_at_prod_id, module_at_prod_id ;

# Transactions?
# i. Total transactions. 5651255
SELECT count(Distinct TC_id) FROM dta_at_TC_upc;

# transactions realized under some kind of promotion. 2670312
SELECT deal_flag_at_TC_prod_id, count(Distinct TC_id) FROM dta_at_TC_upc where deal_flag_at_TC_prod_id=1;

# creating connections between tables
-- Creating Primary Keys:
#Households table:
ALTER TABLE dta_at_hh
  ADD CONSTRAINT PK_hh_id
    PRIMARY KEY (hh_id);

#Trips table:
ALTER TABLE dta_at_TC
  ADD CONSTRAINT PK_TC_id
    PRIMARY KEY (TC_id);

# Product table
ALTER TABLE dta_at_prod_id
  ADD CONSTRAINT PK_prod_id
    PRIMARY KEY (prod_id);

#Purchases table
ALTER TABLE dta_at_TC_upc
  ADD CONSTRAINT PK_TC_id
    PRIMARY KEY (TC_id, prod_id);

-- Creating Foreign Keys:
# Connecting Trips table with households:
ALTER TABLE dta_at_TC   ADD CONSTRAINT FK3_transaction_hh      FOREIGN KEY (hh_id)  REFERENCES dta_at_hh(hh_id);

# Connecting Purchases table with Products:
ALTER TABLE dta_at_TC_upc   ADD CONSTRAINT FK3_purchases_TC       FOREIGN KEY (TC_id)     REFERENCES dta_at_TC(TC_id);
#DELETE FROM dta_at_TC_upc;
ALTER TABLE dta_at_TC_upc   ADD CONSTRAINT FK3_purchases_prod     FOREIGN KEY (prod_id)   REFERENCES dta_at_prod_id (prod_id);


