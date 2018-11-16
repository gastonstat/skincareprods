
mysql -u root

CREATE DATABASE care_products;

SHOW DATABASES;

USE care_products;

DROP TABLE products;

CREATE TABLE products (
	id_prod varchar(200),
	product varchar(300) DEFAULT NULL,
	category varchar(100) DEFAULT NULL,
	id_category varchar(100) DEFAULT NULL,	
	health float(5) DEFAULT NULL,
	num_ings int(10),
	has_high int(5),
	has_fragrance int(5),
	PRIMARY KEY(id_prod)
);

SHOW TABLES;

SHOW COLUMNS IN products;


-- Use this for local deployment
LOAD DATA LOCAL INFILE '/Users/Gaston/Documents/Insight/data/good_guide/care_products.csv'
INTO TABLE products
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;



SELECT * FROM products LIMIT 5;


SELECT COUNT(has_high) 
FROM products
WHERE has_high = 1;



mysql -u root

USE care_products;

SHOW TABLES;


-- query test
SELECT product
FROM products
WHERE category = 'baby-sunscreen' 
AND has_high = 1
AND num_ings > 10;


-- display all categories
SELECT category
FROM products
GROUP BY category;

-- average health score
SELECT AVG(health)
FROM products
WHERE category = 'baby-lotion';

-- test id_category
SELECT id_category, category
FROM products
WHERE category='baby-lotion'
GROUP BY category;

-- recommendation
SELECT product, health
FROM products
WHERE category='anti-aging'
ORDER BY health DESC
LIMIT 5;


-- ==================================
-- Overall Summary Statistics 
-- ==================================

-- average health score by categories
SELECT category, AVG(health)
FROM products
GROUP BY category;

-- average number of ingredients by categories
SELECT category, AVG(num_ings)
FROM products
GROUP BY category
ORDER BY AVG(num_ings) DESC;

-- average health score by categories
SELECT category, COUNT(category)
FROM products
GROUP BY category
ORDER BY COUNT(category) DESC;

-- proporiton of products containing high-level-concern ingredients by categories
SELECT category, SUM(has_high) / COUNT(category) AS prop_high, AVG(num_ings)
FROM products
GROUP BY category
ORDER BY prop_high DESC;
