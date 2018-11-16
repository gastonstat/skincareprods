mysql -u root

--CREATE DATABASE care_products;

SHOW DATABASES;

USE care_products;

--DROP TABLE category_ingredients;

--	id MEDIUMINT NOT NULL AUTO_INCREMENT,


CREATE TABLE category_ingredients (
	category varchar(100) DEFAULT NULL,
	ingredient varchar(100) DEFAULT NULL,
	concern varchar(10) DEFAULT NULL,
	quantity float(5) DEFAULT NULL,
	proportion float(20) DEFAULT NULL,
	id MEDIUMINT NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (id)
);

SHOW TABLES;

SHOW COLUMNS IN category_ingredients;


LOAD DATA LOCAL INFILE '/Users/Gaston/Documents/Insight/data/good_guide/category_ingredients.csv'
INTO TABLE category_ingredients
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;


SELECT * FROM category_ingredients LIMIT 10;


-- categories
SELECT category
FROM category_ingredients
GROUP BY category;


SELECT ingredient, proportion 
FROM category_ingredients
WHERE category = 'fragrance-for-men' AND concern = 'high'
ORDER BY proportion DESC;


SELECT ingredient, proportion 
FROM category_ingredients
WHERE category = 'shampoo' AND concern = 'high'
ORDER BY proportion DESC
LIMIT 10;


SELECT category, proportion 
FROM category_ingredients
WHERE ingredient = 'triclosan'
ORDER BY proportion DESC;


