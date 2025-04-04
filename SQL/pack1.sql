-- 1.1. Get all the unique companies

SELECT DISTINCT COMPANY FROM MANUFACTURER;

-- 1.2. Get the total number of companies

SELECT COUNT(DISTINCT  COMPANY) FROM MANUFACTURER;

-- 1.3. Get all the unique wares in Food category

SELECT  WARE FROM CATEGORY WHERE CLASS = 'Food';  

-- 1.4. Get a list of all unique companies which names begin with letter A or B, sorted in alphabetical order

SELECT DISTINCT COMPANY
	FROM MANUFACTURER 
	WHERE COMPANY  LIKE 'A%'
	OR COMPANY LIKE 'B%' 
	ORDER BY COMPANY  ASC;

-- 1.5. Get all the unique final products (i.e. the wares that are not in use as a material anywhere)

SELECT WARE FROM PRODUCT EXCEPT SELECT WARE FROM MATERIAL;

-- 1.6. Get all the unique wares that could not be produced

SELECT WARE FROM CATEGORY EXCEPT SELECT WARE FROM MATERIAL;

-- 1.7. Get all the unique wares that both materials and products

SELECT WARE FROM MATERIAL INTERSECT SELECT WARE FROM PRODUCT;

-- 1.8. Get the minimal and maximal prices of Paper

SELECT MIN(PRICE), MAX(PRICE) FROM PRODUCT WHERE WARE = 'Paper';

-- 1.9. Get the average price and variance price of Meat, both rounded to one decimal point.

SELECT ROUND(AVG(PRICE), 1), ROUND(AVG(PRICE * PRICE) - (AVG(PRICE) * AVG(PRICE)), 1) FROM PRODUCT WHERE WARE = 'Meat';
