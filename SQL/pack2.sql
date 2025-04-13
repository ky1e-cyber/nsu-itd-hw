-- 2.1. Get all the unique companies producing Drinking water in alphabetic order.
SELECT MANUFACTURER.COMPANY AS COMPANY
FROM MANUFACTURER, PRODUCT
WHERE PRODUCT.WARE='Drinking water' AND MANUFACTURER.RECIPE_ID=PRODUCT.RECIPE_ID
ORDER BY COMPANY ASC;

-- 2.2. Get all the companies producing wares in Raw food category. Result must contain unique pairs of
-- companies and wares producing by them from the given category and must be sorted by the ware first and
-- the company name next.
SELECT  DISTINCT MANUFACTURER.COMPANY AS COMPANY, 
										PRODUCT.WARE AS WARE 
FROM PRODUCT 
INNER JOIN CATEGORY 
ON CATEGORY.CLASS='Raw food' AND CATEGORY.WARE=PRODUCT.WARE 
INNER JOIN MANUFACTURER ON MANUFACTURER.RECIPE_ID=PRODUCT.RECIPE_ID 
ORDER BY WARE ASC, COMPANY ASC;

-- 2.3. Get all the unique wares in alphabetical order that can be produced from wares in Mineral category.
SELECT DISTINCT PRODUCT.WARE
FROM MATERIAL
INNER JOIN CATEGORY  ON CATEGORY.CLASS='Mineral' AND CATEGORY.WARE=MATERIAL.WARE 
INNER JOIN PRODUCT ON MATERIAL.RECIPE_ID=PRODUCT.RECIPE_ID
ORDER BY PRODUCT.WARE ASC;

