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

-- 2.4. Get all the unique companies producing both wares from Fuel and Food categories. Use appropriate set
-- operation in the query.
SELECT COMPANY
FROM MANUFACTURER m, CATEGORY c, PRODUCT p WHERE c.CLASS = 'Fuel' and c.WARE = p.WARE
INTERSECT SELECT COMPANYx	
FROM MANUFACTURER m, CATEGORY c, PRODUCT p WHERE c.CLASS = 'Food' and c.WARE = p.WARE;

-- 2.5. Rewrite the previous query without using the set operations. Enrich the result with wares from both
-- categories. It is acceptable to get multiple rows for companies producing multiple wares from any category
-- mentioned, but the rows must be unique in result.
SELECT DISTINCT p.WARE, m.COMPANY FROM CATEGORY  c  
INNER JOIN PRODUCT p ON p.WARE = c.WARE 
INNER JOIN MANUFACTURER m ON m.RECIPE_ID = p.RECIPE_ID 
WHERE c.CLASS = 'Fuel' OR c.CLASS = 'Food';
-- ORDER BY m.COMPANY; -- to check if it querries all wares, even when it is the same company ones

-- 2.6. Get all the companies in alphabetical order that producing at least 2 different wares from the same
-- category.
SELECT DISTINCT m1.COMPANY FROM MANUFACTURER m1 
INNER JOIN MANUFACTURER m2 ON m1.COMPANY = m2.COMPANY AND m1.RECIPE_ID != m2.RECIPE_ID 
INNER JOIN PRODUCT p1 ON p1.RECIPE_ID = m1.RECIPE_ID
INNER JOIN PRODUCT p2 ON p2.RECIPE_ID = m2.RECIPE_ID AND p2.WARE != p1.WARE
INNER JOIN CATEGORY c1 ON c1.WARE = p1.WARE
INNER JOIN CATEGORY c2 ON c2.WARE = p2.WARE 
WHERE c1.CLASS = c2.CLASS ORDER BY m1.COMPANY ASC;

-- 2.7. Get all the unique wares in alphabetical order that can be produced using nothing besides wares in
-- Mineral category.
SELECT DISTINCT p.WARE
FROM PRODUCT p
LEFT JOIN MATERIAL mat ON mat.RECIPE_ID = p.RECIPE_ID 
LEFT JOIN CATEGORY cat ON cat.WARE = mat.WARE AND cat.CLASS != 'Mineral'  
WHERE cat.CLASS is NULL ORDER BY p.WARE ASC;

-- 2.8. Get all the unique companies in alphabetical order implementing production chains. The production
-- chain is at least two subsequent recipes where the first recipe producing ware that is in use as material in
-- the second one. Example of such chain in terms of wares is Grain->Meat cow->Meat.
SELECT DISTINCT man1.COMPANY
FROM MATERIAL mat1 -- mat1 -> prod1 = mat2 -> prod2
INNER JOIN PRODUCT prod1 ON prod1.RECIPE_ID = mat1.RECIPE_ID 
INNER JOIN MANUFACTURER man1 ON man1.RECIPE_ID = prod1.RECIPE_ID
INNER JOIN MATERIAL mat2 ON mat2.WARE = prod1.WARE 
INNER JOIN PRODUCT prod2 ON prod2.RECIPE_ID = mat2.RECIPE_ID
INNER JOIN MANUFACTURER man2 ON man2.COMPANY = man1.COMPANY
ORDER BY man1.COMPANY ASC;

-- 2.9. Modify the query from the previous task to show also the production chain in terms of wares (3 of them) 
-- with additional sorting by middle one (that both a material and a product for the given company).
SELECT DISTINCT man1.COMPANY, mat1.WARE, mat2.WARE, prod2.WARE
FROM MATERIAL mat1 -- mat1 -> prod1 = mat2 -> prod2
INNER JOIN PRODUCT prod1 ON prod1.RECIPE_ID = mat1.RECIPE_ID 
INNER JOIN MANUFACTURER man1 ON man1.RECIPE_ID = prod1.RECIPE_ID
INNER JOIN MATERIAL mat2 ON mat2.WARE = prod1.WARE 
INNER JOIN PRODUCT prod2 ON prod2.RECIPE_ID = mat2.RECIPE_ID
INNER JOIN MANUFACTURER man2 ON man2.COMPANY = man1.COMPANY ORDER BY mat2.WARE ASC;
