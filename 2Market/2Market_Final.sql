/*
 Query the joined table to gather insights based on questions that may help identify the most effective adcvertising channel:
Which social media platform (Twitter, Instagram, or Facebook) is the most effective method of adcvertising in each country? 
(In this case, consider the total number of leadc conversions as a measure of effectiveness).
Which social media platform is the most effective method of adcvertising based on marital status?
(In this case, consider the total number of leadc conversions as a measure of effectiveness).
Which social media platform(s) seem(s) to be the most effective per country? 
(In this case, assume that purchases were in some way influenced by leadc conversions from a campaign).
Hint: You’ll want to generate the amount spent per product type per country and include a total of the adcs for each of the social media platforms. 
Then, review the output to determine whether there is anything interesting related to the amount spent per product in each country and which social media platforms were used for adcvertising.
*/
CREATE TABLE campaign AS
SELECT 
mdc."ID",
mdc."Age",
mdc."Marital_Status", 
mdc."Country",
mdc."Alcohol" ,
mdc."Vegetables" ,
mdc."Meat",
mdc."Fish",
mdc."Chocolates",
mdc."Commodities",
mdc."Response" ,
mdc."Count_success" ,
adc."Bulkmail_ad" ,
adc."Twitter_ad" ,
adc."Instagram_ad" ,
adc."Facebook_ad" ,
adc."Brochure_ad" 
FROM public.market_data_clean mdc 
INNER JOIN public.ad_data_clean adc 
ON mdc."ID" =adc."ID" 

/*
1) Which social media platform (Twitter, Instagram, or Facebook) is the most effective
method of adcvertising in each country? 
(consider the total number of leadc conversions as a measure of effectiveness)
*/
SELECT 
"Country",
COUNT ( CASE WHEN "Bulkmail_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Bulkmail_Positive,
COUNT ( CASE WHEN "Brochure_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Brochure_Positive,
COUNT ( CASE WHEN "Twitter_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Twitter_Positive,
COUNT ( CASE WHEN "Instagram_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Insta_Positive,
COUNT ( CASE WHEN "Facebook_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Facebook_Positive
FROM campaign 
GROUP BY "Country" 

/*
2) Which social media platform is the most effective method of adcvertising 
 based on marital status?
(consider the total number of leadc conversions as a measure of effectiveness).
 */
SELECT 
"Marital_Status" ,
COUNT ( CASE WHEN "Twitter_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Twitter_Positive,
COUNT ( CASE WHEN "Instagram_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Insta_Positive,
COUNT ( CASE WHEN "Facebook_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Facebook_Positive
FROM campaign 
GROUP BY "Marital_Status"  

/*
3) Which social media platform(s) seem(s) to be the most effective per country? 
( assume that purchases were in some way influenced by leadc conversions from a campaign).
Hint: You’ll want to generate the amount spent per product type per country and include a total of the adcs for each of the social media platforms. 
Then, review the output to determine whether there is anything interesting related to the amount spent per product in each country and which social media platforms were used for adcvertising.
 */
SELECT 
"Country",
SUM (ca."Alcohol") AS Total_Alcohol,
SUM (ca."Vegetables") AS Total_Vegetables,
SUM (ca."Meat") AS Total_Meat,
SUM (ca."Fish") AS Total_Fish,
SUM (ca."Chocolates") AS Total_Chocolates,
SUM (ca."Commodities") AS Total_Commodities,
COUNT ( CASE WHEN "Twitter_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Twitter_Positive,
COUNT ( CASE WHEN "Instagram_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Insta_Positive,
COUNT ( CASE WHEN "Facebook_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Facebook_Positive
FROM campaign ca
GROUP BY "Country" 
ORDER BY 
	Total_Alcohol DESC, 
	Total_Vegetables DESC, 
	Total_Meat DESC, 
	Total_Fish DESC, 
	Total_Chocolates DESC, 
	Total_Commodities  DESC

	
	/*
	Which campaign adcd was more successful (ie social media and hard copies campaign)
	*/
SELECT 
"Country",
COUNT ( CASE WHEN "Bulkmail_adc"=1 AND "Count_success"=1 THEN 1 END ) AS Bulkmail_Positive,
COUNT ( CASE WHEN "Brochure_adc"=1 AND "Count_success"=1 THEN 1 END ) AS Brochure_Positive,
COUNT ( CASE WHEN "Twitter_adc"=1 AND "Count_success"=1 THEN 1 END ) AS Twitter_Positive,
COUNT ( CASE WHEN "Instagram_adc"=1 AND "Count_success"=1 THEN 1 END ) AS Insta_Positive,
COUNT ( CASE WHEN "Facebook_adc"=1 AND "Count_success"=1 THEN 1 END ) AS Facebook_Positive
FROM campaign 
GROUP BY "Country" 

SELECT 
"Country",
COUNT ( "Bulkmail_ad") AS All_Bulkmail,
COUNT ( "Brochure_ad") AS All_Brochure,
COUNT ( "Twitter_ad") AS All_Twitter,
COUNT ( "Instagram_ad") AS All_Insta,
COUNT ( "Facebook_ad") AS All_Facebook,
COUNT ( CASE WHEN "Bulkmail_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Bulkmail_Positive,
COUNT ( CASE WHEN "Brochure_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Brochure_Positive,
COUNT ( CASE WHEN "Twitter_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Twitter_Positive,
COUNT ( CASE WHEN "Instagram_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Insta_Positive,
COUNT ( CASE WHEN "Facebook_ad"=1 AND "Count_success"=1 THEN 1 END ) AS Facebook_Positive
FROM campaign 
GROUP BY "Country" 

	