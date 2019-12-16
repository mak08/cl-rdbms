--  EEQuery
--
-- SELECT FROM Employee[org.boardarea = 'TIP' AND salary > '100.000']
-- { name,
--   address[kind='home'] { street, city},
--   salary
-- }

--  Generated SQL
--
SELECT ABC.MYNAME.PARENT_ID, ABC.MYNAME.FIRST, ABC.MYNAME.MIDDLE, ABC.MYNAME.LAST, ABC.MYADDRESS.STREET, ABC.MYADDRESS.CITY, ABC.MYEMPLOYEE.SALARY 
 		FROM ((((
							ABC.MYEMPLOYEE 
				INNER JOIN	ABC.MYEMPLOYEE$n1$ORG ON (ABC.MYEMPLOYEE.ENTITY_ID = ABC.MYEMPLOYEE$n1$ORG.SOURCE_ID))	 
				INNER JOIN  ABC.MYORGUNIT ON ((    ABC.MYEMPLOYEE$n1$ORG.TARGET_ID = ABC.MYORGUNIT.ENTITY_ID) 
					  					  	   AND ((ABC.MYORGUNIT.BOARDAREA = 'TIP') 
											   AND (ABC.MYEMPLOYEE.SALARY > '100000')))) 
				INNER JOIN  ABC.MYNAME ON (ABC.MYEMPLOYEE.NAME = ABC.MYNAME.ENTITY_ID)) 
				INNER JOIN  ABC.MYADDRESS ON ((    ABC.MYEMPLOYEE.ENTITY_ID = ABC.MYADDRESS.PARENT_ID) 
					  					  	   AND (ABC.MYADDRESS.KIND = 'home')))  
+
