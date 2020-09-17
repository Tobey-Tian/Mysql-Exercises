use final_project;

#find the frequency households go shopping

set @row_number=0;


select (@row_number:=@row_number+1) as rownum, str_to_date(TC_date,'%Y-%m-%d') AS TC_DATE1,hh_id from trips;

 
set @row_number2=1;

select (@row_number2:=@row_number2+1) as rownum2, str_to_date(TC_date,'%Y-%m-%d') as TC_DATE2,hh_id from  trips;


drop table if exists diff;
create temporary table diff
select datediff(B.TC_DATE2,A.TC_DATE1) AS DIFF, B.* from (select (@row_number:=@row_number+1) as rownum, str_to_date(TC_date,'%Y-%m-%d') AS TC_DATE1,hh_id from trips) A 
inner join
(select (@row_number2:=@row_number2+1) as rownum2, str_to_date(TC_date,'%Y-%m-%d') as TC_DATE2,trips.* from  trips) 
B on A.rownum=B.rownum2 and A.hh_id=b.hh_id ;

#find households who do not shop over 3 months
select  count(a.max_diff),hh_id from   (select max(abs(DIFF)) as max_diff,hh_id from diff group by hh_id) a where a.max_diff>90;

#find households who shop within 1 month
select  count(a.max_diff),hh_id from   (select max(abs(DIFF)) as max_diff,hh_id from diff group by hh_id) a where a.max_diff<=30;
 

 
#question 2 
drop table if exists loyalty;
create temporary table loyalty
select count(a.TC_retailer_code) as different_purchase,a.* from (select sum(TC_total_spent) as spent,c.* from 
(select datediff(TC_DATE2,TC_DATE1) AS DIFF, B.* from (select (@row_number:=@row_number+1) as rownum, str_to_date(TC_date,'%Y-%m-%d') AS TC_DATE1,hh_id from trips) A inner join
(select (@row_number2:=@row_number2+1) as rownum2, str_to_date(TC_date,'%Y-%m-%d') as TC_DATE2,trips.* from trips) B on A.rownum=B.rownum2 and a.hh_id=b.hh_id) c
 where abs(diff)<=30 group by hh_id,TC_retailer_code ) a left join
(select avg(TC_total_spent) as mean_spent, d.* from 
(select datediff(TC_DATE2,TC_DATE1) AS DIFF, B.* from (select (@row_number:=@row_number+1) as rownum, str_to_date(TC_date,'%Y-%m-%d') AS TC_DATE1,hh_id from trips) A inner join
(select (@row_number2:=@row_number2+1) as rownum2, str_to_date(TC_date,'%Y-%m-%d') as TC_DATE2,trips.* from trips) B on A.rownum=B.rownum2 and a.hh_id=b.hh_id) d
 where abs(diff)<=30 group by hh_id) b  on a.hh_id=b.hh_id where a.spent<0.8*b.mean_spent group by hh_id;


#how many households spend 80% on single retailer
select count(*) from loyalty where different_purchase<=1;

#how many households spend on 2 retailers
select * from loyalty where different_purchase<=2;

#find the distribution of households spending on 2 retailers among the country
select count(a.hh_state),a.hh_state from (select households.*,loyalty.different_purchase  from households inner join loyalty on households.hh_id=loyalty.hh_id where different_purchase<=2) a
group by a.hh_state;

#find the income distribution of household 
select count(a.hh_income),a.hh_income from
(select households.*,loyalty.different_purchase  from households inner join loyalty on households.hh_id=loyalty.hh_id where different_purchase<=2) a
group by a.hh_income;

#find which retailers have more loyal households
select count(TC_retailer_code) as shooping, TC_retailer_code from loyalty where different_purchase<=2 group by TC_retailer_code order by shooping desc;
