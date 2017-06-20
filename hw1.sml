fun is_older (lhs:(int*int*int), rhs:(int*int*int))  =
  if #1 lhs <> #1 rhs then #1 lhs < #1 rhs else 
  if #2 lhs <> #2 rhs then #2 lhs < #2 rhs else
  #3 lhs < #3 rhs

	      
fun number_in_month (dates:(int*int*int) list, month:int) =
  if null dates then 0 else
  if #2 (hd dates) = month then 1 + number_in_month (tl dates, month) else
  number_in_month (tl dates, month)
		 
fun number_in_months (dates:(int*int*int) list, months:int list) =
  if null months then 0 else
  number_in_month (dates, hd months) + number_in_months (dates, tl months)
		  
(* keep original order  *)	      
fun dates_in_month (dates:(int*int*int) list, month:int) =
  if null dates then [] else
  if #2 (hd dates) = month then
      (hd dates)::dates_in_month (tl dates, month) else
  dates_in_month (tl dates, month)
	
fun dates_in_months (dates:(int*int*int) list, months:int list) =
  if null months then [] else
  dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)



fun get_nth (lst, n:int) =
  if n = 1 then hd lst else
  get_nth (tl lst, n-1)


fun date_to_string (date:(int*int*int)) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", " December"]
  in
      get_nth (months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
  end							   
	  
(* bugs *)
fun number_before_reaching_sum (sum:int, lst:int list) =
  let
      fun aux (lst:int list) (acc:int) (cur:int) =
      	if acc + hd lst >= sum then cur else
      	aux (tl lst) (acc + hd lst) (cur + 1)
  in
      aux lst 0 0
  end

(* sample sol
fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= hd lst
    then 0
    else 1 + number_before_reaching_sum(sum - hd lst, tl lst)
*)

fun what_month (day:int) =
  let
      val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in		      
      1 + number_before_reaching_sum (day, months)
  end				


fun month_range (day1:int, day2:int) =
  if day1 > day2 then [] else
  what_month day1 :: month_range (day1 + 1, day2)

fun bad_oldest (dates:(int*int*int) list) =
  if null dates then NONE else
  if null (tl dates) then SOME (hd dates) else
  if is_older (hd dates, valOf (bad_oldest (tl dates))) then SOME (hd dates) else
  bad_oldest (tl dates)


fun oldest (dates:(int*int*int) list) =
  if null dates then NONE else
  if null (tl dates) then SOME (hd dates) else
  let
      val acc = valOf (oldest (tl dates))
  in
      if is_older (hd dates, acc) then SOME (hd dates) else
      SOME acc
  end
       



(* challenging part *)
      
(* O(n^2), otherwise find any of [1..12] in the list, which is O(n)  *)
fun remove_duplicate lst:int list =
  let
      fun is_in (elem:int) lst =
    	if null lst then false else
    	if elem = hd lst then true else
    	is_in elem (tl lst)
  in	      
      if null lst then [] else
      if not (is_in (hd lst) (tl lst)) then hd lst::remove_duplicate (tl lst) else
      remove_duplicate (tl lst)
  end		   
  
      
fun number_in_months_challenge (dates:(int*int*int) list, months:int list) =
  number_in_months (dates, remove_duplicate (months))

fun dates_in_months_challenge (dates:(int*int*int) list, months:int list) =
  dates_in_months (dates, remove_duplicate (months))

fun reasonable_date (date:int*int*int) =
  let
      val year = #1 date;
      val month = #2 date;
      val day = #3 date;
      val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
      fun is_leap_year (year:int) =
	if year mod 100 = 0 then false else
	if year mod 4 = 0 orelse year mod 400 = 0 then true else false
  in
      if year < 1 orelse month < 1 orelse month > 12 orelse day < 1 then false else
      if month = 2 andalso is_leap_year year andalso day < 30 then true else
      day <= get_nth (days_of_months, month)
  end
