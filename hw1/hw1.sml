fun is_older (date1: int*int*int, date2: int*int*int) =
    if #1 date2 >  #1 date1 
    then true
    else 
	if (#2 date2 > #2 date1) andalso (#1 date2 = #1 date1)  
	then true
	else 
            if #3 date2> #3 date1 andalso  #1 date2 = #1 date1 andalso #2 date2 = #2 date1 
            then true
            else false

fun number_in_month (date_list: (int*int*int) list, month:int) =
    if null date_list 
    then 0
    else 
        let val tl_ans =number_in_month(tl(date_list),month) 
	in if #2(hd(date_list)) = month 
	   then  tl_ans+ 1
           else tl_ans
        end

fun number_in_months (date_list: (int*int*int) list, months:int list) =
    if null months 
    then 0
    else  number_in_months(date_list, tl(months))+number_in_month(date_list,hd(months))

fun dates_in_month (date_list: (int*int*int) list, month:int) =
    if null date_list 
    then [ ]
    else 
        let val tl_ans =dates_in_month(tl(date_list),month) 
	in if #2(hd(date_list)) = month 
	   then hd(date_list):: tl_ans
           else tl_ans
        end

fun dates_in_months (date_list: (int*int*int) list, months:int list) =
    if null months 
    then [ ]
    else 
    dates_in_month(date_list,hd(months))@dates_in_months(date_list,tl(months))

fun get_nth (slist : string list, n : int)=
    if n = 1 
    then hd(slist)
    else get_nth(tl(slist),n-1)

fun date_to_string (date: int*int*int)=
    let val months = ["January","February","March"," April","May", "June", "July","August","September","October", "November","December"]
    in
	get_nth(months,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, nlist: int list)=
    if sum <= hd(nlist) 
    then 0
    else number_before_reaching_sum(sum - hd(nlist), tl(nlist))+1

fun what_month (day : int)=
    let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day,months)+1
    end

fun month_range(day1: int, day2: int)=
    if day1 > day2 
    then []
    else what_month(day1)::month_range(day1+1,day2)

fun oldest(date_list: (int*int*int) list)=
    if null date_list
    then NONE
    else
	let val tl_ans = oldest(tl date_list)
        in if isSome tl_ans andalso is_older(valOf tl_ans,hd date_list)
           then tl_ans
	   else SOME (hd date_list)
        end
