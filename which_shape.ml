(* Programmer:	Tom Birdsong
 * Project:	SDE2	
 * Course:	ECE3520
 * Semester:	Spring 2019
 *)

(* gen_string(n,what)
 * receives	n = int string length
 *		what = element of unknown type to fill array
 *
 * returns	string with n elements equal to what
 *)

let rec gen_string = function(n,what) ->
	if n = 1
	then [what]
	else 
		n - 1 |>
		fun less ->  gen_string(less,what) |>
		fun retList -> retList@[what];;

(* gen_square(n)
 * receives	n = side length
 *
 * returns	string representing square
 *)

let gen_square = function(n) ->
	gen_string(n,'u') |>
	fun u -> u@gen_string(n,'r') |>
	fun d -> d@gen_string(n,'d') |>
	fun l -> l@ gen_string(n,'l');;

(* gen_rect(n,m)
 * receives	n = "u" and "d" side length
 *		m = "r" and "l" side length
 *
 * returns	string representing rectangle
 *)

let gen_rect = function(n,m) ->
	gen_string(n,'u') |>
	fun u -> u@gen_string(m,'r') |>
	fun r -> r@gen_string(n,'d') |>
	fun l -> l@gen_string(m,'l');;

(* countups(alist,0)
 * receives	alist = list to parse for 'u'
 * 		0
 *
 * returns	total number of 'u' in list
 *)

let rec countups= function 
	([],_) -> 0
	| (head::tail,_) ->
		countups(tail,0) |>
		fun (tailcount) ->
			if head = "u"
			then
				tailcount + 1
			else
				tailcount
	;;

(* consec_counts(alist,count,what)
 * receives	alist = list to parse for what
 * 		count = number of consecutive so far (initialized at 0)
 * 		what = thing to be counted in list
 *
 * returns	tuple (rest,num) where
 *			rest = remainder of list after consecutive whats
 *			num = number of consecutive whats
 *)

let rec consec_counts = function 
	([],count,_) -> ([],count)
	| (head::tail,count,what) ->
		if head = what
		then
			count + 1 |>
			fun cp1 ->
			consec_counts(tail,cp1,what)
		else
			if count = 0
			then
				consec_counts(tail,0,what)
			else
				([head]@tail,count)
	;;

(* first_el(alist,what) 
 * receives	alist = list to check head
		what = expected head value
 *
 * returns	1 if head is as expected, 0 otherwise
 *)
let first_el = function
	([],_) -> 0
	| (head::tail,what) -> 
		if head = what
		then
			1
		else
			0
	;;

(* sq(alist)
 * receives	alist = list to be parsed
 * 
 * returns	1 if string represents a square, 0 otherwise
 *
 * note: allows leftovers and does not require sides are same length
 *)

let sq = function (alist) ->
	consec_counts(alist,0,"u") |>
	fun (afterU,uLen) ->
		(afterU,uLen,consec_counts(afterU,0,"r")) |>
	fun (afterU,uLen,(afterR,rlen)) ->
		(afterU,uLen,afterR,rlen,consec_counts(afterR,0,"d")) |>
	fun(afterU,uLen,afterR,rlen,(afterD,dlen)) ->

	if first_el(alist,"u") = 1 &&
		first_el(afterU,"r") = 1 &&
		first_el(afterR,"d") = 1 &&
		first_el(afterD,"l") = 1
	then
		1
	else
		0
	;;

(* sq_all(alist)
 * receives	alist = list to be parsed
 *
 * returns	1 if string represents a square, 0 otherwise
 *
 * note: does not check for equality of side lengths but does require entire string
 *	to be used
 *)

let sq_all = function (alist) ->
	consec_counts(alist,0,"u") |>
	fun (afterU,_) ->
		(afterU,consec_counts(afterU,0,"r")) |>
	fun (afterU,(afterR,_)) ->
		(afterU,afterR,consec_counts(afterR,0,"d")) |>
	fun (afterU,afterR,(afterD,_)) ->
		(afterU,afterR,afterD,consec_counts(afterD,0,"l")) |>
	fun (afterU,afterR,afterD,(afterL,_)) ->

	if	first_el(alist,"u") = 1 &&
		first_el(afterU,"r") = 1 &&
		first_el(afterR,"d") = 1 &&
		first_el(afterD,"l") = 1 &&
		List.length afterL = 0
	then
		1
	else
		0
	;;



(* sqA(alist)
 * receives	alist = list to be parsed
 * 
 * returns	1 if string represents a square, 0 otherwise
 *
 * note: does not check for equality of side lengths but does require entire string
 *	to be used
 *)

let sqA = function (alist) ->

	consec_counts(alist,0,"u") |>
	fun (afterU,ulen) ->
		(afterU,ulen,consec_counts(afterU,0,"r")) |>
	fun (afterU,ulen,(afterR,rlen)) ->
		(afterU,ulen,afterR,rlen,consec_counts(afterR,0,"d")) |>
	fun (afterU,ulen,afterR,rlen,(afterD,dlen)) ->
		(afterU,ulen,afterR,rlen,afterD,dlen,consec_counts(afterD,0,"l")) |>
	fun (afterU,ulen,afterR,rlen,afterD,dlen,(afterL,llen)) ->

	if	first_el(alist,"u") = 1 &&
		first_el(afterU,"r") = 1 &&
		first_el(afterR,"d") = 1 &&
		first_el(afterD,"l") = 1 &&
		List.length afterL = 0 &&
		ulen = rlen &&
		rlen = dlen &&
		dlen = llen
	then
		1
	else
		0
	;;	

(* eqtriA(alist)
 * receives	alist = list to be parsed
 * 
 * returns	1 if list represents an equilateral triangle, 0 otherwise
 *)

let eqtriA = function (alist) ->
	consec_counts(alist,0,"u") |>
	fun (m30list,ulen) ->
		(m30list,ulen,consec_counts(m30list,0,"m30")) |>
	fun (m30list,ulen,(p240list,m30len)) ->
		(m30list,ulen,p240list,m30len,consec_counts(p240list,0,"p240")) |>
	fun (m30list,ulen,p240list,m30len,(rest,p240len)) ->
	
	if first_el(alist,"u") = 1 &&
		first_el(m30list,"m30") = 1 &&
		first_el(p240list,"p240") = 1 &&
		List.length rest = 0 &&
		ulen = m30len &&
		m30len = p240len
	then
		1
	else
		0
	;;

(* one_shift(alist)
 * receives	alist = list to be shifted
 * 
 * returns	list shifted one to the left
 *)

let rec one_shift = function 
	(h1::h2::t) ->
		[h2]@one_shift([h1]@t)
	| (h) -> h
	;;

(* all_shifts_recurs(alist,n)
 * receives	alist = previous cycle
 *		n = cycles remaining (including this one)
 *
 * returns	list of this and all following shifts
 *)

let rec all_shifts_recurs = function 
	(alist,0) ->
		[]
	| (alist,n) ->
		one_shift(alist) |>
		fun shift ->
			[shift]@all_shifts_recurs(shift,n-1)
	;;
	

(* all_shifts(alist)
 * receives	alist = list to be shifted
 *
 * returns	list of all cyclic shifts of alist, not including alist
 *)

let all_shifts = function (alist) ->
	List.length alist |>
	fun len -> all_shifts_recurs(alist,len - 1);;

(* all_cases(alist)
 * receives 	list to be cycled
 *
 * returns	list of all shifts of shift
 *)

let all_cases = function (alist) ->
	[alist]@all_shifts(alist);;

(* try_all_sqA_recurs(clist)
 * receives	clist = list of cycles to check through
 *
 * returns	1 if one of the cycles represents a square, 0 otherwise
 *)

let rec try_all_sqA_recurs = function
	([]) -> 0
	| (ch::ct) ->
		sqA(ch) |>
		fun case ->
			if case = 1
			then
				1
			else
				try_all_sqA_recurs(ct)
	;;
	
(* try_all_sqA(alist)
 * receives	alist = list to cycle through, try to find string satisfying sqA
 *
 * returns	1 if some cycle of alist represents a square, 0 otherwise
 *)

let try_all_sqA = function (alist) ->
	all_cases(alist) |>
	fun cycles -> try_all_sqA_recurs(cycles);;	


(* try_all_eqtriA_recurs(clist)
 * receives	clist = list of cycles to check through
 * 
 * returns	1 if some cycle in clist represents an equilateral triangle, 0 otherwise
 *)

let rec try_all_eqtriA_recurs = function
	([]) -> 0
	| (ch::ct) ->
		eqtriA(ch) |>
			fun tri ->
			if tri = 1
			then
				1
			else
				try_all_eqtriA_recurs(ct)
	;;

(* try_all_eqtriA(alist)
 * receives	alist = list to be parsed
 *
 * returns	1 if some cycle of alist represents an equilateral triangle, 0 otherwise
 *)

let try_all_eqtriA = function (alist) ->
	all_cases(alist) |>
	fun cycles -> try_all_eqtriA_recurs(cycles);;

