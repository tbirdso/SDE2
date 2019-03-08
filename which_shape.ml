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
	else let less = n - 1 in
		let retList = gen_string(less,what) in
		List.append retList [what];;

(* gen_square(n)
 * receives	n = side length
 *
 * returns	string representing square
 *)

let gen_square = function(n) ->
	let u = gen_string(n,'u') in
	let r = gen_string(n,'r') in
	let d = gen_string(n,'d') in
	let l = gen_string(n,'l') in
	List.concat [u;r;d;l];;

(* gen_rect(n,m)
 * receives	n = "u" and "d" side length
 *		m = "r" and "l" side length
 *
 * returns	string representing rectangle
 *)

let gen_rect = function(n,m) ->
	let u = gen_string(n,'u') in
	let r = gen_string(m,'r') in
	let d = gen_string(n,'d') in
	let l = gen_string(m,'l') in
	List.concat [u;r;d;l];;

(* countups(alist,0)
 * receives	alist = list to parse for 'u'
 * 		0
 *
 * returns	total number of 'u' in list
 *)

let rec countups= function (alist,0) ->
	let len = List.length alist in
	if len = 0
	then
		0
	else
		let tail = List.tl alist in
		let tailcount = countups(tail,0) in
		let head = List.hd alist in
		if head = 'u'
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

let rec consec_counts = function (alist,count,what) ->
	let len = List.length alist in
	if len = 0
	then
		([],count)
	else
		let head = List.hd alist in
		let tail = List.tl alist in
		if head = what
		then
			let cp1 = count + 1 in
			consec_counts(tail,cp1,what)
		else
			if count = 0
			then
			consec_counts(tail,0,what)
			else
			(alist,count)
	;;

(* sq(alist)
 * receives	alist = list to be parsed
 * 
 * returns	1 if string represents a square, 0 otherwise
 *
 * note: allows leftovers and does not require sides are same length
 *)

let first_el = function(alist,what) -> 
	let len = List.length alist in
	if len > 0
	then
		let head = List.hd alist in
		if head = what
		then
			1
		else
			0
	else
		0
	;;

let sq = function (alist) ->
	let head = List.hd alist in
	if head = "u"
	then
		let (afterU,ulen) = consec_counts(alist,0,"u") in
		let (afterR,rlen) = consec_counts(afterU,0,"r") in
		let (afterD,dlen) = consec_counts(afterR,0,"d") in
		let (afterL,llen) = consec_counts(afterD,0,"l") in

		let validR = first_el(afterU,"r") in
		let validD = first_el(afterR,"d") in
		let validL = first_el(afterD,"l") in
		
		if validR = 1 && validD = 1 && validL = 1
		then
			1
		else
			0
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
	let head = List.hd alist in
	if head = "u"
	then
		let (afterU,ulen) = consec_counts(alist,0,"u") in
		let (afterR,rlen) = consec_counts(afterU,0,"r") in
		let (afterD,dlen) = consec_counts(afterR,0,"d") in
		let (afterL,llen) = consec_counts(afterD,0,"l") in

		let validR = first_el(afterU,"r") in
		let validD = first_el(afterR,"d") in
		let validL = first_el(afterD,"l") in
		let xlen = List.length afterL in
		
		if validR = 1 && validD = 1 && validL = 1 && xlen = 0
		then
			1
		else
			0
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
	let head = List.hd alist in
	let (afterU,ulen) = consec_counts(alist,0,"u") in
	let (afterR,rlen) = consec_counts(afterU,0,"r") in
	let (afterD,dlen) = consec_counts(afterR,0,"d") in
	let (afterL,llen) = consec_counts(afterD,0,"l") in

	let validU = first_el(alist,"u") in
	let validR = first_el(afterU,"r") in
	let validD = first_el(afterR,"d") in
	let validL = first_el(afterD,"l") in
	let xlen = List.length afterL in
	
	if validR = 1 && validD = 1 && validL = 1 &&
		ulen = rlen && rlen = dlen && dlen = llen && xlen = 0
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
	let head = List.hd alist in

	let (m30list,ulen) = consec_counts(alist,0,"u") in
	let (p240list,m30len) = consec_counts(m30list,0,"m30") in
	let (rest,p240len) = consec_counts(p240list,0,"p240") in

	let validu = first_el(alist,"u") in
	let validm30 = first_el(m30list,"m30") in
	let validp240 = first_el(p240list,"p240") in
	let xlen = List.length rest in

	if validu = 1 && validm30 = 1 && validp240 = 1 &&
		ulen = m30len && m30len = p240len && xlen = 0
	then
		1
	else
		0
	;;










