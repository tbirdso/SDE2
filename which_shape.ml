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

let rec gen_string n what =
	if n = 1
	then [what]
	else let less = n - 1 in
		let retList = gen_string less what in
		List.append retList [what];;

(* gen_square(n)
 * receives	n = side length
 *
 * returns	string representing square
 *)

let gen_square n = 
	let u = gen_string n 'u' in
	let r = gen_string n 'r' in
	let d = gen_string n 'd' in
	let l = gen_string n 'l' in
	List.concat [u;r;d;l];;

(* gen_rect(n,m)
 * receives	n = "u" and "d" side length
 *		m = "r" and "l" side length
 *
 * returns	string representing rectangle
 *)

let gen_rect n m = 
	let u = gen_string n 'u' in
	let r = gen_string m 'r' in
	let d = gen_string n 'd' in
	let l = gen_string m 'l' in
	List.concat [u;r;d;l];;

(* countups(alist,0)
 * receives	alist = list to parse for 'u'
 * 		0
 *
 * returns	total number of 'u' in list
 *)

let rec countups alist 0 = 
	let len = List.length alist in
	if len = 0
	then
		0
	else
		let tail = List.tl alist in
		let tailcount = countups tail 0 in
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
 *)

let rec consec_counts alist count what =
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
			consec_counts tail cp1 what
		else
			if count = 0
			then
			consec_counts tail 0 what
			else
			(alist,count)
	;;


