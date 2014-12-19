(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s: string, sl: string list) =
    case sl of
	[] =>NONE
      | hd1::tl1  => case same_string(s,hd1) of 
			  true => SOME(tl1) 
		       | false  => case all_except_option(s,tl1) of
					NONE=>NONE
				      | SOME z =>SOME(hd1::z) 

fun get_substitutions1(sll: (string list) list, s: string) =
    case sll of
	[] =>[]
	   | hd1::tl1 => let val gs = get_substitutions1(tl1,s) 
			 in
			     case all_except_option(s,hd1) of
			     NONE => gs
			      | SOME x => x@gs
			 end 

fun get_substitutions2(sll: (string list) list, s: string) =
let fun aux(sll,s,acc)=
    case sll of
	[] => acc
	| hd1::tl1 => case all_except_option(s,hd1) of
			     NONE => aux(tl1,s,acc)  
			      | SOME x => aux(tl1,s,acc@x)	
in 
    aux(sll,s,[])
end

fun similar_names(sll: string list list, r:{first:string,middle:string,last:string}) =
let fun aux(sl) =
    case sl  of
	 [] => []
       | hd1::tl1 => { first=hd1, middle =  #middle r, last = #last r }::aux(tl1)  
in
    case get_substitutions2(sll, #first r) of
         [] =>r::[]
         | x  => r::aux(x) 
end		     
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove 

(* put your solutions for problem 2 here *)(*Homework 2*)
fun card_color(s:suit,r:rank) =
    case s of
	Clubs => Black
      | Diamonds  => Red
      | Hearts => Red
      | Spades => Black   

fun card_value(s:suit,r:rank) =
    case r of
	Num (i) => i
      | Ace  => 11
      | _ => 10
     
fun remove_card(cs: card list, c:card, e) =
    case cs of
	[] => []
     | hd1::tl1 => case hd1=c of
		      true => tl1
		     |false =>case tl1 of
				    [] =>raise e
                                   |x => hd1::remove_card(x,c,e) 


 			          
fun all_same_color(cl:card list)=
case cl of
    [] =>true
    | _::[] =>true
    | head::(neck::rest) => card_color(head)=card_color(neck) andalso all_same_color(neck::rest)				

fun sum_cards(cl:card list) =
let fun f(xs,acc)=
	case xs of
	    [] => acc
	    | hd1::tl1 => f(tl1,card_value(hd1)+acc)
in
    f(cl,0)
end


fun score(cl:card list,goal:int) =
let fun preliminary_score(cl:card list, goal:int) =
	let val s = sum_cards(cl) 
	in
	    if s>goal then 3*(s-goal)
		      else goal-s
	end
in
case all_same_color(cl) of
    true => preliminary_score(cl,goal) div 2
    |false =>  preliminary_score(cl,goal) 
end

fun officiate(cl:card list, ml: move list, goal:int) = 
let fun held_cards(cl:card list, ml:move list, goal:int,hl:card list) =
	case ml of
	    []=> hl
	  | hd1::tl1 => case hd1 of
			    Draw => (case cl of 
					[] => hl
				      | hd2::tl2 => held_cards(tl2,tl1,goal,hd2::hl))
			| Discard c  => held_cards(cl,tl1,goal,remove_card(hl,c,IllegalMove))		   
in
 score(held_cards(cl,ml,goal,[]),goal)
end
