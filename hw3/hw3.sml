(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of
 typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals cl = List.filter (fn x => Char.isUpper (String.sub(x,0))) cl

fun longest_string1 sl = List.foldl (fn(x,acc) =>if String.size x > String.size acc then x else acc) "" sl 

fun longest_string2 sl = List.foldl (fn(x,acc) =>if String.size x >= String.size acc then x else acc) "" sl 

fun longest_string_helper g sl  =
List.foldl (fn(x,acc) => if g(String.size x, String.size acc) then x else acc) "" sl   

val longest_string3   =  longest_string_helper (fn (x,acc) => x > acc)

val longest_string4   =  longest_string_helper (fn (x,acc)  => x >= acc)

val longest_capitalized  = longest_string1  o  only_capitals

fun rev_string str = String.implode (List.rev (String.explode str))

fun first_answer f l = 
    case l of
	[] => raise NoAnswer
     |  hd1::tl1 => case f hd1 of   
			 NONE => first_answer f tl1
		      | SOME v => v 

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


fun all_answers f xs =
    let fun loop (acc,xs) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of 
			      NONE => NONE
			    | SOME y => loop((y @ acc), xs')
    in loop ([],xs) end

val count_wildcards = g (fn () => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
fun count_some_var (x,p) = g (fn () => 0) (fn s => if s = x then 1 else 0) p

fun check_pat pat = 
    let fun get_vars pat =
	    case pat of
		Variable s => [s]
	      | TupleP ps  => List.foldl (fn (p,vs) => get_vars p @ vs) [] ps
	      | ConstructorP(_,p) => get_vars p
	      | _          => []
	fun unique xs = 
	    case xs of
		[]     => true
	      | x::xs' => (not (List.exists (fn y => y=x) xs'))
			  andalso unique xs'
    in 
	unique (get_vars pat)
    end

fun match (valu,pat) =
    case (valu,pat) of
	(_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				  then all_answers match (ListPair.zip(vs,ps))
				  else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
						   then match(v,p)
						   else NONE
      | _ => NONE

fun first_match valu patlst =
    SOME (first_answer (fn pat => match (valu,pat)) patlst)
    handle NoAnswer => NONE




 
