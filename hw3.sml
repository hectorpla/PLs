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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


(* should use value binding instead of function wrapper *)
val only_capitals = List.filter (fn s => Char.isUpper (String.sub(s, 0)))

fun longest_string comp = foldl (fn (x,y) => if comp(x,y) then x else y) ""

fun comp1(x,y) = String.size(x) > String.size(y)		 
val longest_string1 = longest_string comp1

fun comp2(x,y) = String.size(x) >= String.size(y)		 
val longest_string2 = longest_string comp2


fun longest_string_helper ncomp  = 
  foldl (fn (x,y) => if ncomp(String.size(x), String.size(y)) then x else y) "" 

val longest_string3 = longest_string_helper Int.>

val longest_string4  = longest_string_helper Int.>=

val longest_capitalized = (longest_string1 o only_capitals)
  
val rev_string = (implode o rev o explode)

					       
fun first_answer f lst =
  case lst of
      [] => raise NoAnswer
    | hd::tl => case f hd of
			  SOME v => v
		      | None => first_answer f tl


(* MISunderstood: if NONE for ANY element then NONE *)
fun all_answers f lst =
  let
      val all = foldl (fn (x, acc) => case f x of
				      NONE => acc
				    | SOME l => l @ acc) [] lst
  in
      if lst = [] then SOME []
      else if all = [] then NONE
      else SOME all
  end
			 
fun all_answers_sample f xs =
    let fun loop (acc,xs) =
          case xs of
	      [] => SOME acc
	    | x::xs' => case f x of 
			    NONE => NONE
              		  | SOME y => loop((y @ acc), xs')
    in loop ([],xs) end

      
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
					
val count_wildcards = g (fn() => 1) (fn str => 0)

val count_wild_and_variable_lengths = g (fn() => 1) (fn str => 1)

fun count_some_var (str, p) =
  g (fn() => 0) (fn another => if another = str then 1 else 0) p


fun check_pat p =
  let
      fun all_variables p =
	case p of
	    Variable s => [s]
	 | TupleP ps => List.foldl (fn (p, acc) => all_variables(p) @ acc) [] ps
	 | ConstructorP(_, p) => all_variables p
	 | _ => [] 
					       
      fun check_helper ss =
	case ss of
	    [] => true
	  | x::xs => not (List.exists (fn str => str = x) xs) andalso check_helper xs
  in
      (check_helper o all_variables) p
  end

(* Failpoints: variable match; length of vs and ps should match *)
fun match (v, p) =
  case (v, p) of
      (v, Variable s) => SOME [(s, v)]
     |(Tuple vs, TupleP ps) => all_answers match (ListPair.zip(vs, ps))
     |(Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2 then match(v,p) else NONE
     |(_, Wildcard)  => SOME []
     |(Unit, UnitP)  => SOME []
     |(Const v1, ConstP v2)  => if v1 = v2 then SOME [] else NONE
     | _ => NONE

fun match_sample (valu,pat) =
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
		
fun first_match v ps =
  SOME (first_answer match (List.map (fn p => (v, p)) ps))
  handle NoAnswer => NONE
