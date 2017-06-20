(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


fun mem (str:string, lst:string list) =
  case lst of
      [] => false
	 | _ => if str = hd lst then true else mem (str, tl lst)		

fun all_except_option (str:string, lst:string list) =
  let fun delete lst = 
	case lst of
	    [] => []
	  | x::xs => if same_string(x, str) then delete(xs) else x::delete(xs)
  in
      let val new = delete(lst) in
	  if lst = new then NONE else SOME new
      end
  end

(* sample solution *)
fun all_except_option_sample (s,xs) =
  case xs of
      [] => NONE
    | x::xs' => if same_string(s,x)
                then SOME xs'
                else case all_except_option(s,xs') of
                         NONE => NONE
                       | SOME y => SOME(x::y)      

fun get_substitutions1 (substitutions, s) =
  case substitutions of
      [] => []
    | x::xs => case all_except_option(s, x) of
		  NONE => get_substitutions1(tl substitutions, s)
	       | SOME subs => subs @ get_substitutions1(tl substitutions, s)


fun get_substitutions2 (substitutions, s) =
  let fun aux (subs, acc) =
	case subs of
	    [] => acc
	 | x::xs => case all_except_option(s, x) of
			NONE => aux(xs, acc)
		      | SOME sub => aux(xs, acc @ sub)
  in
      aux(substitutions, [])
  end


fun similar_names (substitutions, fullname) =
  let
      val {first=fst, middle=mid, last=lst} = fullname
      val firstnames = get_substitutions2(substitutions, fst)
      fun enum_subs (subs, acc) =
	case subs of
	    [] => acc
	  | x::xs => enum_subs (xs, {first=x, middle=mid, last=lst}::acc)
  in
     fullname::enum_subs(firstnames, [])
  end

fun card_color (s, r) =
  case s of
      Clubs => Black
   | Spades  => Black
   | Diamonds => Red
   | Hearts => Red


fun card_value (s, r) =
  case r of
      Num(i) => i
    | Ace => 11
    | Jack => 10
    | Queen => 10
    | King => 10
		  
		   
fun remove_card (cs, c, e) =
  let
      fun delete (lst, acc) =
	case lst of
	    [] => rev acc
	  | hd::tl => if hd = c then (rev acc) @ tl else delete(tl, hd::acc)
  in
      let
	  val removed = delete(cs, [])
      in
      case cs of
	  [] => raise e
	| _ => if removed = cs then raise e else removed
      end
  end

(* sample solution *)
fun remove_card_sample (cs,c,e) =
    case cs of
	      [] => raise e
      | x::cs' => if x = c then cs' else x :: remove_card(cs',c,e)


fun all_same_color cs =
  case cs of
      [] => true
    | _::[] => true
    | hd::neck::tl => card_color(hd) = card_color(neck) andalso all_same_color (neck::tl)

fun sum_cards cs =
  let
      fun aux (lst, acc) =
	case lst of
	    [] => acc
	  | hd::tl => aux (tl, card_value(hd) + acc)
  in
      aux(cs, 0)
  end

fun score (cs, goal) =
  let
      val sum = sum_cards(cs);
      val samecol = all_same_color(cs)
  in
      let
	  val prelim = if sum - goal < 0 then (goal - sum) else 3 * (sum - goal)
      in
	  if samecol then prelim div 2 else prelim
      end
  end

(* sample solution *)
fun score_sample (cs,goal) = 
    let 
        val sum = sum_cards cs
    in
        (if sum >= goal then 3 * (sum - goal) else goal - sum)
	      div (if all_same_color cs then 2 else 1)
    end
								  
fun officiate_g (cs, ms, goal, score) =
  let
      fun play (cs, ms, held, scr) =
	case (cs, ms) of
	    (_, []) => score(held, goal)
	  | ([], Draw::_) => score(held, goal)
	  | (c::cs', Draw::ms') => if sum_cards(c::held) > goal
				 then score(c::held, goal)
				 else play(cs', ms', c::held, scr)
	  | (_, Discard(dc)::ms') => play(cs, ms', remove_card(held, dc, IllegalMove), scr)						     
  in
      play(cs, ms, [], 0)
  end

fun officiate (cs, ms, goal) = officiate_g(cs, ms, goal, score)
      
(* repeated sum computation *)
fun score_challenge (cs, goal) =
  let
      fun count_ace (cards, count) =
	case cards of
	    [] => 0
	  | (_, Ace)::cards' => count_ace (cards', count + 1)
	  | _::cards' => count_ace (cards', count)
  in
      let
	  val aces = count_ace(cs, 0);
	  fun max_score (aces) =
	    if aces = 0
	    then score(cs, goal)
	    else Int.min(score(cs, goal - 10 * aces), max_score(aces - 1))
      in
	  max_score(aces)
      end
  end

fun officiate_challenge (cs, ms, goal) = officiate_g(cs, ms, goal, score_challenge)

fun careful_player (cs, goal) =
  let
      fun can_reach_goal_by_discard(cards) =
	let
	    val sum = sum_cards(cards)
	in
	    case cards of
		[] => NONE
	      | c::cards' => if sum - card_value(c) = goal
			     then SOME c
			     else can_reach_goal_by_discard(cards')
	end
  in
      let
	  fun get_moves (cards, held, ms) =
	    case cards of
		[] => rev ms
	      | c::cards' => if goal - sum_cards(held) > 10
			     then get_moves(cards', c::held, Draw::ms)
			     else
				 case can_reach_goal_by_discard(c::held) of
				     SOME c => rev (Discard(c)::ms)
				   | NONE => get_moves(cards', c::held, Draw::ms)
      in
	  get_moves(cs, [], [])
      end
  end		     
  
      
