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

(*1 check pattern*)
fun check_pat (p) =
    let
        fun check_list (xs :string list) = 
            case xs of
                [] => true
                | x::xs' => if List.exists(fn n => n = x) xs'
                            then false
                            else check_list(xs')
        fun make_list (p) =
            case p of
                Variable (s) => [s]
                | TupleP (ps) => foldl (fn (x,acc) => acc @ make_list x) [] ps
                | ConstructorP (c, p) => make_list p
                | _ => [] 
                (*
                | Wildcard => []
                | UnitP => []
                | ConstP (n) => []
                *)
    in
        check_list(make_list p)
    end

(*2 match value and pattern*)
fun match (v,p): (string * valu) list option = 
    case (v,p) of
        (_,Wildcard) => SOME []
        | (_, Variable s) => SOME [(s,v)]    
        | (Unit,UnitP) => SOME []
        | (Const x,ConstP y) => if x = y
                                then SOME []
                                else NONE
        | (Tuple vs,TupleP ps) => if List.length vs = List.length ps
                                then 
                                    let 
                                        val lst = List.filter(fn (v,p) => match(v,p) <> NONE) (ListPair.zip(vs,ps))
                                        fun b_list(v,p) =
                                            case match(v,p) of
                                                SOME b => b
                                                | NONE => []
                                    in
                                        if List.length lst = List.length vs
                                        then SOME (foldl (fn(x,acc)=> acc @ b_list(x)) [] lst)
                                        else NONE
                                    end
                                else NONE
        | (Constructor(s2,v),ConstructorP (s1,p)) => if s1 = s2
                                                then match(v,p)
                                                else NONE
        | _ => NONE


(*3. RSP*)
type name = string

datatype RSP = ROCK
                | SCISSORS
                | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament = PLAYER of name * (RSP strategy ref)
                        | MATCH of tournament * tournament
fun onlyOne(one:RSP) = 
    Cons(one, fn() => onlyOne(one))

fun alterTwo(one:RSP, two:RSP) = 
    Cons(one, fn() => alterTwo(two, one))

fun alterThree(one:RSP, two:RSP, three:RSP) = 
    Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val rs = alterTwo(ROCK, SCISSORS)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)
val rps = alterThree(ROCK, PAPER, SCISSORS)

fun next(strategyRef) =
    let 
        val Cons(rsp:RSP, func) = !strategyRef 
    in
        strategyRef := func();
        rsp
    end

fun whosWinner(t) = 
    case t of
        MATCH(x,y) => 
                        let
                            val PLAYER(x_name,x_ref) = whosWinner(x)
                            val PLAYER(y_name,y_ref) = whosWinner(y)
                            val x_next = next(x_ref)
                            val y_next = next(y_ref)
                        in
                            case (x_next,y_next) of
                                (ROCK,ROCK) => whosWinner(MATCH(PLAYER(x_name,x_ref),PLAYER(y_name,y_ref)))
                                | (ROCK,SCISSORS) => whosWinner(x)
                                | (ROCK,PAPER) => whosWinner(y)
                                | (SCISSORS,ROCK) => whosWinner(y)
                                | (SCISSORS,SCISSORS) => whosWinner(MATCH(PLAYER(x_name,x_ref),PLAYER(y_name,y_ref)))
                                | (SCISSORS,PAPER) => whosWinner(x)
                                | (PAPER,ROCK) => whosWinner(x)
                                | (PAPER,SCISSORS) => whosWinner(y)
                                | (PAPER,PAPER) => whosWinner(MATCH(PLAYER(x_name,x_ref),PLAYER(y_name,y_ref)))(*MATCH(whosWinner(x),whosWinner(y))*)
                        end
        | PLAYER(name,s) => PLAYER(name,s)