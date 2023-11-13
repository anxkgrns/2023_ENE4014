

(*#1 Simple Eval*)
datatype expr = NUM of int 
                | PLUS of expr * expr 
                | MINUS of expr * expr

datatype formula = TRUE
                    | FALSE
                    | NOT of formula
                    | ANDALSO of formula * formula
                    | ORELSE of formula * formula
                    | IMPLY of formula * formula
                    | LESS of expr * expr
                    (* LESS(a, b) is true if a < b *)




fun eval temp =
    let
        fun exp operator =
            case operator of
                NUM(X) => X
                | PLUS(X,Y) => (exp X) + (exp Y)
                | MINUS(X,Y) => (exp X) - (exp Y)
    in
        case temp of
                TRUE => true
                | FALSE => false
                | NOT(X) => not (eval X)
                | ANDALSO(X,Y) => (eval X) andalso (eval Y)
                | ORELSE(X,Y) => (eval X) orelse (eval Y)
                | IMPLY(X,Y) => if (eval X) = true andalso (eval Y) =false
                                then false
                                else true
                | LESS(X,Y) => (exp X) < (exp Y)
                        
    end
    

(*2 METROMAP*)
type name = string

datatype metro = STATION of name
                | AREA of name*metro
                | CONNECT of metro*metro



fun checkMetro m_temp =
    let
        fun checkArealist(temp , list1) =
            let 
                fun checkList(st_name, list1) = 
                    if(list1 = [])
                    then false
                    else if (hd list1) = st_name
                        then true
                        else checkList(st_name,tl list1)
            in
                case temp of
                    STATION(X) => if checkList(X,list1)
                                    then true
                                    else false
                    | AREA(X,Y) => checkArealist(Y, X::list1)
                    | CONNECT(X,Y) => checkArealist(X,list1) andalso checkArealist(Y,list1)
            end
    in
        checkArealist(m_temp,[])
    end
    


(*3 LAZY LIST*)
datatype 'a lazyList = nullList
                    | cons of 'a * (unit -> 'a lazyList) 

fun seq(first,last) =
    if first > last
    then nullList
    else cons(first, fn () => seq(first+1,last))

fun infSeq(first) = 
    cons(first,fn() => infSeq(first+1))

fun firstN(lazyListVal,n) =
    case lazyListVal of
        nullList => []
        | cons(X,Y) => if n = 0
                        then []
                        else X::firstN(Y(),n-1)


datatype 'a option = SOME of 'a | NONE

fun Nth(lazyListVal,n) =
    if n <= 0
    then NONE
    else
        case lazyListVal of
            nullList => NONE
            | cons(X,Y) => if n = 1
                            then SOME X
                            else Nth(Y(),n-1)

fun filterMultiples(lazyListVal,n) =
    if n = 0
    then case lazyListVal of
        nullList => nullList
        | cons(X,Y) => if X = 0
                        then filterMultiples(Y(),n)
                        else cons(X,fn() =>filterMultiples(Y(),n))
    else
    case lazyListVal of
        nullList => nullList
        | cons(X,Y) => if X mod n = 0
                        then filterMultiples(Y(),n)
                        else cons(X,fn() =>filterMultiples(Y(),n))

fun primes() =
    let
        fun sieve(lazyListVal) =
            case lazyListVal of 
                nullList => nullList
                | cons(X,Y) => let
                                    val lazyListVal2 = filterMultiples(Y(),X)
                                in
                                    cons(X,fn()=>sieve(lazyListVal2))
                                end
    in
        sieve(infSeq(2))
    end




