(*#1 merge list 함수*)
fun merge (list1:int list, list2:int list) = 
    if null list1 andalso null list2
    then []
    else if null list1
    then list2
    else if null list2
    then list1
    else
        let
            val a = hd list1;
            val b = hd list2;
        in
            if a>b then b :: merge(list1,tl list2)
            else a :: merge(tl list1,list2)
        end
    
    


(*#2 reverse list 함수*)
fun reverse (list:int list) =
    if null list
    then []
    else 
        let
            fun reversing(list1:int list, list2:int list) =
                if null list1
                then list2
                else reversing(tl list1, (hd list1)::list2)
        in
            reversing(tl list, [hd list])
        end


(*#3 pi funtion*)
fun pi(a:int,b:int,f:int->int) = 
    if a<=b
    then f(a)*pi(a+1,b,f)
    else 1
    
(*
fun f(a:int) =
    a*)

(*#4 Digits Function*)
fun digits(num:int) = 
    if num >= 10
    then 
        let
            fun reversedigits(n:int, lst:int list) = 
                if n >= 10
                then reversedigits(n div 10, (n mod 10)::lst)
                else n::lst
        in
            reversedigits(num div 10, [num mod 10])
        end
    else [num]

(*5*)
(*Additive Persistence : 더한 횟수 구하기*)
fun additivePersistence(num:int) = 
    if num >= 10
    then 
        let
            fun sumList(sum:int, list:int list) = 
                if null list
                then sum
                else 
                    let
                        val x = hd list
                        val temp_sum = sum+x
                    in
                        sumList(temp_sum, tl list)
                    end
        in
            1 + additivePersistence(sumList(0,digits(num))) 
        end       
    else 0

(*digitalRoot : 각자리수 더해서 한자리 만들기*)
fun digitalRoot(num:int) = 
    if num >= 10
    then 
        let
            fun sumList(sum:int, list:int list) = 
                if null list
                then sum
                else 
                    let
                        val x = hd list
                        val temp_sum = sum+x
                    in
                        sumList(temp_sum, tl list)
                    end
        in
            digitalRoot(sumList(0,digits(num)))        
        end
    else num