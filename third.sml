(*
1.	Определите функцию
a)	вычисления количества единиц в списке:
count_1s([4,3,1,6,1,1,2,1]) = 4
b)	определения принадлежности элемента списку:
member(5, [4, 2, 5]) = true, 
member(3, [4, 2, 5]) = false
*)

fun count_1s nil = 0
  | count_1s (1::t) = 1 + count_1s t
  | count_1s (i::t) = count_1s t;

count_1s [1, 2, 3, 1, 2, 3, 1]

fun member (x, nil) = false
  | member (x, (i::t)) = if x = i then true else member(x, t);

member(5, [4, 2, 5]);
member(3, [4, 2, 5]);

(*
2.	Определите функции
a)	выделения первых n элементов списка:
firstN(3, [4,3,1,6,1,1,2,1]) = [4,3,1]
b)	выделения «хвоста» списка, начинающегося с (n+1)-го элемента:
fromN(3, [4,3,1,6,1,1,2,1]) = [6,1,1,2,1]
*)

fun firstN (n, nil) = nil
  | firstN (0, t) = nil
  | firstN (n, x::t) = x :: firstN(n - 1, t);

firstN (2, [1, 2, 3]);
firstN (25, [1, 2, 3]);
firstN (1, [1, 2, 3]);
firstN (0, [1, 2, 3]);

fun fromN (i, nil) = nil
  | fromN (0, arr) = arr
  | fromN (i, x::t) = fromN (i - 1, t);

fromN (3, [1, 2, 3, 4, 5, 6, 7, 8, 9]);
fromN (1, [1, 2, 3, 4, 5, 6, 7, 8, 9]);
fromN (0, [1, 2, 3, 4, 5, 6, 7, 8, 9]);
fromN (3, [4,3,1,6,1,1,2,1]);
(*
3.	Определите функцию
a)	вставки элемента в упорядоченный список:
insert(5, [2,3,6,9]) = [2,3,5,6,9]
b)	построения списка чисел в заданном интервале:
upto(6, 9) = [6, 7, 8, 9]
*)

fun insert (x, nil     ) = [x]
  | insert (x, (i::j)  ) = if x > i 
                           then i::insert(x, j)
                           else x::i::j;

insert (9, [1, 3, 5, 7]);
insert (0, [1, 3, 5, 7]);
insert (4, [1, 3, 5, 7]);
insert (5, [3]);
insert (1, [3]);
insert (1, nil);

local
  fun recurs (s, f) = if s = f
                      then [s]
                      else s :: recurs(s + 1, f);
in
  fun upto (i, j) = if j <= i then nil else recurs(i, j);
end;

upto (5, 9);
upto (3, 1);

(*
4.	Переделайте функцию firstword таким образом, чтобы она правильно обрабатывала строки с пробелами в начале:
firstword’(“ My name is Boris”) = “My”
(подсказка: определите и используйте функцию удаления пробелов в начале списка)
*)

fun fw(nil) = nil
  | fw(#" "::t) = nil
  | fw(h::t) = h :: fw(t);

fun strip (#" "::t) = strip t
  | strip (t) = t;

val firstword' = implode o fw o strip o explode;

firstword'("Without start spaces");
firstword'("   Hello world");

(*
5.	Определите функцию преобразования строки (с пробелами) в список слов:
scanlex(“My name is Boris”) = [“My”, “name”, “is”, “Boris”]
(подсказка: кроме firstword’, определите и используйте также функцию butfirstword' выделения «хвоста» строки, начинающегося с первого пробела)
*)

(*Функция первого слова*)
fun fw(nil) = nil
  | fw(#" "::t) = nil
  | fw(h::t) = h :: fw(t);

fun strip (#" "::t) = strip t
  | strip (t) = t;

val firstword' = implode o fw o strip o explode;

(*Функция хвоста слова*)
fun tail (nil) = nil
  | tail (#" "::t) = t
  | tail (_::t) = tail(t);

val butfirstword' = implode o strip o tail o strip o explode;

(*Функция составления массива слов*)
fun scanlex "" = nil
  | scanlex (line) = firstword'(line) :: scanlex(butfirstword'(line));

scanlex (" My name   is     Boris  ")
