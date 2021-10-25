(*
1.	Определите функцию при помощи функции map:
a)	fttl(["strange", "show", "think"])=["range", "ow", "ink"]
b)	fsml(["war", "la", "tea"])= ["swarm", "slam", "steam"]
*)

val fttl = map (fn x => implode x)
         o map (fn x::t => t | nil => nil)
         o map (fn x::t => t | nil => nil)
         o map (fn x => explode x);
fttl(["strange", "show", "think", "", "a", "bc", "efg"])


val fsml = map (fn x => implode x)
          o map (fn x => x @ [#"m"])
          o map (fn x => #"s" :: x)
          o map (fn x => explode x);

fsml(["war", "la", "tea"])

(*
2.	Определите функции при помощи функции reduce:
a)	count [3,2,5,1] = 4
b)	duplist [4,2,5,1] = [4,4,2,2,5,5,1,1]
*)

fun	reduce f b nil = b
  | reduce f b (h::t)	= f(h,reduce f b t);

val count = reduce (fn (a, b) => a + 1) 1;
count [3, 2, 5, 1];


val duplist = reduce (fn (a:int, b:int list) => [a, a] @ b) nil;
duplist [4, 2, 5, 1];

(*
3.	Определите функцию:
a)	определения максимального из всех значений, хранящихся на узлах данного дерева (один входной параметр – дерево, возвращает максимальное значение);
b)	подсчета количества узлов, содержащих данное значение (два входных параметра – дерево и значение, возвращает число узлов).
*)

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree;

val tree1 = Node(
  Node(Empty, 5, Empty),
  2,
  Node(Empty, 3, Empty)
);

val tree2 = Node(
  Node(
    Empty, 
    5, 
    Node(
      Empty,
      6,
      Empty
      )
    ),
    2,
    Empty
);

val tree3 = Node(
  Node(
    Empty, 
    5, 
    Node(
      Empty,
      6,
      Empty
      )
    ),
    2,
    Node(
      Node(Empty, 5, Empty),
      5,
      Node(Empty, 3, Empty)
    )
);

(* Решение под A *)

local
  val min_value = 0
  val max = reduce (fn (a, b) => if a < b then b else a) 0;
in
  fun max_in_tree (Empty) = min_value
    | max_in_tree (Node(left, v_cur, right)) = max [v_cur, max_in_tree(left), max_in_tree(right)];
end;

max_in_tree(Empty);
max_in_tree(tree1);
max_in_tree(tree2);

(* Решение под Б *)

local
  fun eq (x:int, y:int) = if x = y then 1 else 0;
in
  fun count_value_in_tree (x, (Empty)) = 0
    | count_value_in_tree (x, (Node(left, value, right))) = count_value_in_tree(x, (left)) + count_value_in_tree(x, (right)) + eq(value, x);
end;

count_value_in_tree(2, tree3);
count_value_in_tree(5, tree3);
count_value_in_tree(7, tree3);

(*
4.	Определите функцию:
a)	удаления пробелов из текстового файла (два входных параметра – имена исходного и результирующего файлов);
b)	получения пустого текстового файла (составленного из пробелов) заданной длины (два входных параметра – имя результирующего файла и его длина).
*)

(* Переменные окружения *)
val dir = "C:/Users/sugar/Desktop/";
val fileIn = dir ^ "in.txt";
val fileOut = dir ^ "out.txt";

(* Методы для работы с файлами *)
fun fileToString(fileName) = let
  val stream = TextIO.openIn(fileName)
  val str = TextIO.inputAll(stream)
  val _ = TextIO.closeIn(stream)
in 
  str 
end;

fun stringToFile(str, fileName) = let
  val stream = TextIO.openOut fileName
  val _ = TextIO.output(stream, str)
  val _ = TextIO.closeOut stream
in 
  str 
end;

fun changeFile (input,output,func) = let
  val text = fileToString(input)
  val changed = func(text)
  val _ = stringToFile(changed, output)
in
  changed
end;

(* Решение под А *)
local
  fun clear_spaces (nil) = nil
    | clear_spaces (#" "::t) = clear_spaces(t)
    | clear_spaces (x::t) = x::clear_spaces(t);
  val clear' = implode o clear_spaces o explode;

  fun fileToString(fileName) = let
    val stream = TextIO.openIn(fileName)
    val str = TextIO.inputAll(stream)
    val _ = TextIO.closeIn(stream)
  in 
    str 
  end;

  fun stringToFile(str, fileName) = let
    val stream = TextIO.openOut fileName
    val _ = TextIO.output(stream, str)
    val _ = TextIO.closeOut stream
  in 
    str 
  end;

  fun changeFile (input,output,func) = let
    val text = fileToString(input)
    val changed = func(text)
    val _ = stringToFile(changed, output)
  in
    changed
  end;
in
  fun strip_spaces_in_file (fileIn, fileOut) = changeFile(fileIn, fileOut, clear')
end;

strip_spaces_in_file (fileIn, fileOut)

(* Решение под Б *)

local
  fun stringToFile(str, fileName) = let
    val stream = TextIO.openOut fileName
    val _ = TextIO.output(stream, str)
    val _ = TextIO.closeOut stream
  in 
    str 
  end;
stringToFile
  fun space_line_N 0 = ""
  | space_line_N count = " " ^ space_line_N (count - 1);
in
  fun empty_file (file_name, length) = stringToFile(space_line_N length, file_name);
end;

val dir = "C:/Users/sugar/Desktop/";
empty_file (dir ^ "test2.txt", 50000)