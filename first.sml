(*
1.	Определите функцию (min3) от трех аргументов целого типа, 
возвращающую наименьший из аргументов. Использовать вызов 
функции от двух аргументов (написать свою или использовать стандартную).
*)

fun min3 (a:int) (b:int) (c:int) = Int.min(c,Int.min(a,b));
min3 1 2 3;
min3 5 4 6;
min3 9 8 7;

(*
2.	Используя конструкцию let, упростите запись выражения (5 * 4 + (5 * 4 - 72)) div 5 + (5 * 4 - 72)*(5 * 4 - 72) - 5 * 4
*)
let
  val a = 5 * 4
  val b = a - 72
in
  (a + b) div 5 + b * b - a
end;

(*
3.	Используя конструкцию local, определите функцию «учетверения» строки (quadr),
 использующую локальную функцию «удвоения» строки (dupl). Например:
dupl “go ” = “go go ”
quadr “go ” = “go go go go ”
*)
local
  fun dupl (a:string) = a ^ a
in
  fun quadr (a:string) = dupl ( dupl a )
end;

quadr "go "

(*
4.	Определите функцию ComplexModule вычисления модуля комплексного числа. 
Функция получает в качестве параметра запись (record) с полями re и im типа real,
возвращает real. Использовать функцию sqrt модуля Math.
*)

local
  fun sqr (i:real) = i * i
in
  fun ComplexModule (re:real) (im:real) = Math.sqrt( sqr re + sqr im ) 
end;
ComplexModule 7.0 ~4.0

(*
5.	Определите функцию «переворота» строки (trans).
Например, trans “1234” = “4321”. Использовать рекурсию. 
*)
local
  fun last_char "" = ""
    | last_char s:string = substring(s, size s - 1, 1)

  fun crop_last_char (s:string) = substring(s, 0, size s - 1)
  
  fun body_recurs ("", destination:string) = destination
    | body_recurs (source:string, destination:string) = body_recurs (
        crop_last_char source, destination ^ last_char source
      );
in
  fun revert "" = ""
    | revert a:string = body_recurs(a, "")
end;

revert "54321"

