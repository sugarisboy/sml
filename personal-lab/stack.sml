
datatype 'a stack = Empty | Stack of 'a * 'a stack;


fun push (x, Empty) = Stack(x, Empty)
  | push (x, stack) = Stack(x, stack);

fun value (Empty) = 0
  | value (Stack(x, stack)) = x;

fun pop (Empty) = Empty
  | pop (Stack(x, stack)) = stack;


val s = Stack(5, Stack(4, Stack(3, Stack(2, Empty))));
value(s);


explode ">5>4<>5>6"

local
  fun recurs (nil, stack) = stack
    | recurs (x::nil, stack) = stack
    | recurs (#">"::x::tail, stack) = recusr(tail, push(x, stack))
    | recurs (#"<", stack) = recusr(tail, pop(stack));
in
  fun calc(input) = recurs(input, Empty);
end

fun recurs (nil, stack) = stack
| recurs (x::nil, stack) = stack
| recurs (#">"::x::tail, stack) = recurs (tail, push(x, stack))
| recurs (#"<"::tail, stack) = recurs (tail, pop(stack));

recurs(explode ">5>4<>5>6", Empty)

fun calc input = 