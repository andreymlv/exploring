(* Ссылка на список функций которые принимают int и возвращают unit *)
val cbs: (int -> unit) list ref = ref []

(* Функция мутации cbs. Добавляет в начало cbs функцию f: int *)
fun onKeyEvent f = cbs := f :: (!cbs) 

fun onEvent i =
  let 
    fun loop fs =
      case fs of
        [] => () (* если функций нет в списке, то ничего не делаем *)
      | f::fs' => (f i; loop fs') (* иначе: выполняем функцию, делаем следующую
                                   итерацию *)
  in
    (* Проходимся по всему списку *)
    loop (!cbs)
  end

val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)

fun printIfPressed i = 
  onKeyEvent (fn j => if i = j
                        then print ("you pressed " ^ Int.toString i ^ "\n")
                      else ())

val _ = printIfPressed 4
val _ = printIfPressed 11
val _ = printIfPressed 23
