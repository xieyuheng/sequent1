type formal_name = string ;;
type formal_var = string ;;

type formal_arrow = formal_cedent * formal_cedent
and formal_cedent = formal_form list
and formal_bind = formal_var * formal_cedent * bool
and formal_form =
  | N of formal_name
  | V of formal_var
  | A of formal_arrow
  | B of formal_bind
;;

type formal_top =
  | DT of (formal_name * formal_arrow) * (formal_name * formal_arrow) list
  | DF of (formal_name * formal_arrow) * formal_arrow list
  | AP of formal_arrow
;;

type var = string array * int ;;
type name = string ;;

type arrow = cedent * cedent
and cedent = form list
and bind = var * cedent * bool
and form =
  | NAME  of name
  | VAR   of var
  | ARROW of arrow
  | BIND  of bind
;;

type data =
  | VAR of var
  | ARROW of arrow
  | CONS of name * data list
  | TRUNK of name * data list
;;

type store =
  | FUNCTION of formal_arrow * formal_arrow list
  | TYPE_CONSTRUCTOR of formal_arrow * int * name list
  | DATA_CONSTRUCTOR of formal_arrow * int * name
;;

type env
= data list
* (var * data) list
* (name * store) list
;;

let parse_arrow
: formal_arrow -> arrow
= fun fa ->
  match fa with
  | fac, fsc -> ()
;;

let apply_arrow
: arrow -> env -> env
= fun a e ->
  match a, e with
  | (ac, sc), (ds, bs, ns) ->

;;

let rec eval
: formal_top list -> env -> env
= fun fs e ->
  match fs with
  | [] -> e
  | h :: r -> eval r (eval_formal_top h e)

and eval_formal_top
: formal_top -> env -> env
= fun f e ->
  match f, e with
  | DT ((fn, fa), fnfas), (ds, bs, ns) ->
    (ds, bs, (add_  ns))
  | DF ((fn, fa), fas), (ds, bs, ns) ->
    ()
  | AP (fa), (ds, bs, ns) ->
    (apply_arrow (parse_arrow fa) e)
;;

let
:
= fun ->
  match  with
  |
  |
  |
;;
