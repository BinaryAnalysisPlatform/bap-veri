open Core_kernel[@@warning "-D"]


type t = {
  cols : int;
  rows : int;
  data : (string * string list) Int.Map.t;
}

let create headers = {
  cols = 0;
  rows = 0;
  data =
    List.foldi headers ~init:(Map.empty (module Int))
      ~f:(fun i tab hd ->
          let hd = sprintf " %s " hd in
          Map.set tab ~key:i ~data:(hd,[]));
}


let add_row t data = {
  cols = max t.cols (List.length data);
  rows = t.rows + 1;
  data =
    List.foldi data ~init:t.data ~f:(fun i tab cell ->
        Map.change tab i ~f:(function
            | None -> None
            | Some (hd,cells) -> Some (hd, sprintf " %s " cell :: cells)));
}

let pp fmt t =
  let total, widths =
    Map.fold t.data ~init:(0,[]) ~f:(fun ~key:i ~data:(header,cells) (total,acc) ->
        let max_len = List.fold (header::cells) ~init:0
            ~f:(fun len x ->
                let len' = String.length x in
                if len'  > len then len' else len) in
        total + max_len, (i,max_len) :: acc) in
  let col_width i = List.Assoc.find_exn widths i ~equal:Int.equal in
  let print_cell ?(term='|') i cell =
    let w = col_width i in
    let w' = String.length cell in
    let spaces = String.init (w - w') ~f:(fun _ -> ' ') in
    Format.fprintf fmt "%s%s%c" cell spaces term in
  let cell col_i row_j =
    let _, data = Map.find_exn t.data col_i in
    match List.nth (List.rev data) row_j with
    | None -> "-"
    | Some x -> x in
  let rows = List.init t.rows ~f:Fn.id in
  let cols = List.init t.cols ~f:Fn.id in
  let barrier = String.init (total + t.cols - 1) ~f:(fun _ -> '-') in
  Format.fprintf fmt "|%s|\n|" barrier;
  Map.iteri t.data ~f:(fun ~key:i ~data:(header,_) -> print_cell i header);
  Format.fprintf fmt "\n|";
  Map.iteri t.data ~f:(fun ~key:i ~data:_ ->
      let line = String.init (col_width i) ~f:(fun _ -> '-') in
      let term = if i + 1 = t.cols then '|' else '+' in
      print_cell ~term i line);
  Format.fprintf fmt "\n|";
  List.iter rows ~f:(fun row_i ->
      List.iter cols ~f:(fun col_i ->
          let cell = cell col_i row_i in
          print_cell col_i cell);
      Format.fprintf fmt "\n|");
  Format.fprintf fmt "%s|\n%!" barrier
