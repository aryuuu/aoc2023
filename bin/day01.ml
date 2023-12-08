let lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let string_to_char str = str |> String.to_seq |> List.of_seq
let is_digit c = match c with
| '0' ..'9' -> true
| _ -> false

let first_and_last_item items = 
  let first = List.hd items in
  let last = List.nth items (List.length items - 1) in
  (first, last)

let extract_first_and_last_int str =
  string_to_char str 
    |> List.filter is_digit 
    |> List.map Char.code
    |> List.map (fun c -> c - 48)
    |> first_and_last_item
    |> (fun (first, last) -> (first * 10) + last)

let () = lines "input/day01.txt" 
    |> List.filter (fun str -> String.length str > 0)
    |> List.map extract_first_and_last_int 
    |> List.fold_left (+) 0
    |> print_int
