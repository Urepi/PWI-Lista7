(* Najdłuższy palindrom w pliku tekstowym (UTF-8).
   Porównujemy tylko litery, bez rozróżniania wielkości;
   spacje i interpunkcję pomijamy (jak w "Kobyła ma mały bok").
   Uruchomienie:  ocaml palindrom.ml 1984.txt  *)

(* zamiana wielkiej litery na małą: ASCII i polskie znaki *)
let male u =
  if u >= 65 && u <= 90 then u + 32
  else match u with
    | 0x104 -> 0x105 | 0x106 -> 0x107 | 0x118 -> 0x119   (* Ą Ć Ę *)
    | 0x141 -> 0x142 | 0x143 -> 0x144 | 0xD3  -> 0xF3    (* Ł Ń Ó *)
    | 0x15A -> 0x15B | 0x179 -> 0x17A | 0x17B -> 0x17C   (* Ś Ź Ż *)
    | _ -> u

(* litera: łacińska ASCII albo z rozszerzonego alfabetu łacińskiego *)
let litera u =
  (u >= 65 && u <= 90) || (u >= 97 && u <= 122)
  || (u >= 0xC0 && u <= 0x24F && u <> 0xD7 && u <> 0xF7)

let () =
  let plik = if Array.length Sys.argv > 1 then Sys.argv.(1) else "1984.txt" in
  let tekst = In_channel.with_open_bin plik In_channel.input_all in
  let dl = String.length tekst in

  (* 1. wypisujemy same litery: kod znaku oraz miejsce w tekście *)
  let litery = ref [] in
  let i = ref 0 in
  while !i < dl do
    let d = String.get_utf_8_uchar tekst !i in
    let n = Uchar.utf_decode_length d in
    let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
    if litera u then litery := (male u, !i, !i + n) :: !litery;
    i := !i + n
  done;
  let a = Array.of_list (List.rev !litery) in
  let n = Array.length a in
  let kod k = let (u, _, _) = a.(k) in u in

  (* 2. rozszerzanie wokół każdego środka: 2n-1 środków
        (na literze dla długości nieparzystych, między literami dla parzystych) *)
  let najl = ref 0 and najp = ref 0 in   (* najlepszy palindrom: litery najl..najp *)
  for c = 0 to 2 * n - 2 do
    let l = ref (c / 2) and p = ref (c / 2 + c mod 2) in
    while !l >= 0 && !p < n && kod !l = kod !p do decr l; incr p done;
    (* teraz l+1 .. p-1 to palindrom *)
    if !p - !l - 1 > !najp - !najl + 1 then begin
      najl := !l + 1; najp := !p - 1
    end
  done;

  (* 3. wypisujemy oryginalny fragment tekstu *)
  if n = 0 then print_endline "Brak liter w pliku."
  else begin
    let (_, poczatek, _) = a.(!najl) and (_, _, koniec) = a.(!najp) in
    let fragment = String.sub tekst poczatek (koniec - poczatek) in
    let fragment = String.map (fun ch -> if ch = '\n' then ' ' else ch) fragment in
    Printf.printf "Najdłuższy palindrom (%d liter):\n%s\n" (!najp - !najl + 1) fragment
  end
