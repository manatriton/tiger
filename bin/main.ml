(* open Base
   open Stdio
   open Tiger

   let () =
     let wc = Hello.word_count "foo bar baz foo bar" in
     Map.iter_keys wc ~f:(fun key ->
         let value = Map.find_exn wc key in
         printf "(\"%s\": %d)\n" key value) *)
