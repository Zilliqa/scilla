  $ scilla-fmt zip.scilexp
  let list_head =
    tfun 'A =>
      fun (l : List 'A) =>
        match l with
        | Cons h t => Some {('A)} h
        | Nil => None {('A)}
        end
  in
  let list_tail =
    tfun 'A =>
      fun (l : List 'A) =>
        match l with
        | Cons h t => Some {(List 'A)} t
        | Nil => None {(List 'A)}
        end
  in
  let list_reverse =
    tfun 'A =>
      fun (l : List 'A) =>
        let folder = @list_foldl ('A) (List 'A) in
        let init = Nil {('A)} in
        let iter =
          fun (z : List 'A) =>
            fun (h : 'A) =>
              Cons {('A)} h z
        in
        folder iter init l
  in
  let list_zip =
    tfun 'A =>
      tfun 'B =>
        fun (m1 : List 'A) =>
          fun (m2 : List 'B) =>
            let list_zip_helper =
              fun (l1 : List 'A) =>
                fun (l2 : List 'B) =>
                  let folder =
                    @list_foldl ('A) (Pair (List (Pair 'A 'B)) (List 'B))
                  in
                  let nil = Nil {(Pair 'A 'B)} in
                  let init = Pair {(List (Pair 'A 'B)) (List 'B)} nil l2 in
                  let iter =
                    fun (z : Pair (List (Pair 'A 'B)) (List 'B)) =>
                      fun (h : 'A) =>
                        match z with
                        | Pair r b =>
                          (* Get b's head, pair it with h and add to r. *)
                          let header = @list_head ('B) in
                          let tailer = @list_tail ('B) in
                          let bhead = header b in
                          match bhead with
                          | Some bel =>
                            let newp = Pair {('A) ('B)} h bel in
                            let newp_concat = Cons {(Pair 'A 'B)} newp r in
                            let btail = tailer b in
                            let newb =
                              match btail with
                              | Some t => t
                              | None => let nilb = Nil {('B)} in nilb
                              end
                            in
                            Pair {(List (Pair 'A 'B)) (List 'B)} newp_concat newb
                          | None => z
                          end
                        end
                  in
                  folder iter init l1
            in
            let res = list_zip_helper m1 m2 in
            match res with
            | Pair x y => let reverser = @list_reverse (Pair 'A 'B) in reverser x
            end
  in
  list_zip
