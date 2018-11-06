
let if_sparse m =
    let if_sparse_aux = fun l ->
        List.fold_left (fun counter e ->
        match counter with
        | (num, nonzero) ->
            if e = 0 then (num + 1, nonzero)
            else (num + 1, nonzero + 1)) (0, 0) l in
    let (num, nonzero) = List.fold_left (fun counter row ->
        match if_sparse_aux row with
        | (num, nonzero) ->
        match counter with
        | (num', nonzero') -> (num + num', nonzero + nonzero')) (0, 0) m in
    if nonzero * 2 < num then true else false

{-@ type sparse_mat = {m:Mat | if_sparse m = true} @-}
{-@ type dense_mat = {m:Mat | if_sparse m = false} @-}

let if_approximate_in_vector vec =
    let m = foldl vec  (None, true)
        (fun (m, if_appr) (_, v) ->
            if if_appr = false then (m, false) else
            match m with
            | None -> (Some v, true)
            | Some v' -> if v = v'
                         then (Some v, true)
                         else (Some v', false)) in
    match m with
    | (_, if_appr) -> if_appr

let sample_vector vec =
    let m = foldl vec None
        (fun m (_, v) -> Some v) in
    match m with
    | None -> 0
    | Some v -> v

let dot vec1 vec2 =
    foldl vec1 0 (fun _sum (i1, v1) ->
        _sum + (foldl vec2 0
             (fun product (i2, v2) ->
                if i1 = i2 then v1*v2 else product)))

let scalar_x_vector scalar vec =
    foldl vec 0
        (fun result (_, v) ->
            result + scalar*v)

let multiply mat1 mat2 =
    foldcoll mat1 [] (fun result_mat (i, row_of_mat1) ->
        let new_row = foldrowl mat2 []
            (fun result_row (j, col_of_mat2) ->
                let dot_result =
                    Approximate(if_approximate_in_vector row_of_mat1) {
                        let scalar = sample_vector row_of_mat1 in
                        scalar_x_vector scalar col_of_mat2
                    }{
                        dot row_of_mat1 col_of_mat2 in
                    }
                result_row@[dot_result]) in
        result_mat@[new_row]);;

(* main *)
let mat2 =
    [[1;0;3;4];
     [1;2;0;4];
     [1;0;0;0]] in
let mat1 =
    [[1;0;3];
     [1;0;2];
     [0;0;0];
     [0;0;4]] in
printmat (multiply mat1 mat2)