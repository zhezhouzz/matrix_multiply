open Library;;

let if_sparse_aux l =
    List.fold_left (fun counter e ->
        match counter with
    | (num, nonzero) ->
        if e = 0 then (num + 1, nonzero)
        else (num + 1, nonzero + 1)) (0, 0) l

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

let if_approximate_in_vector_dense vec =
    let m = Library.DenseMatrix.foldl vec  (None, true)
        (fun (m, if_appr) (_, v) ->
            if if_appr = false then (m, false) else
            match m with
            | None -> (Some v, true)
            | Some v' -> if v = v'
                         then (Some v, true)
                         else (Some v', false)) in
    match m with
    | (_, if_appr) -> if_appr

let if_approximate_in_vector_sparse vec =
    let m = Library.SparseMatrix.foldl vec (None, true)
        (fun (m, if_appr) (_, v) ->
            if if_appr = false then (m, false) else
            match m with
            | None -> (Some v, true)
            | Some v' -> if v = v'
                         then (Some v, true)
                         else (Some v', false)) in
    match m with
    | (_, if_appr) -> if_appr

let sample_vector_sparse vec =
    let m = Library.SparseMatrix.foldl vec None
        (fun m (_, v) -> Some v) in
    match m with
    | None -> 0
    | Some v -> v

let sample_vector_dense vec =
    let m = Library.DenseMatrix.foldl vec None
        (fun m (_, v) -> Some v) in
    match m with
    | None -> 0
    | Some v -> v

let dot_sparse_sparse vec1 vec2 =
    Library.SparseMatrix.foldl vec1 0
        (fun _sum (i1, v1) ->
            _sum + (Library.SparseMatrix.foldl vec2 0
                 (fun product (i2, v2) ->
                    if i1 = i2 then
                        v1*v2
                    else product)))

let dot_dense_sparse vec1 vec2 =
    Library.DenseMatrix.foldl vec1 0
        (fun _sum (i1, v1) ->
            _sum + (Library.SparseMatrix.foldl vec2 0
                 (fun product (i2, v2) ->
                    if i1 = i2 then
                        v1*v2
                    else product)))

let dot_sparse_dense vec1 vec2 =
    Library.SparseMatrix.foldl vec1 0
        (fun _sum (i1, v1) ->
            _sum + (Library.DenseMatrix.foldl vec2 0
                 (fun product (i2, v2) ->
                    if i1 = i2 then
                        v1*v2
                    else product)))

let dot_dense_dense vec1 vec2 =
    Library.DenseMatrix.foldl vec1 0
        (fun _sum (i1, v1) ->
            _sum + (Library.DenseMatrix.foldl vec2 0
                 (fun product (i2, v2) ->
                    if i1 = i2 then
                        v1*v2
                    else product)))

let scalar_x_vector_dense scalar vec =
    Library.DenseMatrix.foldl vec 0
        (fun result (_, v) ->
            result + scalar*v)

let scalar_x_vector_sparse scalar vec =
    Library.SparseMatrix.foldl vec 0
        (fun result (_, v) ->
            result + scalar*v)

let printvec_sparse vec =
    Library.SparseMatrix.foldl vec ()
    (fun _ e ->
    match e with
    | (i, value) -> Printf.printf "(%i,%i)" i value; print_string " ");
    print_string "\n";;

let printvec_dense vec =
    Library.DenseMatrix.foldl vec ()
    (fun _ e ->
    match e with
    | (_, value) -> print_int value; print_string " ");
    print_string "\n";;

let printmat_dense mat =
    Library.DenseMatrix.foldcoll mat ()
    (fun _ e ->
    match e with
    | (_, value) -> printvec_dense value);;

let printmat_sparse mat =
    Library.SparseMatrix.foldcoll mat ()
    (fun _ e ->
    match e with
    | (i, value) -> print_int i; print_string ": "; printvec_sparse value);;

let multiply_dense_dense_dense mat1 mat2 =
        Library.DenseMatrix.to_matrix (Library.DenseMatrix.foldcoll mat1 []
        (fun result_mat (i, row_of_mat1) ->
            let new_row = Library.DenseMatrix.foldrowl mat2 []
                (fun result_row (j, col_of_mat2) ->
                    let dot_result =
                        if if_approximate_in_vector_dense row_of_mat1
                        then
                            let scalar = sample_vector_dense row_of_mat1 in
                            print_string "DenseVector ~> Scalar\n";
                            scalar_x_vector_dense scalar col_of_mat2
                        else
                            dot_dense_dense row_of_mat1 col_of_mat2 in
                    result_row@[dot_result]) in
            result_mat@[new_row]));;

let multiply_sparse_dense_dense mat1 mat2 =
        Library.DenseMatrix.to_matrix (Library.SparseMatrix.foldcoll mat1 []
        (fun result_mat (i, row_of_mat1) ->
            let new_row = Library.DenseMatrix.foldrowl mat2 []
                (fun result_row (j, col_of_mat2) ->
                    let dot_result =
                        if if_approximate_in_vector_sparse row_of_mat1
                        then
                            let scalar = sample_vector_sparse row_of_mat1 in
                            print_string "SparseVector ~> Scalar\n";
                            scalar_x_vector_dense scalar col_of_mat2
                        else
                            dot_sparse_dense row_of_mat1 col_of_mat2 in
                    result_row@[dot_result]) in
            result_mat@[new_row]));;

let multiply_dense_sparse_dense mat1 mat2 =
        Library.DenseMatrix.to_matrix (Library.DenseMatrix.foldcoll mat1 []
        (fun result_mat (i, row_of_mat1) ->
            let new_row = Library.SparseMatrix.foldrowl mat2 []
                (fun result_row (j, col_of_mat2) ->
                    let dot_result =
                        if if_approximate_in_vector_dense row_of_mat1
                        then
                            let scalar = sample_vector_dense row_of_mat1 in
                            print_string "DenseVector ~> Scalar\n";
                            scalar_x_vector_sparse scalar col_of_mat2
                        else
                            dot_dense_sparse row_of_mat1 col_of_mat2 in
                    result_row@[dot_result]) in
            result_mat@[new_row]));;

let multiply_sparse_sparse_sparse mat1 mat2 =
        Library.SparseMatrix.to_matrix (Library.SparseMatrix.foldcoll mat1 []
        (fun result_mat (i, row_of_mat1) ->
            let new_row = Library.SparseMatrix.foldrowl mat2 []
                (fun result_row (j, col_of_mat2) ->
                    let dot_result =
                        if if_approximate_in_vector_sparse row_of_mat1
                        then
                            let scalar = sample_vector_sparse row_of_mat1 in
                            print_string "SparseVector ~> Scalar\n";
                            scalar_x_vector_sparse scalar col_of_mat2
                        else
                            dot_sparse_sparse row_of_mat1 col_of_mat2 in
                    result_row@[dot_result]) in
            result_mat@[new_row]));;

(* main *)
let mat1 =
    [[1;0;3;4];
     [1;2;0;4];
     [1;0;0;0]] in
let mat2 =
    [[1;0;3];
     [1;0;2];
     [0;0;0];
     [0;0;4]] in
let if_sparse_mat1 = if_sparse mat1 in
let if_sparse_mat2 = if_sparse mat2 in
match (if_sparse_mat1, if_sparse_mat2) with
| (true, true) ->
    let mat1'= Library.SparseMatrix.to_matrix mat1 in
    let mat2'= Library.SparseMatrix.to_matrix mat2 in
    print_string "SparseMatrix x SparseMatrix\n";
    printmat_sparse (multiply_sparse_sparse_sparse mat1' mat2')
| (true, false) ->
    let mat1'= Library.SparseMatrix.to_matrix mat1 in
    let mat2'= Library.DenseMatrix.to_matrix mat2 in
    print_string "SparseMatrix x DenseMatrix\n";
    printmat_dense (multiply_sparse_dense_dense mat1' mat2')
| (false, true) ->
    let mat1'= Library.DenseMatrix.to_matrix mat1 in
    let mat2'= Library.SparseMatrix.to_matrix mat2 in
    print_string "DenseMatrix x SparseMatrix\n";
    printmat_dense (multiply_dense_sparse_dense mat1' mat2')
| (false, false) ->
    let mat1'= Library.DenseMatrix.to_matrix mat1 in
    let mat2'= Library.DenseMatrix.to_matrix mat2 in
    print_string "DenseMatrix x DenseMatrix\n";
    printmat_dense (multiply_dense_dense_dense mat1' mat2');;