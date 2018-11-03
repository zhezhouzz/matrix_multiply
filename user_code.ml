open Library;;

let dot_sparse_sparse vec1 vec2 =
    Library.SparseVector.foldl vec1 0
        (fun _sum (i1, v1) ->
            _sum + (Library.SparseVector.foldl vec2 0
                 (fun product (i2, v2) ->
                    if i1 = i2 then
                        v1*v2
                    else product)))

let dot_dense_sparse vec1 vec2 =
    Library.DenseMatrix.foldl vec1 0
        (fun _sum (i1, v1) ->
            _sum + (Library.SparseVector.foldl vec2 0
                 (fun product (i2, v2) ->
                    if i1 = i2 then
                        v1*v2
                    else product)))

let dot_sparse_dense vec1 vec2 =
    Library.SparseVector.foldl vec1 0
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

let printvec_sparse vec =
    Library.SparseVector.foldl vec ()
    (fun _ e ->
    match e with
    | (_, value) -> print_int value; print_string " ");
    print_string "\n";;

let printvec_dense vec =
    Library.DenseMatrix.foldl vec ()
    (fun _ e ->
    match e with
    | (_, value) -> print_int value; print_string " ");
    print_string "\n";;

let vec1 = Library.DenseMatrix.to_vector [1;2;4] in
let vec2 = Library.DenseMatrix.to_vector [1;2;4] in
print_int (dot_dense_dense vec1 vec2); print_string "\n";;
let vec1 = Library.SparseVector.to_vector [1;2;4] in
let vec2 = Library.SparseVector.to_vector [1;2;4] in
print_int (dot_sparse_sparse vec1 vec2); print_string "\n";;

let printmat_dense mat =
    Library.DenseMatrix.foldcoll mat ()
    (fun _ e ->
    match e with
    | (_, value) -> printvec_dense value);;

let rec printvec_list vec =
    List.map (fun e ->
        print_int e; print_string " ") vec;
    print_string "\n";;

let rec printmat_list mat =
    match mat with
    | [] -> ()
    | h :: t -> printvec_list h; printmat_list t;;

let multiply_dense_dense mat1 mat2 =
        Library.DenseMatrix.foldcoll mat1 []
        (fun result_mat (i, row_of_mat1) ->
            let new_row = Library.DenseMatrix.foldrowl mat2 []
                (fun result_row (j, col_of_mat2) ->
                    let dot_result =
                        dot_dense_dense row_of_mat1 col_of_mat2 in
                    result_row@[dot_result]) in
            result_mat@[new_row]);;

let show_dense_dense mat1 mat2 =
        Library.DenseMatrix.foldcoll mat1 ()
        (fun _ (i, row_of_mat1) ->
            Library.DenseMatrix.foldrowl mat2 ()
            (fun _ (j, col_of_mat2) ->
                print_int i; print_string "[i]: ";
                printvec_dense row_of_mat1;
                print_int j; print_string "[j]: ";
                printvec_dense col_of_mat2;
                ));;


let mat1 = Library.DenseMatrix.to_matrix
    [[1;2;3;4];
     [1;2;3;4];
     [1;2;3;4]] in
let mat2 = Library.DenseMatrix.to_matrix
    [[1;2;3];
     [1;2;3];
     [1;2;3];
     [1;2;3]] in
(* show_dense_dense mat1 mat2 *)
printmat_list (multiply_dense_dense mat1 mat2)