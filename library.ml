module type Vector =
sig
    type vector
    val combine : (int * int) list -> int -> vector
    val to_vector : int list -> vector
    exception NoSuchElement
    val look: vector -> int -> int
    val foldl: vector -> 'b -> ('b -> int * int -> 'b) -> 'b
end

module SparseVector: Vector =
    struct
    type length = int
    type vector = ((int * int) list) * length
    let combine (l: (int*int) list) (length: int) : vector =
        (l, length)
    let rec to_vector_aux (l:int list) (idx: int): (int * int) list =
        match l with
        | [] -> []
        | h :: t -> if h = 0 then to_vector_aux t (idx + 1)
                    else (idx, h) :: (to_vector_aux t (idx + 1))
    let to_vector (l:int list): vector =
        ((to_vector_aux l 0), List.length l)
    exception NoSuchElement
    let rec look_aux (v : (int * int) list) (idx: int) : int =
        match v with
        | [] -> 0
        | (i, h) :: t -> if i = idx then h
                         else look_aux t idx
    let look (v : vector) (idx: int) : int =
        match v with
        | (v', length) ->
            if idx >= length then raise NoSuchElement
            else look_aux v' idx
    let rec foldl_aux (v : (int * int) list) (start: 'b) (f: 'b -> int * int -> 'b): 'b =
        match v with
        | [] -> start
        | (i, h) :: t -> foldl_aux t (f start (i, h)) f
    let foldl (v : vector) (start: 'b) (f: 'b -> int * int -> 'b): 'b =
        match v with
        | (v', length) -> foldl_aux v' start f
end

module DenseVector: Vector =
    struct
    type vector = int list
    let combine (l:(int*int) list) (length:int) : vector = []
    let to_vector (l:int list) : vector = l
    exception NoSuchElement
    let rec look (v : vector) (idx: int) : int =
        match v with
        | [] -> raise NoSuchElement
        | h :: t -> if idx <= 0 then h
                    else look t (idx - 1)
    let rec foldl_aux (v : vector) (start: 'b) (f: 'b -> int * int -> 'b) (idx: int): 'b =
        match v with
        | [] -> start
        | h :: t -> foldl_aux t (f start (idx, h)) f (idx + 1)
    let foldl (v : vector) (start: 'b) (f: 'b -> int * int -> 'b) : 'b =
        foldl_aux v start f 0
end

module type Matrix =
sig
    include Vector
    type matrix
    val to_matrix : int list list -> matrix
    val matlook: matrix -> int * int -> int
    val foldcoll: matrix -> 'b -> ('b -> int * vector -> 'b) -> 'b
    val foldrowl: matrix -> 'b -> ('b -> int * vector -> 'b) -> 'b
end

module DenseMatrix: Matrix =
    struct
    include DenseVector
    type matrix = int list list
    let to_matrix (m:int list list) : matrix = m

    let rec matlook_row (line : int list) (idx: int) : int =
        match line with
        | [] -> raise DenseVector.NoSuchElement
        | h :: t -> if idx <= 0 then h
                    else matlook_row t (idx - 1)
    let rec matlook (m : matrix) (idx: int * int) : int =
        match idx with
        | (i, j) -> match m with
                    | [] -> raise DenseVector.NoSuchElement
                    | h :: t -> if i <= 0 then
                                    matlook_row h j
                                else matlook t (i-1, j)
    let rec foldcoll_aux (m : matrix) (start: 'b) (f: 'b -> int * DenseVector.vector -> 'b) (idx: int): 'b =
        match m with
        | [] -> start
        | h :: t -> foldcoll_aux t (f start (idx, (DenseVector.to_vector h))) f (idx + 1)
    let foldcoll (m : matrix) (start: 'b) (f: 'b -> int * DenseVector.vector -> 'b) : 'b =
        foldcoll_aux m start f 0

    let rec get_col (m:matrix) (idx: int) : DenseVector.vector =
        DenseVector.to_vector(
            List.map (fun vector -> DenseVector.look (DenseVector.to_vector vector) idx) m)

    let rec foldrowl_aux (m : matrix) (start: 'b) (f: 'b -> int * DenseVector.vector -> 'b) (idx: int) (length: int): 'b =
        if idx = length then start
        else let col = get_col m idx in
             let start' = f start (idx, col) in
             foldrowl_aux m start' f (idx+1) length
    let foldrowl (m : matrix) (start: 'b) (f: 'b -> int * DenseVector.vector -> 'b) : 'b =
        match m with
        | [] -> start
        | h :: _ -> foldrowl_aux m start f 0 (List.length h)
end

module SparseMatrix: Matrix =
    struct
    include SparseVector
    type dim = int * int
    type matrix = ((int * int * int) list) * dim
    exception BadFormat
    let rec to_matrix_aux0 (v:int list) (idx:int) (row_num: int) (row_length : int): (int * int * int) list =
        match v with
        | [] -> []
        | h :: t -> if idx >= row_length then raise BadFormat else
            if h = 0 then to_matrix_aux0 t (idx + 1) row_num row_length
            else (row_num, idx, h) :: (to_matrix_aux0 t (idx + 1) row_num row_length)
    let rec to_matrix_aux1 (m:int list list) (idx:int) (row_length : int): (int * int * int) list =
        print_string "row_length = "; print_int row_length; print_string "\n";
        match (List.fold_left (fun x e ->
            match x with | (result, idx) ->
            (result@(to_matrix_aux0 e 0 idx row_length), idx + 1)) ([], 0) m) with
        | (result, _) -> result
    let to_matrix (m:int list list) : matrix =
        match m with
        | [] -> ([], (0, 0))
        | line :: m' -> let dim1 = List.length line in
            ((to_matrix_aux1 m 0 dim1), (List.length m, dim1))

    let get_row (m : (int*int*int) list) (row_num:int) (row_length:int): SparseVector.vector =
        SparseVector.combine (List.fold_left (fun result e ->
            match e with | (i, j, value) ->
                if row_num = i then result@[(j, value)]
                else result) [] m) row_length

    let get_rownum_list (m: (int*int*int) list) =
        match (List.fold_left (fun result e ->
            match e with | (i, _, _) ->
            match result with
            | (rownum_list, None) -> (rownum_list@[i], Some i)
            | (rownum_list, Some k) ->
                if k = i then (rownum_list, Some k)
                else (rownum_list@[i], Some i)) ([], None) m) with | (rownum_list, _) -> rownum_list

    let foldcoll (m : matrix) (start: 'b) (f: 'b -> int * SparseVector.vector -> 'b) : 'b =
        match m with | (m', (_, dim1)) ->
        let rownum_list = get_rownum_list m' in
        match (List.fold_left (fun start row_num ->
            match start with | (idx, start') ->
            (idx + 1, f start' (idx, (get_row m' idx dim1)))) (0, start) rownum_list) with
        | (_, result) -> result

    let rec matlook_aux (m' : (int*int*int) list) (i: int) (j:int) : int =
        match m' with
        | [] -> 0
        | (i', j', value)::l -> if (i = i') && (j = j') then value
                             else matlook_aux l i j
    let matlook (m : matrix) (idx: int * int) : int =
        match m with | (m', (dim0, dim1)) ->
        match idx with | (i, j) ->
        if (i >= dim0) || (j >= dim1) then raise SparseVector.NoSuchElement
        else matlook_aux m' i j

    let get_col (m : (int*int*int) list) (col_num:int) (col_length:int): SparseVector.vector =
        SparseVector.combine (List.fold_left (fun result e ->
            match e with | (i, j, value) ->
                if col_num = j then result@[(i, value)]
                else result) [] m) col_length

    let rec foldrowl_aux (m: (int*int*int) list) (start: 'b) (f: 'b -> int * SparseVector.vector -> 'b) (dim0:int) (dim1:int) (idx:int): 'b =
        if dim1 = idx then start
        else
        let col = get_col m idx dim0 in
        let start' = f start (idx, col) in
            foldrowl_aux m start' f dim0 dim1 (idx + 1)

    let foldrowl (m : matrix) (start: 'b) (f: 'b -> int * SparseVector.vector -> 'b) : 'b =
        match m with | (m', (dim0, dim1)) ->
        foldrowl_aux m' start f dim0 dim1 0
end

