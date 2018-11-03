module type Vector =
sig
    type vector
    val to_vector : int list -> vector
    exception NoSuchElement
    val look: vector -> int -> int
    val foldl: vector -> 'b -> ('b -> int * int -> 'b) -> 'b
end

module SparseVector: Vector =
    struct
    type vector = (int * int) list
    let rec to_vector_aux (l:int list) (idx: int): vector =
        match l with
        | [] -> []
        | h :: t -> if h = 0 then to_vector_aux t idx
                    else (idx, h) :: (to_vector_aux t (idx + 1))
    let to_vector (l:int list): vector =
        to_vector_aux l 0
    exception NoSuchElement
    let rec look (v : vector) (idx: int) : int =
        match v with
        | [] -> raise NoSuchElement
        | (i, h) :: t -> if i = idx then h
                    else look t idx
    let rec foldl (v : vector) (start: 'b) (f: 'b -> int * int -> 'b): 'b =
        match v with
        | [] -> start
        | (i, h) :: t -> foldl t (f start (i, h)) f
end

module DenseVector: Vector =
    struct
    type vector = int list
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
    val look: matrix -> int * int -> int
    val foldcoll: matrix -> 'b -> ('b -> int * vector -> 'b) -> 'b
    val foldrowl: matrix -> 'b -> ('b -> int * vector -> 'b) -> 'b
end

module DenseMatrix: Matrix =
    struct
    include DenseVector
    type matrix = int list list
    let to_matrix (m:int list list) : matrix = m

    let rec look_row (line : int list) (idx: int) : int =
        match line with
        | [] -> raise DenseVector.NoSuchElement
        | h :: t -> if idx <= 0 then h
                    else look_row t (idx - 1)
    let rec look (m : matrix) (idx: int * int) : int =
        match idx with
        | (i, j) -> match m with
                    | [] -> raise DenseVector.NoSuchElement
                    | h :: t -> if i <= 0 then
                                    look_row h j
                                else look t (i-1, j)
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
        if idx = 3 then start
        else let col = get_col m idx in
             let start' = f start (idx, col) in
             foldrowl_aux m start' f (idx+1) length
    let foldrowl (m : matrix) (start: 'b) (f: 'b -> int * DenseVector.vector -> 'b) : 'b =
        match m with
        | [] -> start
        | h :: _ -> foldrowl_aux m start f 0 (List.length h)
end


