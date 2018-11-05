(*
A  ------ a
B  ------ b
C  ------ c
A, B, C: rowIndex
a, b, c: colIndex

cost_mat =
  | a | b | c
--------------
A |   |   |
--------------
B |   |   |
--------------
C |   |   |
*)

(* Argument:
cost_mat: rowIndex -> colIndex -> cost
jump_table: colIndex -> Option rowIndex | rowIndex -> Option colIndex
free_cowindex : cowIndex (*jump_table free_cowindex = None /\
    forall col_index: colIndex, jump_table col_index <> free_cowindex*)
toscan: colIndex set (* The unscanned colnode set*)
cost_sum: cost (* the sum of cost of before path *)
dp_distance: colIndex -> cost (* the the min distance from j to the scanned graph *)
*)


let get_SCAN TODO dp_distance =
    if SCAN = [] then
        let min_d = min_distance TODO dp_distance in
        foldl TODO [] (fun SCAN_new colindex ->
            if dp_distance colindex = min_d
            then
                SCAN_new@[colindex]
            else
                SCAN_new)
    else
        SCAN

(* SCAN != [] /\
forall In h SCAN, y[h] != None *)
let choose_a_colnode SCAN READY y =
    foldl SCAN NONE (fun _ colindx ->
        ((y colindx), (SCAN - colindx), (READY + colindx)))

(* TODO != [] *)
let phase1 SCAN TODO dp_distance y =
    if SCAN = [] then
        let SCAN = get_SCAN TODO dp_distance in
        let TODO = TODO - SCAN in
        let unassigned_colindx = foldl SCAN None (fun unassigned_colindx colindex ->
            if y colindex = None
            then colindex
            else unassigned_colindx) in
        match unassigned_colindx with
        | Some colindex -> inl colindex
        | None -> inr (SCAN, TODO)
    else
        inr (SCAN, TODO)

(*

i1 ---- j1
   \
    \
     \
      \
i  ---- j
*)

let z_path dij di1j di1j1 SCAN =
    if di1j1 + dij = di1j then
            match y j with
            | None -> inl j
            | _ -> inr (SCAN - j)

let phase2 cost_mat jump_table TODO SCAN i cost_sum dp_distance =
    foldl TODO (inr (dp_distance, jump_table, SCAN)) (fun m j ->
        match m with
        | inl j -> inl (j, dp_distance)
        | inr (dp_distance, jump_table, SCAN) ->
        let cost_sum' = cost_sum + cost_mat i j in
        let m = z_path (cost_mat i j) (dp_distance j) cost_sum in
        match z with
        | inl j -> inl (j, update dp_distance (j, cost_sum'))
        | inr SCAN' ->
        if cost_sum' < dp_distance j then
            let dp_distance' = update dp_distance (j, cost_sum') in
            let jump_table' = update jump_table (j, i) in
            inr (dp_distance', jump_table', SCAN', cost_sum'));;

let min_distance TODO dp_distance =
    foldl TODO None (fun min_d colindex ->
        Some (min (dp_distance colindex) min_d))

let rec dijkstra_shortpath i SCAN TODO READY dp_distance y cost_sum jump_table cost_mat =
    let m = phase1 SCAN TODO dp_distance y in
    match m with
    | inl j -> (j, dp_distance, READY)
    | inr (SCAN, TODO) ->
    let (i, SCAN', READY') = choose_a_colnode SCAN READY y in
    let m = phase2 cost_mat jump_table TODO SCAN' i cost_sum dp_distance in
    match m with
    | inl (j, dp_distance') -> (j, dp_distance, READY')
    | inr (dp_distance', jump_table', SCAN'', cost_sum') ->
    let TODO' = TODO - SCAN'' in
    dijkstra_shortpath SCAN'' TODO' READY' dp_distance' y cost_sum' jump_table' cost_mat

let augmentation dp_distance READY y x cost_mat i j jump_table =
    let _, y', x', cost_mat' =
    foldl READY (dp_distance, y, x, cost_mat) (fun (dp_distance, y, x, cost_mat) k ->
        let diff = (dp_distance k) - (dp_distance j) in
        let cost_mat' = cost_mat - (make_mat None k dif) in
        let (x', y') = update jump_table (x, y)) in
    (y', x', cost_mat')

let argument_the_solution y x cost_mat =
    foldl free_rowindex (y, x, cost_mat) (fun (y, x, cost_mat) i ->
        let READY = [] in
        let SCAN = [] in
        let TODO = foldcoll cost_mat [] (fun collist (j, _) -> collist@[j]) in
        let dp_distance = cost_mat i in
        let jump_table = (_, i) in
        let j, dp_distance', READY' = dijkstra_shortpath SCAN TODO READY dp_distance y cost_sum jump_table cost_mat in
        augmentation dp_distance' READY' y x cost_mat i j jump_table)

let get_min_cost line =
    foldl line None (fun m (index, cost) ->
        match m with
        | None -> (index, cost)
        | Some (index', cost') ->
        if cost <= cost'
        then Some (index, cost)
        else Some (index', cost'))

let get_snd_min_cost line minj =
    foldl line None (fun m (index, cost) ->
        if index = minj then m else
        match m with
        | None -> (index, cost)
        | Some (index', cost') ->
        if cost <= cost'
        then Some (index, cost)
        else  Some (index', cost'))

let column_reduction cost_mat x y=
    let matches = empty in
    foldcoll cost_mat (cost_mat, matches, x, y, []) (fun (cost_mat, matches, x, y, v) (j, line) ->
        let (i, mincost) = get_min_cost line in
        let v' = update v (i, mincost) in
        let cost_mat' = cost_mat - (make_mat i None mincost) in
        let matches' = update matches (i, (matches i) + 1) in
        let (x', y') =
            if (x i = None) && (y j = None)
            then update (x, y) (i, j)
            else (x, y) in
        (cost_mat', matches', x', y', v'))

let reduction_transfer cost_mat x v matches =
    foldl cost_mat (cost_mat, v) (fun _ (i, line) ->
        if matches i = 1
        then
            let (minj, mincost) = get_min_cost line in
            let j = x i in
            let v' = update v (j, (v j - mincost)) in
            let cost_mat' = cost_mat + (make_mat None j (v j - mincost)) in
            (cost_mat', v')
        else
            (cost_mat, v))

let rec augmenting_row_reduction_aux freerowlist v cost_mat x y=
    match freerowlist with
    | [] -> (freerowlist v cost_mat x y)
    | i :: freerowlist' ->
    let (minj, mincost) = get_min_cost (cost_mat i) in
    let (subminj, submincost) = get_snd_min_cost row mincost in
    let v' = update v (j, (v j - (submincost - mincost))) in
    let cost_mat' = cost_mat + (make_mat None j (v j - (submincost - mincost))) in
    let (j, jcost) =
        if mincost = submincost && subminj <> minj
        then (subminj, submincost)
        else (minj, mincost) in
    let i' = y j in
    let (x', y') = update (x, y) (i, j) in
    let freerowlist'' =
        match i' with
        | None -> freerowlist'
        | Some i'' -> if mincost = submincost
                      then i'' :: freerowlist'
                      else freerowlist'@[i''] in
    augmenting_row_reduction_aux freerowlist'' v' cost_mat' x' y'

let augmenting_row_reduction v cost_mat x y =
    let freerowlist = get_free x in
    augmenting_row_reduction_aux freerowlist v cost_mat x y

let main cost_mat =
    let x = [] in
    let y = [] in
    let (cost_mat1, matches, x', y', v) = column_reduction cost_mat x y in
    let (cost_mat2, v') = reduction_transfer cost_mat1 x' v matches in
    let (freerowlist v'' cost_mat3 x'' y'') = augmenting_row_reduction v' cost_mat2 x' y' in
    let (resulty, resultx, _) = argument_the_solution y'' x'' cost_mat3 in
    (resulty, resultx)


