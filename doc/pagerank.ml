open Opertor;;

let if_sparse mat = match (iter ((0, 0),0) (fun (zeronum, num) (_, _, v) ->
                             if v = 0 then
                                Some (zeronum + 1, num +1)
                             else
                                Some (zeronum, num + 1))).fst with
                    | (zeronum, num) ->
                    if num = 0 then true else
                    if zeronum/num >= 0.5 then true
                    else false;;

let if_sparse vec = match (iter ((0, 0),0) (fun (zeronum, num) (_, v) ->
                             if v = 0 then
                                Some (zeronum + 1, num +1)
                             else
                                Some (zeronum, num))).fst with
                    | (zeronum, num) ->
                    if num = 0 then true else
                    if zeronum/num >= 0.5 then true
                    else false;;

(*refinement type like Liquid Haskell*)
{-@ type DenseMat = {v:Mat | if_sparse v = false} @-}
{-@ type SparseMat = {v:Mat | if_sparse v = true} @-}

{-@ type PageRankPair = {(v_bef:Vec, v_aft:Vec) | length v_bef = length v_aft @-}
{-@ type PageRankHistory = {(v_bef:Vec, v_aft:Vec, history:Vec) | length v_bef = length v_aft /\ length v_bef = length history @-}

Coercion (DenseMat -> SparseMat) = ...;;
Coercion (SparseMat -> DenseMat) = ...;;
Coercion (PageRankPair -> PageRankHistory) =
    fun pair -> match pair with
                | (v_bef, v_aft) -> (v_bef, v_aft, (make_vec length(v_bef)));;
Coercion (PageRankHistory -> PageRankPair) =
    fun his -> match his with
               | (v_bef, v_aft, _) -> (v_bef, v_aft);;

{-@ adj_mat::Mat, dump::Scalar, eps::Scalar, his::PageRankHistory @-}
let pr_approximate adj_mat dump eps his =
    match his with
    | (_, v_aft, history) ->
    let pr_vec_normal = normalize v_aft history in
    let active_adj_mat = split adj_mat history in
    let inactive_pr_vec = split pr_vec_normal history in
    let active_pr_vec_aft = vecxmat pr_vec_normal active_adj_mat in
    let pr_vec_new = concat inactive_pr_vec active_pr_vec_aft in
    let history_aft = update_history eps history v_aft pr_vec_new in
    PageRankHistory(v_aft, pr_vec_new, history_aft);;

{-@ adj_mat::Mat, dump::Scalar, eps::Scalar, his::PageRankPair @-}
let pr_accurate adj_mat dump eps pair =
    match pair with
    | (_, v_aft) ->
    let v1 = vecxmat v_aft adj_mat in
    let v2 = multi v1 dump in
    let v_new = plus v2 dump in
    PageRankPair(v_aft, v_new);;


Choice whichdecider (m:Mproutput) = let m' = unpack m in
    match type m' with
    | PageRankPair -> decider_pair
    | PageRankHistory -> decider_history;;

{-@ adj_mat::Mat @-}
let allocator adj_mat =
    PageRankPair((make_vector length(adj_mat) 0),
    (make_vector length(adj_mat) 1/length(adj_mat)));;

let rec mainloop adj_mat damp eps pr_output =
    let if_convergence = (whichdecider pr_output) pr_output in
    if if_convergence then pr_output_aft
    else let pr_output_aft = (whichalgo pr_output) adj_mat damp eps pr_output in
    mainloop adj_mat damp eps pr_output_aft;;

let rec main dataset_path eps damp =
    let Coercion adj_mat = load_file dataset_path in
    let pr_output = allocator adj_mat in
    mainloop adj_mat damp eps pr_output;;
