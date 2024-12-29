module Utils

open System.Text.RegularExpressions

module Text =
    let split (separator: string) (text: string) = text.Split(separator)

    let splitOnce (separator: string) (text: string) =
        let pos = text.IndexOf(separator)

        if pos < 0 then
            invalidArg "text" "Separator not found in string"
        else
            (text.Substring(0, pos), text.Substring(pos + separator.Length))

    let lines text = split "\n" text

    let indexesOf (subString: string) (text: string) =
        Regex.Matches(text, Regex.Escape(subString)) |> Seq.map _.Index

    let chars (text: string) = text.ToCharArray()

    let charToInt (c: char) = int c - int '0'

module Array =
    let flat2Darray array2D =
        seq {
            for x in [ 0 .. (Array2D.length1 array2D) - 1 ] do
                for y in [ 0 .. (Array2D.length2 array2D) - 1 ] do
                    yield array2D.[x, y]
        }

module Func =
    let tryF f x =
        try
            Some(f x)
        with _ ->
            None

    let repeat n f = List.replicate n f |> List.reduce (>>)

    let flip f x y = f y x

[<RequireQualifiedAccess>]
module BinomialHeapPQ =
    //  type 'a treeElement = Element of uint32 * 'a
    type 'a treeElement =
        class
            val k: uint32
            val v: 'a
            new(k, v) = { k = k; v = v }
        end

    type 'a tree = Node of uint32 * 'a treeElement * 'a tree list

    type 'a heap = 'a tree list

    type 'a outerheap =
        | HeapEmpty
        | HeapNotEmpty of 'a treeElement * 'a heap

    let empty = HeapEmpty

    let isEmpty =
        function
        | HeapEmpty -> true
        | _ -> false

    let inline private rank (Node(r, _, _)) = r

    let inline private root (Node(_, x, _)) = x

    exception Empty_Heap

    let getMin =
        function
        | HeapEmpty -> None
        | HeapNotEmpty(min, _) -> Some min

    let rec private findMin heap =
        match heap with
        | [] -> raise Empty_Heap //guarded so should never happen
        | [ node ] -> root node, []
        | topnode :: heap' ->
            let min, subheap = findMin heap' in
            let rtn = root topnode

            match subheap with
            | [] -> if rtn.k > min.k then min, [] else rtn, []
            | minnode :: heap'' ->
                let rmn = root minnode

                if rtn.k <= rmn.k then
                    rtn, heap
                else
                    rmn, minnode :: topnode :: heap''

    let private mergeTree (Node(r, kv1, ts1) as tree1) (Node(_, kv2, ts2) as tree2) =
        if kv1.k > kv2.k then
            Node(r + 1u, kv2, tree1 :: ts2)
        else
            Node(r + 1u, kv1, tree2 :: ts1)

    let rec private insTree (newnode: 'a tree) heap =
        match heap with
        | [] -> [ newnode ]
        | topnode :: heap' ->
            if (rank newnode) < (rank topnode) then
                newnode :: heap
            else
                insTree (mergeTree newnode topnode) heap'

    let insert k v =
        let kv = treeElement (k, v) in
        let nn = Node(0u, kv, [])

        function
        | HeapEmpty -> HeapNotEmpty(kv, [ nn ])
        | HeapNotEmpty(min, heap) ->
            let nmin = if k > min.k then min else kv
            HeapNotEmpty(nmin, insTree nn heap)

    let rec private merge' heap1 heap2 = //doesn't guaranty minimum tree node as head!!!
        match heap1, heap2 with
        | _, [] -> heap1
        | [], _ -> heap2
        | topheap1 :: heap1', topheap2 :: heap2' ->
            match compare (rank topheap1) (rank topheap2) with
            | -1 -> topheap1 :: merge' heap1' heap2
            | 1 -> topheap2 :: merge' heap1 heap2'
            | _ -> insTree (mergeTree topheap1 topheap2) (merge' heap1' heap2')

    let merge oheap1 oheap2 =
        match oheap1, oheap2 with
        | _, HeapEmpty -> oheap1
        | HeapEmpty, _ -> oheap2
        | HeapNotEmpty(min1, heap1), HeapNotEmpty(min2, heap2) ->
            let min = if min1.k > min2.k then min2 else min1
            HeapNotEmpty(min, merge' heap1 heap2)

    let rec private removeMinTree =
        function
        | [] -> raise Empty_Heap // will never happen as already guarded
        | [ node ] -> node, []
        | t :: ts ->
            let t', ts' = removeMinTree ts
            if (root t).k <= (root t').k then t, ts else t', t :: ts'

    let deleteMin =
        function
        | HeapEmpty -> HeapEmpty
        | HeapNotEmpty(_, heap) ->
            match heap with
            | [] -> HeapEmpty // should never occur: non empty heap with no elements
            | [ Node(_, _, heap') ] ->
                match heap' with
                | [] -> HeapEmpty
                | _ ->
                    let min, _ = findMin heap'
                    HeapNotEmpty(min, heap')
            | _ :: _ ->
                let Node(_, _, ts1), ts2 = removeMinTree heap
                let nheap = merge' (List.rev ts1) ts2 in
                let min, _ = findMin nheap
                HeapNotEmpty(min, nheap)

    let reinsertMinAs k v pq = insert k v (deleteMin pq)
