module Tree {
    type Block = str        
    const gen : Block             

    var Round: Block -> int               
    var Parent: Block -> Block              

    
    def Rounds: Set[int] = union(Nat, Set(-1))

    
    // assume ExistsBlock = Block != Block.empty

    action init = all {
        Round' = Map(gen -> 0),
        Parent' = Map(gen -> gen)
    }

    action AddBlock(b: Block ,p: Block,r: int): bool = all{
        Round' = Round.set(b, r),
        Parent' = Parent.set(b, p)
    }



    
    // def extend(A: Block.[str]): Block.[str] =
    //     Block.union(
    //         A,
    //         Block.filter(
    //             bc => Tuple(bc[0], parent[bc[1]]) in A,
    //             Block.cartesian(Block)
    //         )
    //     )

    // const A0: Block.[str] = Block.filter(

    // bc => bc[0] == bc[1],
    // Block.cartesian(Block)
    // )
        
    // def ancestors(i: int): Block.[str] =
    //     if (i == 0) then A0
    //     else extend(ancestors(i - 1))

    // def Ancestor(b: str, c: str): bool =
    //     Block.exists.(i implies Tuple(b, c) assume ancestors(i), Nat)
}