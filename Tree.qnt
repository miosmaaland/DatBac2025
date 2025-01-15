module Tree {
    const Block: Set[str]         
    const gen: str             

    var Round: int               
    var Parent: int              

    
    def Rounds: Set[int] = union(Nat, Set(-1))

    
    assume ExistsBlock = Block != Block.empty

    
    def extend(A: Block.[str]): Block.[str] =
        Block.union(
            A,
            Block.filter(
                bc => Tuple(bc[0], parent[bc[1]]) in A,
                Block.cartesian(Block)
            )
        )

    const A0: Block.[str] = Block.filter(

    bc => bc[0] == bc[1],
    Block.cartesian(Block)
    )
        
    def ancestors(i: int): Block.[str] =
        if (i == 0) then A0
        else extend(ancestors(i - 1))

    def Ancestor(b: str, c: str): bool =
        Block.exists.(i => Tuple(b, c) in ancestors(i), Nat)
}