module TestModule{

    const CNodes: Set[int]
    const FNodes: Set[int]

    def Nodes: Set[int] = union(CNodes, FNodes)
    type Node = int

    // var Vote: Block -> Set[Node]
    // var Max: Node -> int

    // type Block = str        
    // const gen : Block             

    // var Round: Block -> int               
    // var Parent: Block -> Block            

    // def Rounds: Set[int] = union(Nat, Set(-1))

    action init = all {
        CNodes' = Map(Node -> 0),
        
    //     Round' = Map(gen -> 0),
    //     Parent' = Map(gen -> gen),
    }
    action proposed(b,r) = all{
        b.in(blocks), 
        r(b) == -1
    }
    
    }