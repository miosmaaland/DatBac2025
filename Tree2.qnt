module Tree2{
    const CNodes: Set[int]
    const FNodes: Set[int]
    const Quorums: Set[Set[str]]

    def Nodes: Set[int] = union(CNodes, FNodes)
    
    assume QuorumAssumption = and {
        Quorums.forall(Q => Q.subseteq(Nodes)),
        tuples(Quorums, Quorums).forall(((Q1, Q2)) => CNodes.exists(n => Q1.intersect(Q2).contains(n))),
        Quorums.forall(Q => Nodes.powerset().forall(S => (Q.subseteq(S) => Quorums.contains(S)))),
        Quorums.contains(Nodes)
    }

    var votes: str -> Set[(int, int)]
    var lock: str -> str
    var max: str -> int

    type Block = str        
    const gen : Block             

    var Round: Block -> int               
    var Parent: Block -> Block   
    var Vote: Block -> int           

    def Rounds: Set[int] = union(Nat, Set(-1))

    action init = all {
        Round' = Map(gen -> 0),
        Parent' = Map(gen -> gen)
    }

    action AddBlock(b: Block ,p: Block,r: int): bool = all{
        Round' = Round.set(b, r),
        Parent' = Parent.set(b, p)
    }

    action VoteBlock(b: Block, v: Block, r: int): bool = all {
        Round' = Round.set(b,r),
        Vote' = Vote.set(b, v)
    }
}