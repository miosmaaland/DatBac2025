module Tree2{
    const CNodes: Set[int]
    const FNodes: Set[int]
    const Quorums: Set[Set[str]]

    def Nodes: Set[int] = union(CNodes, FNodes)
    type Node = int
    
    // assume QuorumAssumption = and {
    //     Quorums.forall(Q => Q.subseteq(Nodes)),
    //     tuples(Quorums, Quorums).forall(((Q1, Q2)) => CNodes.exists(n => Q1.intersect(Q2).contains(n))),
    //     Quorums.forall(Q => Nodes.powerset().forall(S => (Q.subseteq(S) => Quorums.contains(S)))),
    //     Quorums.contains(Nodes)
    // }

    // assume QuoraAssumptions = 
    // Quorums.forall(Q => Q.subseteq(Nodes))
    // and Quorums.forall(Q1 => 
    //     Quorums.exists(Q2 => 
    //         CNodes.exists(n => n.in(Q1.intersect(Q2)))
    //     )
    // )
    // and Quorums.forall(Q => 
    //     Nodes.powerset().forall(S => (Q.subseteq(S).implies(S.in(Quorums))))
    // )
    // and Nodes.in(Quorums)

    var Vote: Block -> Set[Node]
    var Lock:  Node -> Block
    var Max: Node -> int

    type Block = str        
    const gen : Block             

    var Round: Block -> int               
    var Parent: Block -> Block            

    def Rounds: Set[int] = union(Nat, Set(-1))

    action init = all {
        Round' = Map(gen -> 0),
        Parent' = Map(gen -> gen),
        Lock' = Map(Nodes -> gen),
        Max' = Map(Nodes -> 0),
        Vote' = Map(gen -> Nodes)
    }

    action AddBlock(b: Block ,p: Block,r: int): bool = all {
        Round' = Round.set(b, r),
        Parent' = Parent.set(b, p)
    }

    action VoteBlock(b: Block, v: Node, r: int): bool = all {
        Round' = Round.set(b, r),
        Vote' = Vote.set(b, v)
    }

    action Proposed(b : Block): bool = all {
        b.exists(b => Round.contains(b) != -1)
    }

    action Confirmed(b: Block): bool = all {
        Proposed(b).exists(b => Quorums.contains(Vote(b)))
    }

    action propose(b: Block, p: Block, r: int): bool = all {
        Round' = Round.exclude(b != r),
        Parent' = Parent.exclude(p != b)
    }

    action Propose(b: Block, p: Block, r: int): bool = all {
        
    }
}