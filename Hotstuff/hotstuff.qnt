module Hotstuff{

const CNodes: Set[int]
const FNodes: Set[int]
const Quorums: Set[Set[int]]



val Nodes = union(CNodes, FNodes)

assume QuoraAssumptions = and { 
    Quorums.forall(Q => Q.subseteq(Nodes)),
    Quorums.forall(Q1 => 
        Quorums.exists(Q2 => 
            CNodes.exists(n => n.in(Q1.intersect(Q2)))
        )
    ),
    Quorums.forall(Q => 
        Nodes.powerset().forall(S => (Q.subseteq(S).implies(S.in(Quorums))))
    ),
     Nodes.in(Quorums)
}
    var votes : int
    var lock : int
    var max : int
    var round : int
    var parent : int
    


    def vars = (round, parent, votes, lock, max)

    const Blocks : Set[int]
    const Rounds : Set[int]
    


   val TypeOK = and{
    round.in(Blocks.map(Rounds)),
    parent.in(Blocks.map(Blocks)),
    votes.in(Blocks.map(subseteq(Nodes))),
    lock.in(Nodes.map(Blocks)),
    max.in(Nodes.map(Rounds))

   }


   val Init = all{

    lock' = Nodes.mapBy(_ => gen),
    max' = Nodes.mapBy(_ => -1),
    round' = Blocks.mapBy(_ => -1) 

}
}
