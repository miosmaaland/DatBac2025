
module Hotstuff{

const CNodes: Set[int]
const FNodes: Set[int]
const Quorums: Set[Set[int]]

val Nodes = union(CNodes, FNodes)

assume QuoraAssumptions = 
    Quorums.forall(Q => Q.subseteq(Nodes))
    and Quorums.forall(Q1 => 
        Quorums.exists(Q2 => 
            CNodes.exists(n => n.in(Q1.intersect(Q2)))
        )
    )
    and Quorums.forall(Q => 
        Nodes.powerset().forall(S => (Q.subseteq(S).implies(S.in(Quorums))))
    )
    and Nodes.in(Quorums)

    var votes : int
    var lock : int
    var max : int

    def vars = (round, parent, votes, lock, max)


   action TypeOk = all{
    val round: map(Blocks, Rounds),
    val parent : map(Blocks,Blocks),
    val votes : map(Blocks.subseteq(Nodes)),
    val lock : map(Nodes, Blocks),
    val max : map(Nodes,Rounds)

    

    
   }





}
