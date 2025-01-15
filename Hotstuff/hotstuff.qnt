
module Hotstuff{

const CNodes: Set[int]
const FNodes: Set[int]
const Quorums: Set[Set[int]]
// Define Nodes as the union of CNodes and FNodes
val Nodes = Sets.union(CNodes, FNodes)
// Quorum Assumption
assume QuorumAssumption == 
    forall Q in Quorums {
        Sets.subset(Q, Nodes)
    } and
    forall Q1 in Quorums, Q2 in Quorums {
        exists n in CNodes {
            Sets.member(n, Sets.intersection(Q1, Q2))
        }
    } and
    forall Q in Quorums {
        forall S in Sets.subsets(Nodes) {
            Sets.subset(Q, S) ==> Sets.member(S, Quorums)
        }
    } and
    Sets.member(Nodes, Quorums)
}