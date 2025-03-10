module Hotstuff {
    // Define constants
    const CNodes: Set[int]
    const FNodes: Set[int]
    const Quorums: Set[Set[int]]

    // Define Nodes as the union of CNodes and FNodes
    val Nodes = union(CNodes, FNodes)

    // Define Blocks as a set of blocks
    const Blocks: Set[int] = Set(1, 2, 3)  // Example set of blocks

    // Define Rounds as a set of integers
    const Rounds: Set[int]

    // Define the genesis block explicitly
    const gen = Blocks.min()  // Assume gen is block 1

    
    assume gen.in(Blocks)  // Ensure gen is in Blocks

    var votes: (int -> Set[int])  // Define votes as a map from Blocks to the power set of Nodes
    var lock: (int -> int)        // Define lock as a map from Nodes to Blocks
    var max: (int -> int)         // Define max as a map from Nodes to Rounds
    var round: (int -> int)       // Define round as a map from Blocks to Rounds
    var parent: (int -> int)      // Define parent as a map from Blocks to Blocks

    // Define vars as a tuple
    val vars = (round, parent, max, lock, votes)

    // TypeOK predicate
    val TypeOK = and {
        round.in(Blocks.setOfMaps(Rounds)),  
        parent.in(Blocks.setOfMaps(Blocks)), 
        votes.in(Blocks.setOfMaps(Nodes.powerset())),
        lock.in(Nodes.setOfMaps(Blocks)),    
        max.in(Nodes.setOfMaps(Rounds))      
    }

    // Init action
    action Init = all {
        // Initialize lock: every node points to the genesis block
        lock' = Nodes.map(n => gen),

        // Initialize max: every node has max round 0
        max' = Nodes.map(n => 0),

        // Initialize round: all blocks have round -1, except genesis which has round 0
        round' = Blocks.map(b => if (b == gen) 0 else -1),

        // Initialize parent: every block is its own parent
        parent' = Blocks.map(b => b),

        // Initialize votes: all blocks have no votes, except genesis which has votes from all nodes
        votes' = Blocks.map(b => if (b == gen) Nodes else Set())
    }
}