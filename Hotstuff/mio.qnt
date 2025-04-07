module botstuff {
  // Types and constants
  type Node
  type Block
  type Round = int
  
  // Sets of nodes
  pure val CNodes: Set[Node]      // correct nodes
  pure val FNodes: Set[Node]      // faulty nodes
  pure val Nodes: Set[Node] = CNodes.union(FNodes)
  
  // Set of blocks
  pure val Blocks: Set[Block]     // all possible blocks
  
  // Rounds range
  pure val Rounds: Set[int]       // valid rounds
  
  // Quorums
  type Quorum = Set[Node]
  pure val Quorums: Set[Quorum]
  
  // Quorum assumptions - represented as assertions
  // We'll add them as invariants to check
  val quorumAssumption1 = Quorums.forall(Q => Q.subseteq(Nodes))
  val quorumAssumption2 = Quorums.forall(Q1 => 
    Quorums.forall(Q2 => 
      CNodes.exists(n => Q1.contains(n) and Q2.contains(n))
    )
  )
  val quorumAssumption3 = Quorums.forall(Q => 
    Nodes.powerset().forall(S => 
      (Q.subseteq(S) and S.subseteq(Nodes)) implies Quorums.contains(S)
    )
  )
  val quorumAssumption4 = Quorums.contains(Nodes)
  
  // Special genesis block
  pure val gen: Block
  assume { Blocks.contains(gen) }
  
  // State variables
  var round: Block -> Round = Map()     // round of each block
  var parent: Block -> Block = Map()    // parent of each block
  var votes: Block -> Set[Node] = Map() // votes for each block
  var lock: Node -> Block = Map()       // locked block for each node
  var max: Node -> Round = Map()        // max signed round for each node
  
  // Helper functions
  def proposed(b: Block): bool = {
    round.getOrElse(b, -1) != -1
  }
  
  def confirmed(b: Block): bool = {
    proposed(b) and Quorums.exists(Q => Q.subseteq(votes.getOrElse(b, Set())))
  }
  
  // Initialize state
  action init = all {
    // All nodes initially lock on genesis block
    lock' = Nodes.mapBy(n => gen),
    
    // Max round for all nodes is 0
    max' = Nodes.mapBy(n => 0),
    
    // All blocks except genesis have round -1
    round' = Blocks.mapBy(b => if (b == gen) 0 else -1),
    
    // All blocks are their own parent initially
    parent' = Blocks.mapBy(b => b),
    
    // No votes for any block except genesis
    votes' = Blocks.mapBy(b => if (b == gen) Nodes else Set())
  }
  
  // Propose a new block
  action propose(b: Block, p: Block, r: Round) = {
    // Requirements
    require(Blocks.contains(b));
    require(not proposed(b));
    require(confirmed(p));
    require(r > round.getOrElse(p, -1));
    
    // Update state
    all {
      round' = round.put(b, r),
      parent' = parent.put(b, p),
      unchanged(votes),
      unchanged(lock),
      unchanged(max)
    }
  }
  
  // Vote for a block
  action vote(n: Node, b: Block) = {
    // Requirements
    require(proposed(b));
    require(Nodes.contains(n));
    require(not CNodes.contains(n) or 
           (round.getOrElse(b, -1) > max.getOrElse(n, 0) and 
            round.getOrElse(lock.getOrElse(n, gen), -1) <= round.getOrElse(parent.getOrElse(b, gen), -1)));
    
    // Get parent and grandparent blocks
    val p = parent.getOrElse(b, gen);
    val gp = parent.getOrElse(p, gen);
    
    // Determine if lock should be updated
    val shouldUpdateLock = round.getOrElse(gp, -1) > round.getOrElse(lock.getOrElse(n, gen), -1);
    
    // Update state
    all {
      max' = max.put(n, round.getOrElse(b, -1)),
      votes' = votes.put(b, votes.getOrElse(b, Set()).union(Set(n))),
      lock' = if (shouldUpdateLock) lock.put(n, gp) else lock,
      unchanged(round),
      unchanged(parent)
    }
  }
  
  // System transitions
  action step = {
    nondet b: Block, p: Block, r: Round do {
      propose(b, p, r)
    } or {
      nondet n: Node, b: Block do {
        vote(n, b)
      }
    }
  }
  
  // Safety properties
  val typeOK = all {
    round.forall((b, r) => Blocks.contains(b) and Rounds.contains(r)),
    parent.forall((b, p) => Blocks.contains(b) and Blocks.contains(p)),
    votes.forall((b, ns) => Blocks.contains(b) and ns.subseteq(Nodes)),
    lock.forall((n, b) => Nodes.contains(n) and Blocks.contains(b)),
    max.forall((n, r) => Nodes.contains(n) and Rounds.contains(r))
  }
  
  // Run the protocol
  run init then step*
  
  // Invariants to check
  invariant TypeOK = typeOK
  invariant QuorumAssumptions = all {
    quorumAssumption1,
    quorumAssumption2, 
    quorumAssumption3,
    quorumAssumption4
  }
}
