module hotstuff {
  // Types and constants
  type Node
  type Block
  type Round = int
  
  // Sets of nodes
  pure val CNodes: Set[Node]      // correct nodes
  pure val FNodes: Set[Node]      // faulty nodes
  pure val Nodes: Set[Node] = CNodes.union(FNodes)
  pure val Blocks: Set[Block]
  
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
  
  // State variables
  var round: Block -> Round     // round of each block
  var parent: Block -> Block      // parent of each block
  var votes: Block -> Set[Node]  // votes for each block
  var lock: Node -> Block         // locked block for each node
  var max: Node -> Round         // max signed round for each node
  
  // Helper functions
  def proposed(b: Block): bool = {
    round.get(b, -1) != -1
  }
  
  def confirmed(b: Block): bool = {
    proposed(b) and Quorums.exists(Q => Q.subseteq(votes.get(b, Set())))
  }
  
  // Initialize state
  action init = all{
    // All nodes initially lock on genesis block
    lock' = Nodes.mapBy(n => gen),
    
    // Max round for all nodes is 0
    max' = Nodes.mapBy(n => 0),
    
    // All blocks except genesis have round -1
    round' = Map().put(gen, 0),
    
    // All blocks are their own parent initially
    
    parent' = Blocks.mapBy(b => b),
    
    // No votes for any block except genesis
    votes == Map().put(gen, Nodes),
  }
  
  // Propose a new block
  action propose(b: Block, p: Block, r: Round) : bool = {
    // Requirements
    Blocks.contains(b)
    require(not proposed(b));
    require(confirmed(p));
    require(r > round.get(p, -1));
    
    // Update state
    round = round.put(b, r);
    parent = parent.put(b, p);
  }
  
  // Vote for a block
  action vote(n: Node, b: Block) = {
    // Requirements
    require(proposed(b));
    require(Nodes.contains(n));
    require(not CNodes.contains(n) or 
           (round.get(b, -1) > max.get(n, 0) and 
            round.get(lock.get(n, gen), -1) <= round.get(parent.get(b, gen), -1)));
    
    // Update state
    max = max.put(n, round.get(b, -1));
    votes = votes.put(b, votes.get(b, Set()).union(Set(n)));
    
    // Update lock if needed
    if (round.get(parent.get(parent.get(b, gen), gen), -1) > round.get(lock.get(n, gen), -1)) {
      lock = lock.put(n, parent.get(parent.get(b, gen), gen));
    }
  }
  
  // System transitions
  action step() = {
    nondet b: Block, p: Block, r: Round do {
      propose(b, p, r);
    } or {
      nondet n: Node, b: Block do {
        vote(n, b);
      }
    }
  }
  

  
  // Safety properties
  val type_ok = all {
    round.forall((b, r) => Blocks.contains(b) and Rounds.contains(r)),
    parent.forall((b, p) => Blocks.contains(b) and Blocks.contains(p)),
    votes.forall((b, ns) => Blocks.contains(b) and ns.isSubsetOf(Nodes)),
    lock.forall((n, b) => Nodes.contains(n) and Blocks.contains(b)),
    max.forall((n, r) => Nodes.contains(n) and Rounds.contains(r))
  }
  
  // Run the protocol
  run init() then step()*
  
  // Invariants to check
  invariant typeOK = type_ok
  invariant quorumAssumptions = all {
    quorumAssumption1,
    quorumAssumption2, 
    quorumAssumption3,
    quorumAssumption4
  }
}