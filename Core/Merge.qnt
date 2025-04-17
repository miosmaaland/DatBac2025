module Merge {
  // Types
  type Node = int
  type Block = int
  type Round = int

  // Constants
  pure val BLOCKS = 0.to(6)
  pure val ROUNDS = 0.to(4)
  pure val NODES = Set(11, 22, 33, 44)  // Example node IDs
  pure val CNODES = Set(11, 22, 33)     // Correct nodes
  pure val FNODES = Set(44)             // Faulty node
  pure val gen = 0                       // Genesis block
  
  // Quorum definition (at least 2 correct nodes must agree)
  pure val QUORUMS = NODES.powerset().filter(s => s.intersect(CNODES).size() >= 2)

  // State variables
  var parent: Block -> Block
  var round: Block -> Round
  var votes: Block -> Set[Node]
  var lock: Node -> Block
  var max: Node -> Round

  // Helper predicates
  def proposed(b: Block): bool = {
    b.in(BLOCKS) and round.get(b) != -1
  }

  def confirmed(b: Block): bool = {
    proposed(b) and QUORUMS.exists(q => q.subseteq(votes.get(b)))
  }

  // Actions
  action init = all {
    // Initialize rounds (genesis at 0, others at -1)
    round' = BLOCKS.mapBy(b => -1).set(gen, 0),
    // All blocks initially point to themselves
    parent' = BLOCKS.mapBy(b => b),
    // Only genesis has votes initially (from all nodes)
    votes' = BLOCKS.mapBy(b => Set()).set(gen, NODES),
    // All nodes start with genesis locked
    lock' = NODES.mapBy(_ => gen),
    // All nodes start with max round 0 (genesis)
    max' = NODES.mapBy(_ => 0)
  }

  action propose(b: Block, p: Block, r: Round): bool = all {
    // Preconditions
    b.in(BLOCKS),
    not(proposed(b)),
    confirmed(p),
    r > round.get(p),
    
    // Effects
    round' = round.set(b, r),
    parent' = parent.set(b, p),
    // Other variables unchanged
    votes' = votes,
    lock' = lock,
    max' = max
  }

  action vote(n: Node, b: Block): bool = all {
    // Preconditions
    proposed(b),
    n.in(NODES),
    
    // Voting rules (only enforced for correct nodes)
    n.in(CNODES) implies all {
      round.get(b) > max.get(n),
      round.get(lock.get(n)) <= round.get(parent.get(b))
    },
    
    // Effects
    votes' = votes.setBy(b, vs => vs.union(Set(n))),
    max' = max.set(n, round.get(b)),
    

    
    // Update lock if grandparent has higher round
    val grandparent = parent.get(parent.get(b))
    lock' = if (round.get(grandparent) > round.get(lock.get(n))) {
      lock.set(n, grandparent)
    } else {
      lock
    },
    
    // Other variables unchanged
    round' = round,
    parent' = parent
  }

  // Non-deterministic step action for simulation
  action step = {
    nondet b = BLOCKS.oneOf()
    any {
      // Either propose a new block
      nondet p = BLOCKS.oneOf()
      nondet r = ROUNDS.oneOf()
      propose(b, p, r),

      // Or have a node vote for a block
      nondet n = NODES.oneOf()
      vote(n, b)
    }
  }

  // Example test run
  run exampleTest = init
    .then(propose(1, 0, 1))
    .then(vote(11, 1))
    .then(vote(22, 1))
    .then(vote(33, 1))
    .then(propose(2, 1, 2))
}