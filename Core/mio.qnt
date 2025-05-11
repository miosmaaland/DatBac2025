module Blockchain {
  type Node = int
  type Block = int

  pure val BLOCKS = 0.to(6)
  pure val NODES = Set(11, 22, 33, 44)
  pure val Cnodes = Set(11, 22, 33)
  pure val ROUNDS = 0.to(4)
  // Not sure if my quorum definition is correct, please check
  pure val QUORUMS = NODES.powerset().filter(s => s.intersect(Cnodes).size() >= 2)
  pure val gen = 0

  var parent: int -> Block
  var round: Block -> int
  /// votes stores the votes for every block, i.e.  which node has signed a block.
  var votes: Block -> nodes
  /// lock is the locked block for every node
  var lock : Node -> Block
  /// max is the max signed block for every node
  var max : Node -> int

  def proposed(b: Block): bool = {
    round.get(b) != -1
  }

  /// A block is confirmed if a quorum of nodes voted for the block, i.e. it has
  /// a certificate.
  def confirmed(block: Block): bool = {
    block.proposed() and votes.get(block).in(QUORUMS)
  }



  /// The propose action adds a new block to the tree. The parent must be
  /// confirmed, the new block gets a round larger than that of his parent.
  action propose(b: Block, p: Block, r: int): bool = all {
    b.in(BLOCKS),
    not(b.proposed()),
    p.confirmed(),
    r > round.get(p),
    round' = round.set(b, r),
    parent' = parent.set(b, p),
    lock' = lock,
    max' = max,
    votes' = votes,
  }


  /// Vote models one node voting (or signing) a block.
  /// The max and lock variables for the node are adjusted.
  action vote(n: Node, b: Block): bool = all {
    b.proposed(),
    n.in(NODES),

    // Below are the rules on signing, but they apply only to correct nodes.
    n.in(Cnodes) implies 
      round.get(b) > max.get(n)
      // Below is the updated rule for simplified hotstuff.
      and round.get(lock.get(n)) > round.get(parent.get(b)),
      
    max' = max.set(n, round.get(b)),
    votes' = votes.setBy(b, v => v.union(Set(n))),
    val grandparent = parent.get(parent.get(b))
    // If the grandparent of the new signed block has a
    // larger round than the current lock, the lock is updated.
    lock' = if (round.get(grandparent) > round.get(lock.get(n))) {
      lock.set(n, grandparent)
    } else {
      lock
    },
    round' = round,
    parent' = parent,
  }

  action init = all {
    // Initially all blocks but genesis have round -1.
    round' = BLOCKS.mapBy(b => -1).set(gen, 0),
    // All blocks are their own parent
    parent' = BLOCKS.mapBy(b => b),
    // No block is voted for, except genesis.
    // All nodes voted for the genesis block.
    votes' = BLOCKS.mapBy(b => Set()).set(gen, NODES),
    max' = NODES.mapBy(_ => 0),
    lock' = NODES.mapBy(_ => gen),
  }

  action step = {
    nondet b = BLOCKS.oneOf()
    any {
      nondet p = BLOCKS.oneOf()
      nondet r = ROUNDS.oneOf()
      propose(b, p, r),

      nondet n = NODES.oneOf()
      vote(n, b),
    }
  }

    run exampleTest = init
    .then(propose(4, 0, 1))
    .then(vote(11, 4))
    .then(vote(22, 4))
    .then(vote(33, 4))
    
}
