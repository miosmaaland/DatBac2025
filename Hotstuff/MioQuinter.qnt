module hotstuff {
  // Type definitions
  type Node = int
  type Block = int
  type Round = int

  
  val block : Block
  val node : Node
  // Constants
  const CNodes: Set[Node]
  const FNodes: Set[Node]
  const Quorums: Set[Set[Node]]

  // Derived sets
  val Nodes = CNodes.union(FNodes)
  

  // Variables
  var round: Block -> Round
  var parent: Block -> Block
  var votes: Block -> Set[Node]
  var lock: Node -> Block
  var max: Node -> Round

  // Genesis block
  val gen: Block = 0

  // Quorum assumptions
  val QuorumAssumption =
    // All quorums are subsets of Nodes
    Quorums.forall(Q => Q.subseteq(Nodes)) and
    // Any two quorums intersect in a correct node
    Quorums.forall(Q1 =>
      Quorums.forall(Q2 =>
        CNodes.exists(n => Q1.contains(n) and Q2.contains(n))
      )
    ) and
    // Quorums are subset-closed
    Quorums.forall(Q =>
      Set[Node].forall(S =>
        Q.subsetOf(S) implies Quorum.has(S)
      )
    ) and
    // The set of all nodes is a quorum
    Quorum.has(Nodes)

  // Type invariant
  val TypeOK =
    round.in(block -> round) and
    parent.in(block -> block) and
    votes.in(block -> Nodes) and
    lock.in(node -> block) and
    max.in(node -> round)

  // Initial state
  action init = all {
    // All nodes have genesis locked
    lock' = Map.fromSet(Nodes, _ => gen),
    // All nodes have max round 0
    max' = Map.fromSet(Nodes, _ => 0),
    // All blocks except genesis have round -1
    round' = Map.fromSet(Block, b => if b == gen then 0 else -1),
    // All blocks are their own parent initially
    parent' = Map.fromSet(Block, b => b),
    // Only genesis has votes (from all nodes)
    votes' = Map.fromSet(Block, b => if b == gen then Nodes else Set())
  }

  // Predicates
  def proposed(b) = b.in(Block) and round.get(b) != -1

  def confirmed(b) = proposed(b) and Quorums.exists(Q => votes.get(b) == Q)

  // Propose action
  action propose(b, p, r) = {
    require(
      b.in(Block) and
      not(proposed(b)) and
      confirmed(p) and
      r > round.get(p)
    ),
    round' = round.set(b, r),
    parent' = parent.set(b, p),
    // All other variables unchanged
    votes' = votes,
    lock' = lock,
    max' = max
  }

  // Vote action
  action vote(n, b) = {
    require(
      proposed(b) and
      n.in(Nodes) and
      (
        not(n.in(CNodes)) or
        (round.get(b) > max.get(n) and round.get(lock.get(n)) <= round.get(parent.get(b)))
      )
    ),
    max' = max.set(n, round.get(b)),
    votes' = votes.set(b, votes.get(b).union(Set(n))),
    lock' = 
      if round.get(parent.get(parent.get(b))) > round.get(lock.get(n)) then
        lock.set(n, parent.get(parent.get(b)))
      else
        lock,
    // Round and parent unchanged
    round' = round,
    parent' = parent
  }

  // Next state relation
  action Next =
    any {
      // Either propose a new block
      Block.exists(b =>
        Block.exists(p =>
          Round.exists(r =>
            propose(b, p, r)
        )
      ),
      // Or vote for a block
      Block.exists(b =>
        Nodes.exists(n =>
          vote(n, b)
        )
      )
    }

  // Temporal specification
  temporal Spec = init and always(Next or unchanged(round, parent, votes, lock, max))
}