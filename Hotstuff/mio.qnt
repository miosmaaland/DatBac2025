module hotstuff {
  // Import necessary modules
  import Quint.*
  import Set.*
  import Map.*

  // Constants: correct nodes, faulty nodes, and quorums
  const CNodes: Set[int]
  const FNodes: Set[int]
  const Quorums: Set[Set[int]]

  // Derived sets
  val Nodes = CNodes.union(FNodes)

  // Quorum assumption
  val QuorumAssumption =
    // All quorums are subsets of Nodes
    Quorums.forall(Q => Q.subsetOf(Nodes)) &&
    // Any two quorums intersect in a correct node
    Quorums.forall(Q1 =>
      Quorums.forall(Q2 =>
        CNodes.exists(n => Q1.has(n) && Q2.has(n)))) &&
    // Quorums are subset-closed
    Quorums.forall(Q =>
      Set(Nodes).forall(S =>
        Q.subsetOf(S) => Quorums.has(S)))) &&
    // The set of all nodes is a quorum
    Quorums.has(Nodes)

  // Assume the quorum properties hold
  assume(QuorumAssumption)

  // Type definitions
  type Block = int
  type Round = int
  const gen: Block // Genesis block

  // State variables
  var round: Block -> Round
  var parent: Block -> Block
  var votes: Block -> Set[int] // Set of nodes that voted for each block
  var lock: int -> Block       // Each node's locked block
  var max: int -> Round        // Each node's max round

  // State tuple for UNCHANGED expressions
  val vars = <<round, parent, votes, lock, max>>

  // Type invariant
  val TypeOK =
    round.in(Set(Block).all(b => round.get(b).in(Set(Round))) &&
    parent.in(Set(Block).all(b => parent.get(b).in(Set(Block)))) &&
    votes.in(Set(Block).all(b => votes.get(b).subsetOf(Nodes))) &&
    lock.in(Set(Nodes).all(n => lock.get(n).in(Set(Block)))) &&
    max.in(Set(Nodes).all(n => max.get(n).in(Set(Round))))

  // Initial state
  val Init =
    lock == Map(Nodes.map(n => (n, gen))) &&
    max == Map(Nodes.map(n => (n, 0))) &&
    round == Map(Set(Block).map(b => (b, -1)).put(gen, 0) &&
    parent == Map(Set(Block).map(b => (b, b))) &&
    votes == Map(Set(Block).map(b => (b, Set()))).put(gen, Nodes)

  // Predicates
  def Proposed(b) = b.in(Set(Block)) && round.get(b) != -1

  def Confirmed(b) = Proposed(b) && Quorums.has(votes.get(b))

  // Actions
  action propose(b, p, r) = {
    round' == round.put(b, r),
    parent' == parent.put(b, p)
  }

  action Propose(b, p, r) = {
    b.in(Set(Block)),
    !Proposed(b),
    Confirmed(p),
    r > round.get(p),
    propose(b, p, r),
    unchanged(lock, max, votes)
  }

  action vote(n, b) = {
    max' == max.put(n, round.get(b)),
    votes' == votes.put(b, votes.get(b).union(Set(n))),
    if (round.get(parent.get(parent.get(b))) > round.get(lock.get(n))) {
      lock' == lock.put(n, parent.get(parent.get(b)))
    } else {
      lock' == lock
    }
  }

  action Vote(n, b) = {
    Proposed(b),
    n.in(Nodes),
    (!n.in(CNodes)) || (
      round.get(b) > max.get(n) &&
      round.get(lock.get(n)) <= round.get(parent.get(b))
    ),
    vote(n, b),
    unchanged(round, parent)
  }

  // Next-state relation
  val Next =
    Set(Block).exists(b =>
      Set(Block).exists(p =>
        Set(Round).exists(r =>
          Propose(b, p, r)))) ||
    Set(Block).exists(b =>
      Nodes.exists(n =>
        Vote(n, b)))

  // Temporal specification
  val Spec = Init && always(Next, vars)
}