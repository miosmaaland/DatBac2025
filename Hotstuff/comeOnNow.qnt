module hotstuff {
  // Constants
  const CNodes: Set[str]
  const FNodes: Set[str]
  const Quorums: Set[Set[str]]

  // Derived sets
  val Nodes = CNodes.union(FNodes)

  // Quorum assumption
  val QuorumAssumption = and {
    // All quorums are subsets of Nodes
    Quorums.forall(Q => Q.subseteq(Nodes)),
    // Any two quorums intersect in a correct node
    Quorums.forall(Q1 =>
      Quorums.forall(Q2 =>
      CNodes.exists(n => Q1.has(n) && Q2.has(n)))),
    // Quorums are subset-closed
    Quorums.forall(Q =>
      Set(Nodes).forall(S =>
        Q.subsetOf(S) => Quorums.has(S))),
    // The set of all nodes is a quorum
    Quorums.has(Nodes)
  }

  assume QuorumAssumption

  // Type aliases
  type Block = int
  type Round = int
  const gen: Block = 0  // Genesis block

  // State variables
  var round: Block -> Round
  var parent: Block -> Block
  var votes: Block -> Set[str]
  var lock: str -> Block
  var max: str -> Round

  // State tuple for UNCHANGED
  val vars = (round, parent, votes, lock, max)

  // Type invariant
  val TypeOK = and {
    round.in(Block.setOfMaps(Round)),
    parent.in(Block.setOfMaps(Block)),
    votes.in(Block.setOfMaps(Set(str))),
    lock.in(str.setOfMaps(Block)),
    max.in(str.setOfMaps(Round))
  }

  // Initial state
  action Init = all {
    round' == Block.mapBy(_ => -1).set(gen, 0),
    parent' == Block.mapBy(b => b),
    votes' == Block.mapBy(_ => Set()).set(gen, Nodes),
    lock' == Nodes.mapBy(_ => gen),
    max' == Nodes.mapBy(_ => 0)
  }

  // Predicates
  def Proposed(b) = b.in(dom(round)) && round.get(b) != -1

  def Confirmed(b) = Proposed(b) && Quorums.has(votes.get(b))

  // Actions
  action propose(b, p, r) = all {
    round' == round.set(b, r),
    parent' == parent.set(b, p)
  }

  action Propose(b, p, r) = all {
    b.in(Block),
    !Proposed(b),
    Confirmed(p),
    r > round.get(p),
    propose(b, p, r),
    unchanged(lock, max, votes)
  }

  action vote(n, b) = all {
    max' == max.set(n, round.get(b)),
    votes' == votes.set(b, votes.get(b).union(Set(n))),
    if (round.get(parent.get(parent.get(b))) > round.get(lock.get(n))) {
      lock' == lock.set(n, parent.get(parent.get(b)))
    } else {
      lock' == lock
    }
  }

  action Vote(n, b) = all {
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
  action Next = any {
    all {
      nondet b = oneOf(Block)
      nondet p = oneOf(Block)
      nondet r = oneOf(Round)
      Propose(b, p, r)
    },
    all {
      nondet b = oneOf(Block)
      nondet n = oneOf(Nodes)
      Vote(n, b)
    }
  }

  // Temporal specification
  val Spec = Init && []Next(_)
}