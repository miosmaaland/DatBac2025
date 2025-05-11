module BlockChain {
  type IDBLOCK = int 
  type Node = int
  type Round = int
  type ParentId = int

  pure val correct_nodes = Set(1, 2, 3)
  pure val faulty_node = Set(4)
  pure val NODES = Set(1, 2, 3, 4)
  pure val BLOCKID = 0.to(5)
  pure val ROUNDS = 0.to(5)
  pure val PARENT = 0.to(5)

  type Block = { id: int, rounds: Round, parentId: ParentId }

  var parent: int -> Block
  var round: int
  var blocks : Block
  var BlockID: Set[IDBLOCK]
  var blockbyid: int -> Block
  var lock : Node -> IDBLOCK
  var max : Node -> round
  var votes: int -> Set[Node]
  var BlockChain: Set[Block]

  pure val QUORUMS = NODES.powerset().filter(s => s.intersect(correct_nodes).size() >= 3)
       
  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  def confirmed(b)={
    votes.get(b).size() >=3 
  }

  action init = all {
    blocks' = gen,
    parent' = Map(0 -> gen),
    BlockID' = Set(0),
    round' = 0,
    blockbyid' = Map(0 -> gen),
    votes' = Map(0 -> NODES),
    max' = NODES.mapBy(_ => 0),
    lock' = NODES.mapBy(_ => 0),
    BlockChain' = Set(gen),
  }
  
  action Vote(b: int, n:Node):bool = all{
    n.in(correct_nodes),
    blockbyid.get(b).rounds > max.get(n),
    max' = max.set(n, blockbyid.get(b).rounds),
    b.in(BlockID),

    val grandparent = blockbyid.get(blockbyid.get(b).parentId).parentId
    lock' = if (blockbyid.get(grandparent).rounds > blockbyid.get(lock.get(n)).rounds) {
      lock.set(n, grandparent)
    } else {
      lock
    },
    
    round' = round,
    BlockID' = BlockID,
    parent' = parent,
    blocks' = blocks,
    blockbyid' = blockbyid,
    BlockChain' = BlockChain,

    vote(b,n)
  }

  action vote(b: int, n:Node):bool = all {
    votes' = votes.setBy(blockbyid.get(b).id, vs => vs.union(Set(n))),      
  }

  action Proposed(b,p,r) = all {
    not(BlockChain.exists(x=>x.id==b)),
    BlockChain.exists(x=>x.id==p),
    r == blockbyid.get(b).rounds,
    p == blockbyid.get(b).parentId,
    confirmed(b),
    b.in(BlockID),
    BlockChain' = BlockChain.union(Set(blockbyid.get(b))),

    round' = round,
    lock' = lock,
    max' = max,
    blocks' = blocks,
    BlockID' = BlockID,
    votes' = votes,
    parent' = parent,
    blockbyid' = blockbyid,
  }


   pure def new(b,p,r)={
    { id: b, rounds: r, parentId: p }
   }

  action addBlock(b, p, r) = all {
    not(b.in(BlockID)),
    r > blockbyid.get(p).rounds,
    parent' = parent.put(blockbyid.get(p).id, new(b,p,r)),
    blockbyid' = blockbyid.put(b, new(b,p,r)),
    BlockID' = BlockID.union(Set(b)),
    votes' = votes.put(b, Set()),
    round' = r,
    lock' = lock,
    max' = max,
    blocks' = blocks,
    BlockChain' = BlockChain,
  }

    //b.in(blockbyid.get(b)),
    // votes' = betterBlock.mapBy(x => Set()).set(0, NODES),
    // Non-deterministic step action for simulation
  // action step = {
  //   nondet b = BLOCKID.oneOf()
  //   any {
  //     // Either propose a new block
  //     nondet p = PARENT.oneOf()
  //     nondet r = ROUNDS.oneOf()
  //     addBlock(b, p, r),

  //     // Or have a node vote for a block
  //     nondet n = NODES.oneOf()
  //     vote(n, b)
  //   }
  // }



  
      run TestCases = init 
          .then(addBlock(1, 0, 1))
          .then(addBlock(2, 1, 2))
          .then(addBlock(3, 1, 3))
          .then(addBlock(4, 3, 4))
          .then(addBlock(5, 4, 5))
          .then(Vote(1, 1))
          .then(Vote(1, 2))
          .then(Vote(1, 3))
          .then(Proposed(1, 0, 1))
          .then(Vote(2, 1))
          .then(Vote(2, 2))
          .then(Vote(3, 1))
          .then(Vote(3, 2))
          .then(Vote(3, 3))
          .then(Proposed(3, 1, 3))
          .then(Vote(4, 1))
          .then(Vote(4, 2))
        
}