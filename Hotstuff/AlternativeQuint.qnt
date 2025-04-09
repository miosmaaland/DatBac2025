module Burger {
  type Node = { nodeId: int }
  val first_node : Node = {nodeId: 1}
  val second_node: Node = {nodeId: 2}
  val third_node: Node = {nodeId: 3}
  val faulty_node: Node = {nodeId: 4}
  
  type Block = { id: int, rounds: int, parentId: int }

  var parent: int -> Block
  var round: int
  var blocks : Block
  var nextId: int
  var roundId: int
  var lastId: int
  var voteId: int
  var blockers: Set[newBlock]
  var blockbyid: Set[int -> Block]
  var parents: Set[int -> Block]
  var lock : Node -> Block
  var max : nodes -> round
  var nodes: Set[Node]
  var correct_nodes: Set[Node]
  var votes: lastId -> Set[Node]

       

  val votingNode: Node = {nodeId: voteId}
  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    blockers' = Set(gen),
    blocks' = gen,
    parent' = Map(0 -> gen),
    parents' = Set(Map(0 -> gen)),
    round' = 0,
    nextId' = 0,
    voteId' = 1,
    lastId' = 0,
    roundId' = 0,
    nodes' = Set(first_node,second_node,third_node,faulty_node),
    correct_nodes' = Set(first_node, second_node, third_node),
    max' = Map(Set(first_node,second_node,third_node,faulty_node) -> 0),
    votes' = Map(gen -> Set(first_node, second_node, third_node)),
  }
  
  // val newBlock: Block = { id: nextId, rounds: roundId, parentId: lastId }
  // val currentVotes: Block = {id:nextId, rounds: roundId, parentId: lastId}

  // action voteForBlock(b: Block):bool = any {
  //  def votingNode = oneOf(correct_nodes),
  //  val currentVotes = votes.get(Set(b.id), Set(votingNode)),
  //   votes' = Map((b.id, b.rounds) -> currentVotes.put(voter)),
    
    // If majority of correct nodes voted for this block, add it to blocks
    // if (currentVotes.add(voter).size() >= 2) {
    //   blocks' = blocks.add(b),
    //   lastId' = b.id,
    //   roundId' = b.rounds
    // } else {
    //   blocks' = blocks
    // },
    
    // // Other variables remain unchanged unless specified
    // parent' = parent,
    // round' = round,
    // nextId' = nextId,
    // lock' = lock,
    // max' = max
  // }
  
  action Vote(b: int,p: int,r:int,n:Node):bool = all{
    n.in(correct_nodes),
    vote(b,p,r,n),
  }

  action vote(b: int,p: int,r:int,n:Node):bool = all {
    max' = Map(nodes -> r),
    votes' = Map(blockers -> Set(n)),
    addBlock(b, p, r)   
  }

  action step = all {
    blocks' = blocks,
    round' = round, 
    nextId' = nextId + 1
  } 

  action proposed(b) = {
    b.get(round != -1)
  }   

  def getID(b) =  {
    blockers.filter(x=> x.id == b)
  }
   def getNode(n) = {
     nodes.filter(x => x.nodeId == n)
   }

   pure def createBlock(b,p,r)={
    { id: b, rounds: r, parentId: p }
   }

  action addBlock(b, p, r) = all {
    // val newBlock: Block = { id: b, rounds: r, parentId: p },
    // r > blockbyid.get(p).rounds,
    parent' = Map(lastId -> createBlock(b,p,r)),
    blockers' = blockers.union(Set({ id: b, rounds: r, parentId: p })),
    parents' = parents.union(Set(Map(lastId -> { id: b, rounds: r, parentId: p }))),
    blocks' = { id: b, rounds: r, parentId: p },
    lastId' = p,
    nextId' = b, 
    roundId' = r, 
    r > roundId,
    p == { id: b, rounds: r, parentId: p }.id,
  }
// blockers.filter(b=>b.id = x)








// const voting_nodes : Set[(first_node,second_node,third_node,faulty_node)]

    


}