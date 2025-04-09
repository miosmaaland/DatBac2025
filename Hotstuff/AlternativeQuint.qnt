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
  var voteId: int
//   var blockers: Set[newBlock]
  var blockbyid: int -> Block
//   var parents: Set[int -> Block]
  var lock : Node -> Block
  var max : nodes -> round
  var nodes: Set[Node]
  var correct_nodes: Set[Node]
  var votes: int -> Set[Node]

       

  val votingNode: Node = {nodeId: voteId}
  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    // blockers' = Set(gen),
    blocks' = gen,
    parent' = Map(0 -> gen),
    // parents' = Set(Map(0 -> gen)),
    round' = 0,
    voteId' = 1,
    blockbyid' = Map(0 -> gen),
    nodes' = Set(first_node,second_node,third_node,faulty_node),
    correct_nodes' = Set(first_node, second_node, third_node),
    max' = Map(Set(first_node,second_node,third_node,faulty_node) -> 0),
    votes' = Map(0 -> Set(first_node, second_node, third_node)),
  }
  
//   val newBlock: Block = { id: 0, rounds: 0, parentId: 0 }
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
    votes' = Map(blockbyid.get(b).id -> Set(n)),
    addBlock(b, p, r)   
  }

//   action step = all {
//     blocks' = blocks,
//     round' = round, 
//     nextId' = nextId + 1
//   } 

   pure def createBlock(b,p,r)={
    { id: b, rounds: r, parentId: p }
   }

  action addBlock(b, p, r) = all {
    r > blockbyid.get(p).rounds,
    parent' = parent.put(blockbyid.get(p).id, createBlock(b,p,r)),
    blockbyid' = blockbyid.put(b, createBlock(b,p,r)),
  }


}