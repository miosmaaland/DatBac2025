module Burger {
  type Node = { nodeId: int }
  val first_node : Node = {nodeId: 1}
  val second_node: Node = {nodeId: 2}
  val third_node: Node = {nodeId: 3}
  val faulty_node: Node = {nodeId: 4}

  type Round = int
  type ParentId = int
  
  type Block = { id: int, rounds: Round, parentId: ParentId }

  var parent: int -> Block
  var round: int
  var blocks : Block
  var voteId: int

  var blockbyid: int -> Block

  var lock : Node -> Block
  var max : nodes -> round
  var nodes: Set[Node]
  var correct_nodes: Set[Node]
  var votes: int -> Set[Node]

       

  val votingNode: Node = {nodeId: voteId}
  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    
    blocks' = gen,
    parent' = Map(0 -> gen),
    
    round' = 0,
    voteId' = 1,
    blockbyid' = Map(0 -> gen),
    nodes' = Set(first_node,second_node,third_node,faulty_node),
    correct_nodes' = Set(first_node, second_node, third_node),
    max' = Map(Set(first_node,second_node,third_node,faulty_node) -> 0),
    votes' = Map(0 -> Set(first_node, second_node, third_node)),
  }
  

  
  action Vote(b: int,p: int,r:int,n:Node):bool = all{
    n.in(correct_nodes),
    vote(b,p,r,n),
  }

  action vote(b: int,p: int,r:int,n:Node):bool = all {
    max' = Map(nodes -> r),
    votes' = Map(blockbyid.get(b).id -> Set(n)),
    addBlock(b, p, r)   
  }


   pure def new(b,p,r)={
    { id: b, rounds: r, parentId: p }
   }

  action addBlock(b, p, r) = all {
    r > blockbyid.get(p).rounds,
    parent' = parent.put(blockbyid.get(p).id, new(b,p,r)),
    blockbyid' = blockbyid.put(b, new(b,p,r)),
    
  }


}