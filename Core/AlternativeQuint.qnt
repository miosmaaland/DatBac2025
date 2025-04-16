module Burger {
  type IDBLOCK = int 
  type Node = int
  // val first_node : Node = {nodeId: 1}
  // val second_node: Node = {nodeId: 2}
  // val third_node: Node = {nodeId: 3}
  // val faulty_node: Node = {nodeId: 4}
  pure val correct_nodes = Set(1, 2, 3)
  pure val faulty_node = Set(4)
  pure val NODES = Set(1, 2, 3, 4)
  pure val BLOCKID = 0.to(1)
  type Round = int
  type ParentId = int
  
  type Block = { id: int, rounds: Round, parentId: ParentId }

  var parent: int -> Block
  var round: int
  var blocks : Block
  var voteId: int
  var betterBlock: Set[IDBLOCK]
  var blockbyid: int -> Block

  var lock : Node -> Block
  var max : Node -> round
  var nodes: Set[Node]
  // var correct_nodes: Set[Node]
  var votes: int -> Set[Node]
  var votingNodes: Set[Node]

       
  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    
    blocks' = gen,
    parent' = Map(0 -> gen),
    betterBlock' = Set(0),
    round' = 0,
    voteId' = 1,
    blockbyid' = Map(0 -> gen),
    votingNodes' = Set(),
    // correct_nodes' = Set(first_node, second_node, third_node),
    // max' = Map(Set(first_node,second_node,third_node,faulty_node) -> 0),
    votes' = Map(0 -> NODES),
    max' = NODES.mapBy(_ => 0),
    lock' = NODES.mapBy(_ => gen),
  }
  

  
  action Vote(b: int, n:Node):bool = all{
    n.in(correct_nodes),
    max' = max.set(n, blockbyid.get(b).rounds),
    // votes' = betterBlock.mapBy(x => Set()).set(0, NODES),
    vote(b,n)
  }
  
    // action addToVotes(n) = {
    //    votingNodes' = votingNodes.union(n)
    // }

  action vote(b: int, n:Node):bool = all {
    votes' = votes.setBy(blockbyid.get(b).id, vs => vs.union(Set(n))),      
  }


   pure def new(b,p,r)={
    { id: b, rounds: r, parentId: p }
   }

  action addBlock(b, p, r) = all {
    r > blockbyid.get(p).rounds,
    parent' = parent.put(blockbyid.get(p).id, new(b,p,r)),
    blockbyid' = blockbyid.put(b, new(b,p,r)),
    betterBlock' = betterBlock.union(Set(b)),
    votes' = betterBlock.mapBy(x => Set()).set(0, NODES),
  }


}