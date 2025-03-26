module Blockchain {
  type Node = { id: int }
  
  type Block = { id: int, rounds: int, parentId: int }

  var nodes : Set[Node]
  var parent: Block -> Block  
  var round: int
  var blocks : id -> Block
  var nextId: int
  var lastId: int
  var votes: Block -> nodes
  var lock : Node -> Block
  var max : Node -> int
       


  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    blocks' = Map(0 -> gen),
    parent' = Map(gen -> gen),
    round' = 0,
    nextId' = 1,
    lastId' = 0
  }

  val newBlock = { id: nextId, rounds: nextId, parentId: lastId }


  action step = all {
    blocks' = blocks.put(nextId,newBlock),
    round' = newBlock.id, 
    nextId' = nextId + 1
  } 

  action proposed(b) = {
    b.get(round != -1)
  }   

  action addBlock(b, p, r) = all {
    blocks' = blocks.put(nextId,newBlock), 
    parent' = p.id,
    nextId' = nextId + 1, 
    round' = r, 
    r > p.rounds,
    lastId' = newBlock.id 
  }

  def getRounds(b) = {
    
  b.round
}
}
