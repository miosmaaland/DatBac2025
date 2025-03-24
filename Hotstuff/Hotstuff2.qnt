module Blockchain {
  type Node = { id: int }
  
  type Block = { id: int, rounds: int, parentId: int }

  var nodes : Set[Node]
  var parent: Block -> Block  
  var round: int
  var blocks : Set[Block]
  var nextId: int
  var lastId: int
  var votes: Block -> nodes
  var lock : Node -> Block
  var max : Node -> int
       


  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    blocks' = Set(gen),
    parent' = Map(gen -> gen),
    round' = 0,
    nextId' = 1,
    lastId' = 0
  }

  val newBlock = { id: nextId, rounds: nextId, parentId: lastId }


  action step = all {
    blocks' = blocks.union(Set(newBlock)),
    round' = newBlock.id, 
    nextId' = nextId + 1
  } 

  action proposed(b) = {
    b.get(round != -1)
  }   

  action addBlock(b, p, r) = all {
    blocks' = blocks.union(Set(newBlock)), 
    parent' = parent.put(newBlock, p),
    nextId' = nextId + 1, 
    round' = newBlock.id, 
    lastId' = newBlock.id 
  }

  def getRounds(b) = {
    
  b.round
}
}
