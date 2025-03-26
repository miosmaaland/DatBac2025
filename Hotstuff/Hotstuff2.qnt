module Blockchain {
  type Node = { id: int }
  
  type Block = { id: int, rounds: int, parentId: int }

  var nodes : Set[Node]
  var parent: Set[lastId -> Block]  
  var round: int
  var blocks : Set[int -> Block]
  var nextId: int
  var roundId: int
  var lastId: int
  var votes: Block -> nodes
  var lock : Node -> Block
  var max : Node -> int
       


  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    blocks' = Set(Map(0 -> gen)),
    parent' = Set(Map(gen -> gen)),
    round' = 0,
    nextId' = 0,
    lastId' = 0,
    roundId' = 0
  }

  val newBlock: Block = { id: nextId, rounds: roundId, parentId: lastId }


  action step = all {
    blocks' = blocks.union(Set(Map(nextId -> newBlock))),
    round' = newBlock.id, 
    nextId' = nextId + 1
  } 

  action proposed(b) = {
    b.get(round != -1)
  }   

  action addBlock(b:int, p:int, r:int): bool = all {
    blocks' = blocks.union(Set(Map(nextId -> newBlock))),
    parent' = parent.union(Set(Map(lastId -> newBlock))),
    lastId' = p,
    nextId' = b, 
    roundId' = r,  
  }

}
