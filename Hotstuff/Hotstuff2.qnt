module Blockchain {
  type Block = { id: int, parentId: int }

  var blocks: Set[Block]
  var parent: Block -> Block  
  var round: int
  var nextId: int
  var lastId: int

  val gen: Block = { id: 0, parentId: 0 }

  action init = all {
    blocks' = Set(gen),
    parent' = Map(gen -> gen),
    round' = 0,
    nextId' = 1,
    lastId' = 0
  }

    val newBlock = { id: nextId, parentId: lastId }

   
    var parentBlock : int

action addBlock(new, par, rounds) = all{
    blocks' = blocks,
    parent' = parent,
    round' = round
    
}


    action step = all {
    blocks' = blocks.union(Set(newBlock)),
    
    round' = round,
    nextId' = nextId + 1
    } 

  action propose = all {
   
    blocks' = blocks.union(Set(newBlock)), 
    parent' = parent,
    nextId' = nextId + 1, 
    round' = round + 1, 
    lastId' = newBlock.id 
  }
}