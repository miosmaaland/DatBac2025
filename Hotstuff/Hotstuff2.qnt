module Blockchain {
  type Node = {id : int}
  
  
  type Block = { rounds: int, parentId: int }

  var rounds : Block -> round

  var nodes : Set[Node]
  var parent: Block -> Block  
  var round: int
  var blocks : Set[Block]
  var nextId: int
  var lastId: int
  

  

  val gen: Block = { rounds: 0, parentId: 0 }


  action init = all {
    blocks' = Set(gen),
    parent' = Map(gen -> gen),
    round' = 0,
    nextId' = 1,
    lastId' = 0
  }

    val newBlock = { rounds: nextId, parentId: lastId }

   
    var parentBlock : int





action addBlock(new, par) = all{
    blocks' = blocks,
    parent' = parent
    }


    action step = all {
    blocks' = blocks.union(Set(newBlock)),
    
    round' = round,
    nextId' = nextId + 1
    } 

  action proposed(b) = {
    b.in(blocks.contains(rounds != -1))
  }   

  action propose(b,p,r) = all {
   
    blocks' = blocks.union(Set(newBlock)), 
    parent' = parent,
    nextId' = nextId + 1, 
    round' = round + 1, 
    lastId' = newBlock.rounds 
  }
}