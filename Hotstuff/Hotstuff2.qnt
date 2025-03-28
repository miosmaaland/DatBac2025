module Blockchain {
  
  
  
  type Node = { id: int }
  
  type Block = { id: int, rounds: int, parentId: int }

  var nodes : Set[Node]
  var parent: Set[lastId -> Block]  
  var round: int
  var blocks : Set[id -> Block]
  var nextId: int
  var roundId: int
  var lastId: int
  var votes: str -> Set[(int, int)]
  var lock : Node -> Block
  var max : Node -> int
       


  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    blocks' = Set(Map(0 -> gen)),
    parent' = Set(Map(gen -> gen)),
    round' = 0,
    nextId' = 0,
    lastId' = 0,
    roundId' = 0,
    nodes' = Set()
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


  

  action addBlock(b:int, p : int,r : int) : bool = all {
    blocks' = blocks.union(Set(Map(nextId -> newBlock))),
    parent' = parent.union(Set(Map(lastId -> newBlock))),
    nextId' = b+0, 
    lastId' = p+0,
    roundId' = r+0,  
  }




const first_node : int
const second_node : int
const third_node : int
const faulty_node : int




const voting_nodes : Set[(first_node,second_node,third_node,faulty_node)]

    
}
