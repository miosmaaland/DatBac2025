module Blockchain {
  type Node = { nodeId: int }
  
  type Block = { id: int, rounds: int, parentId: int }

  var nodes : Set[Node]
  var parent: int -> Block
  var round: int
  var blocks : Block
  var nextId: int
  var roundId: int
  var lastId: int
  var blockers: Set[newBlock]
  var parents: Set[int -> Block]
  var votes: Block -> nodes
  var lock : Node -> Block
  var max : Node -> int
       


  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    blockers' = Set(gen),
    blocks' = gen,
    parent' = Map(0 -> gen),
    parents' = Set(Map(0 -> gen)),
    round' = 0,
    nextId' = 0,
    lastId' = 0,
    roundId' = 0,
    // nodes' = Set()
  }

  val newBlock: Block = { id: nextId, rounds: roundId, parentId: lastId }


  action step = all {
    blocks' = newBlock,
    round' = newBlock.id, 
    nextId' = nextId + 1
  } 

  action proposed(b) = {
    b.get(round != -1)
  }   

  

  action addBlock(b, p, r) = all {
    parent' = Map(lastId -> newBlock),
    blockers' = blockers.union(Set(newBlock)),
    parents' = parents.union(Set(Map(lastId -> newBlock))),
    blocks' = newBlock,
    lastId' = p,
    nextId' = b, 
    roundId' = r, 
    r > newBlock.rounds,
  }




// const first_node : int
// const second_node : int
// const third_node : int
// const faulty_node : int




// const voting_nodes : Set[(first_node,second_node,third_node,faulty_node)]

    
}
