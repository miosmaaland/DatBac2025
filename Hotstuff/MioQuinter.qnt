module Blockchain {
  const first_node: int
  const second_node: int
  const third_node: int
  const faulty_node: int
  
  type Block = { id: int, rounds: int, parentId: int }

  var nodes: Set[(first_node,second_node,third_node,faulty_node)]
  var correct_nodes: Set[(first_node, second_node, third_node)]
  var parent: Set[(int, Block)]  
  var round: int
  var blocks: Set[Block]
  var nextId: int
  var roundId: int
  var lastId: int
  var votes: (int, int) -> Set[int]  
  var lock: int -> Block             
  var max: int -> int                 

  val gen: Block = { id: 0, rounds: 0, parentId: 0 }

  action init = all {
    blocks' = Set({gen}),
    parent' = Set({(0, gen)}),
    round' = 0,
    nextId' = 1,
    lastId' = 0,
    roundId' = 0,
    votes' = Map(),
    lock' = Map(),
    max' = Map()
  }

  action voteForBlock(b: Block) = any {
    val voter = oneOf(correct_nodes)
    val currentVotes = votes.getOrDefault((b.id, b.rounds), Set())
    votes' = votes.put((b.id, b.rounds), currentVotes.add(voter))
    
    // If majority of correct nodes voted for this block, add it to blocks
    if (currentVotes.add(voter).size() >= 2) {
      blocks' = blocks.add(b),
      lastId' = b.id,
      roundId' = b.rounds
    } else {
      blocks' = blocks
    },
    
    // Other variables remain unchanged unless specified
    parent' = parent,
    round' = round,
    nextId' = nextId,
    lock' = lock,
    max' = max
  }

  action proposeBlock = {
    val newBlock: Block = { id: nextId, rounds: roundId + 1, parentId: lastId }
    
    // First add the block to consideration
    blocks' = blocks.add(newBlock),
    nextId' = nextId + 1,
    roundId' = roundId + 1,
    
    // Then nodes can vote on it
    voteForBlock(newBlock)
  }

  action step = all {
    val newBlock = { id: nextId, rounds: roundId, parentId: lastId }
    blocks' = blocks.add(newBlock),
    round' = newBlock.id, 
    nextId' = nextId + 1
  } 

  action addBlock(b: int, p: int, r: int) = all {
    val newBlock = { id: b, rounds: r, parentId: p }
    blocks' = blocks.add(newBlock),
    parent' = parent.add((p, newBlock)),
    nextId' = b + 1, 
    lastId' = p + 0,
    roundId' = r + 0 
  }
}