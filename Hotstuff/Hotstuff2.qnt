module Blockchain {
  type Block = { id: int, parentId: int }

  var blocks: Set[Block]
  var parent: Block -> Block  
  var round: int
  var nextId: int

  val genesisBlock: Block = { id: 0, parentId: 0 }

  action init = all {
    blocks' = Set(genesisBlock),
    parent' = Map(genesisBlock -> genesisBlock),
    round' = 0,
    nextId' = 1
  }
}