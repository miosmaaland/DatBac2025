module Blockchain2 {
  
  
  type Block = { round: int}

  val natNumbers = Nat.union(Set(-1))
  var rounds : Block -> round

  var parent: Block -> Block  
  var blocks : Set[Block]
  var round: natNumbers

  val newBlock = { round}

  
  val gen: Block = { round: 0}


  action init = all {
    blocks' = Set(gen),
    parent' = Map(gen -> gen),
    round' = 0,
  }

def getRounds(b) = {
  b.round
}



action addBlock(new, par) = all{
    blocks' = blocks,
    parent' = parent,   
    }

    action step = all {
    blocks' = blocks.union(Set(newBlock)),   
    round' = round.union(Set(getRounds(newBlock.parent) + 1)),
    } 

}