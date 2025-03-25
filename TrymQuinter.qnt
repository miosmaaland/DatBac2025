module Blockchain2 {
  
  
  type Block = { names: str, rounds: int}

  
  

  var parent: Block -> Block
  var blocks : Set[Block]
  var round: int
  var name: str

  val newBlock: Block = {names: name, rounds: round}

  
  val gen: Block = { names: "gen", rounds: 0}


  action init = all {
    blocks' = Set(gen),
    parent' = Map(gen -> gen),
    round' = 0,
    name' = "gen"
  }

def getRounds(b) = {
  b.rounds
}

def updateRounds(b, p) = {
  b.rounds == (p.rounds + 1)
}



action addBlock(new, par) = all{
    blocks' = blocks.union(Set(newBlock)),  
    round' = round + 1,
    name' = new,
    parent' = par.blocks
    }

    // action step = all {
    // blocks' = blocks.union(Set(newBlock)),   
    // } 

}