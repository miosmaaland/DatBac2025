module Parent{


    var parent: str -> Block
    var Counter: int -> count
    

    type Block = {
        parent: str,
        count: int
    }        

    const gen : Block



    action init = all{
        parent' = Map(gen -> gen),
        Counter' = 0
    }

    val TypeOK = all{
        parent' = Map(parent -> Block)


        
    }

}