module sets{
    
    type Pair = (str, str)
    pure def fst(pair : Pair): str = pair._1
    pure def snd(pair : Pair): str = pair._2

    

    pure val availablePairs = Set(
    ("BTC", "USDC"), ("BTC", "USDT"), ("ETH", "USDC"), ("ETH", "USDT"),
    ("EVMOS", "USDC"), ("EVMOS", "WETH"), ("ETH", "WETH"), ("ATOM", "EVMOS"),
    ("ATOM", "JUNO"), ("ATOM", "OSMO"), ("EVMOS", "JUNO"), ("ATOM", "JUNO"),
    ("EVMOS", "OSMO")
  )
    
    pure val hasAtomJuno = ("ATOM", "JUNO").in(availablePairs)
    pure val hasEvmosEth = availablePairs.contains(("Evmos", "Eth"))

    pure val hasAtom = availablePairs.exists(p=>p._1 == "ATOM")

    pure val missingScrt = 
        availablePairs.forall(p=>p._1 != "SCRT" and p._2 != "SCRT")

    pure val availableUnorderedPairs = 
        availablePairs.map( p =>Set(p._1,p._2))

    pure val hasAtomRight = availableUnorderedPairs.exists(p => "Atom".in(p))

    pure val atomPairs = 
        availableUnorderedPairs.filter(p => p.contains("ATOM"))

    pure val howMany = availableUnorderedPairs.size()
    pure def coinsInPairs(pairs : Set[Set[str]]): Set[str] = pairs.flatten()

    pure def someCoinsExample = Set("ATOM", "ETH", "USDT")
    pure def buyableVia1Swap(someCoins) = 
    availableUnorderedPairs
    .filter(p => p.intersect(someCoins).size() == 1)
    .flatten()
    .exclude(someCoins)

    pure def buyableVia2Swaps(someCoins) = 
    someCoins
    .buyableVia1Swap()
    .buyableVia1Swap()

    pure def buyableVia1or2Swaps(someCoins) =
    buyableVia1Swap(someCoins)
    .union(buyableVia2Swaps(someCoins))





    }