module sets{
    type Pair = (str, str)
    
    pure def fst(pair: Pair): str = pair._1
    
    pure def snd(pair: Pair): str = pair._2

    pure val availablePairs = Set(
    ("BTC", "USDC"), ("BTC", "USDT"), ("ETH", "USDC"), ("ETH", "USDT"),
    ("EVMOS", "USDC"), ("EVMOS", "WETH"), ("ETH", "WETH"), ("ATOM", "EVMOS"),
    ("ATOM", "JUNO"), ("ATOM", "OSMO"), ("EVMOS", "JUNO"), ("ATOM", "JUNO"),
    ("EVMOS", "OSMO"))

    pure val hasAtomJuno = ("ATOM", "JUNO").in(availablePairs)
 
    pure val hasEvmosEth = availablePairs.contains(("EVMOS", "ETH"))

    pure val hasAtom = availablePairs.exists(p => p._1 == "ATOM")
 
    pure val missingScrt =
        availablePairs.forall(p => p._1 != "SCRT" and p._2 != "SCRT")

    pure val availableUnorderedPairs =
    availablePairs.map(p => Set(p._1, p._2))
 
    pure val hasAtomRight = availableUnorderedPairs.exists(p => "ATOM".in(p))

    pure val atomPairs = availableUnorderedPairs.filter(p => p.contains("ATOM"))

    pure val howMany = availableUnorderedPairs.size()

    pure def coinsInPairs(pairs: Set[Set[str]]): Set[str] = pairs.flatten()
 
    pure val coins = coinsInPairs(availableUnorderedPairs)

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

    pure def buyableNSwaps(someCoins, n) =
        1.to(n).fold(someCoins,
                 (prevCoins, i) => buyableVia1Swap(prevCoins))

    pure def myExists(inSet: Set[a], pred: a => bool): bool =
    inSet.fold(false, (result, elem) => result or pred(elem))
 
    pure def myForall(inSet: Set[a], pred: a => bool): bool =
        inSet.fold(true, (result, elem) => result and pred(elem))
    
    pure def myFilter(inSet: Set[a], pred: a => bool): Set[a] =
        inSet.fold(Set(),
        (result, elem) =>
            if (pred(elem)) result.union(Set(elem)) else result)
    
    pure def myMap(inSet: Set[a], mapper: a => b): Set[b] =
        inSet.fold(Set(),
        (result, elem) => result.union(Set(mapper(elem))))
    
    pure def myFlatten(inSet: Set[Set[a]]): Set[a] =
        inSet.fold(Set(), (result, elem) => result.union(elem))

    pure val quads =
    availableUnorderedPairs
      .powerset()
      .filter(pairs => pairs.size() == 4)
 
    pure def isCycle(pairs: Set[Set[str]]): bool = {
        pure def appearsTwice(coin) = {
        pairs.filter(ps => coin.in(ps)).size() == 2
        }
        coinsInPairs(pairs).forall(appearsTwice)
    }
    
    pure val cycles4 = quads.filter(isCycle)
}