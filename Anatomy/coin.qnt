module coin {
    
    type Addr = str
    type UInt = int


    pure val Max_UInt = 2^256-1
    pure def isUInt(i : int): bool = (0 <= i and i <= Max_UInt)


    pure val ADDR = Set("null", "alice", "bob", "charlie", "eve" )

    var minter : Addr
    var balances : Addr -> UInt

    val state = {minter : minter, balances : balances}

    def require(cond : bool): bool = cond

    val totalSupply = ADDR.fold(0, (sum, a) => sum + balances.get(a))


    action init : bool = {
        nondet sender = oneOf(ADDR)

        all{
            minter' = sender,
            balances' = ADDR.mapBy(a => 0)
        }
    }

    action mint(sender: Addr, receiver : Addr, amount : UInt): bool = all{
        require(sender == minter),
        val newBal = balances.get(receiver) + amount
        all {
            require(isUInt(newBal)),
            balances' = balances.set(receiver, newBal),
            minter' = minter,
        }
    }

    action send(sender : Addr, receiver : Addr, amount : UInt): bool = all{
        require(not(amount > balances.get(sender))),
        if (sender == receiver) {
            balances' = balances
        } 
        else{
            val newSenderBal = balances.get(sender) - amount
            val newReceiverBal = balances.get(receiver) + amount
            all {
                require(isUInt(newSenderBal)),
                require(isUInt(newReceiverBal)),
                balances' = 
                balances
                .set(sender, newSenderBal)
                .set(receiver, newReceiverBal)
            }
        },
        minter' = minter
    }

    action step : bool = {
        nondet sender = oneOf(ADDR)
        nondet receiver = oneOf(ADDR)
        nondet amount = 0.to(Max_UInt).oneOf()

        any {
            mint(sender, receiver, amount),
            send(sender, receiver, amount),
        }

    }

    val balancesRangeInv : bool =
        ADDR.forall(a => isUInt(balances.get(a)))

    val totalSupplyDoesNotOverflowInv : bool = {
        isUInt(totalSupply)
    }

    temporal noSupplyOverflow : bool = always(totalSupplyDoesNotOverflowInv)

    run sendWithoutMintTest = {
        init.then(send(minter, "bob", 5))
        .fail()
    }

    run mintSendTest = {
        init.then(mint(minter, "bob", 10))
        .then(send("bob", "eve", 4))
        .then(all{
            assert(balances.get("bob") == 6),
            assert(balances.get("eve") == 4),
            minter' = minter,
            balances' = balances,

        })
    }

    run mintTwiceSendError = {

        nondet mintEve = 0.to(Max_UInt).oneOf()
        nondet mintBob = 0.to(Max_UInt).oneOf()
        nondet eveToBob = 0.to(Max_UInt).oneOf()

        init.then(mint(minter, "eve", mintEve))
        .then(mint(minter, "bob", mintBob))
        .then(
            if (eveToBob <= balances.get("eve")) {
                send("eve", "bob", eveToBob)
            }
            else {
                all {minter' = minter, balances' = balances}
            }
        )
    }
    }