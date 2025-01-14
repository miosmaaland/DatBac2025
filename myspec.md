# Counter
 
A counter has a single state variable `n`:
 
```quint myspec.qnt +=
module Counter {
  var n : int
```
 
The initial value of `n` is `0`:
 
```quint myspec.qnt +=
  action init = n' = 0
```
 
At each step, the counter increments the value of `n` by `1`:
 
```quint myspec.qnt +=
  action step = n' = n + 1
}
```
 
If you run a counter for 10 steps, the value of `n` at each step will be equal
to the `i`th step:
 
```quint myspecTests.qnt +=
module TestCounter {
  import Counter.* from "myspec"
 
  run countTest = {
    init.then(10.reps(i => all { step, assert(n == i) }))
  }
}
```