
module hello {

    // the state variable to keep the output by the computer
    var consoleOutput: str

    // the state variable to keep the out read by the user
    var readByUser: str

    // initialize the state machine that captures the protocol
    action init = all {
      consoleOutput' = "",
      readByUser' = ""
    }

    // write "Hello, world!" in consoleOutput of the state machine,
    // if the console output is clean
    action write = all {
      consoleOutput == "",
      consoleOutput' = "Hello, world!",
      readByUser' = readByUser,
    }

    // read the message from `consoleOutput` into `readByUser`,
    // if the console output is not clean
    action read = all {
      consoleOutput != "",
      readByUser' = consoleOutput,
      consoleOutput' = consoleOutput,
    }

    // execute a single step of the state machine:
    // it may be `read` or `write`, whatever is available
    action step = any {
      write,
      read
    }

    // a simple test that demonstrates an interaction between
    // the computer and the user
    run writeReadTest = init.then(write).then(read)
}
