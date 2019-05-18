//
// F# program to parse simple C programs.
//


#light


//##################################################################################
//
// main
//
// Compiles the simple C program given as command-line argument, e.g.
//
//    dotnet run main.c
//
[<EntryPoint>]
let main argv =
  if argv.Length = 0 then
    printfn "Usage: dotnet run file.c"
    0
  else
    let filename = argv.[0]
    printfn "compiling %s..." filename
    //
    // Run the lexer to get the tokens, and then
    // pass these tokens to the parser to see if
    // the input program is legal:
    //
    let tokens = compiler.lexer.analyze filename
    let (valid, msg, program) = compiler.parser.parse tokens
    //
    if valid then
      printfn "%s" msg
    else
      printfn "syntax error: %s" msg
    //
    printfn ""
    printfn "%A" tokens
    printfn ""
    printfn "%A" program
    printfn ""
    //
    0