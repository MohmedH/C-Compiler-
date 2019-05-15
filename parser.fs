//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a tuple containing 
// 3 values:  
//
//   (result, msg, program)
//
// where result is true or false (legal or not legal), 
// msg is a success or syntax error message, and program
// is a list of instructions if parsing was successful.
//
// <<YOUR NAME>>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #05
//

#light

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // These are debug routines that output the tokens, or
  // program, respectively.  They are written so you can
  // inject these into a pipeline to output the current 
  // state of the tokens or program.
  //
  let private __outputTokens (tokens, program) =
    printfn "Tokens: %A" tokens
    (tokens, program)

  let private __outputProgram (tokens, program) =
    printfn "Program: %A" program
    (tokens, program)


  //
  // matchToken
  //
  let private matchToken expected_token (tokens, program) =
    let (token, _) = List.head tokens
    //
    // if the token matches the expected token, keep parsing by
    // returning the rest of the tokens.  Otherwise throw an
    // exception because there's a syntax error, effectively 
    // stopping compilation:
    //
    if expected_token = token then  
      (List.tail tokens, program)
    else
      failwith ("expecting " + (string expected_token) + ", but found " + (string token))

  let rec checkForOp x L = 
    match L with
    | [] -> 0
    | hd::tl -> if hd = x then 1 + checkForOp x tl else 0 + checkForOp x tl
    
  let expr_op (tokens,program) = 
    let(nextToken,p) = List.head tokens
    
    if nextToken = lexer.Tokens.EQ then
      let(T1,P1) = matchToken lexer.Tokens.EQ (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Minus then
      let(T1,P1) = matchToken lexer.Tokens.Minus (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Plus then
      let(T1,P1) = matchToken lexer.Tokens.Plus (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Times then
      let(T1,P1) = matchToken lexer.Tokens.Times (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Divide then
      let(T1,P1) = matchToken lexer.Tokens.Divide (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.LT then
      let(T1,P1) = matchToken lexer.Tokens.LT (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.LTE then
      let(T1,P1) = matchToken lexer.Tokens.LTE (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.GT then
      let(T1,P1) = matchToken lexer.Tokens.GT (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.GTE then
      let(T1,P1) = matchToken lexer.Tokens.GTE (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.NE then
      let(T1,P1) = matchToken lexer.Tokens.NE (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Power then
      let(T1,P1) = matchToken lexer.Tokens.Power (tokens,program)
      (T1,P1)
    else
      failwith ("Invalid Token")
      
      
  let expr_value (tokens,program) =
    let(nextToken,_) = List.head tokens
    
    if nextToken = lexer.Tokens.ID then
      let (T1,P1) = matchToken lexer.Tokens.ID (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Int_Literal then 
      let (T1,P1) = matchToken lexer.Tokens.Int_Literal (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Str_Literal then
      let (T1,P1) = matchToken lexer.Tokens.Str_Literal (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Bool_Literal then
      let (T1,P1) = matchToken lexer.Tokens.Bool_Literal (tokens,program)
      (T1,P1)
    else 
      failwith ("expecting identifier or literal but found " + (string nextToken))
      
  let expr (tokens,program) =
    let(T1,P1) = expr_value(tokens,program)
    
    let(nextToken,_) = List.head (List.tail tokens)
    
    let a = int nextToken
    let n = checkForOp a ([7..17]@[25])
    
    if n = 1 then
      let(T2,P2) = expr_op (T1,P1)
      let(T3,P3) = expr_value (T2,P2)
      (T3,P3)
    else
      (T1,P1)
  
  
  let vardecl (tokens,program) = 
    let(nextToken,nextProgram) = List.head tokens
    
    if nextToken = lexer.Tokens.ID then
      let(T1,P1) = matchToken lexer.Tokens.ID (tokens,["$DECL"; nextProgram;]::program)
      let(T2,P2) = matchToken lexer.Tokens.Semicolon (T1,P1)
      (T2,P2)
    else 
      failwith ("Invalid vardecl")
 
  let input (tokens,program) = 
    let(nextToken,nextProgram) = List.head tokens
    
    let(pp,p) = List.head (List.tail tokens)
    if nextToken = lexer.Tokens.Input then
      let(T1,P1) = matchToken lexer.Tokens.Input (tokens,program)
      let(T2,P2) = matchToken lexer.Tokens.ID (T1,["$INPUT"; p]::P1)
      let(T3,P3) = matchToken lexer.Tokens.Semicolon (T2,P2)
      (T3,P3)
    else
      failwith ("Invalid Input")
  
  let output_value (tokens,program) = 
    let(nextToken,_) = List.head tokens
    
    if nextToken = lexer.Tokens.Endl then
      let(T1,P1) = matchToken lexer.Tokens.Endl (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.ID then
      let(T1,P1) = expr_value (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Str_Literal then
      let(T1,P1) = expr_value (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Bool_Literal then
      let(T1,P1) = expr_value (tokens,program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Int_Literal then
      let(T1,P1) = expr_value (tokens,program)
      (T1,P1)
    else
      failwith ("expecting identifier or literal or endl but found " + (string nextToken))
  
  let output (tokens,program) = 
    let(nextToken,_) = List.head tokens
    let (t,t1) = List.head(List.tail tokens) 
    
    if nextToken = lexer.Tokens.Output then
      let(T1,P1) = matchToken lexer.Tokens.Output (tokens,program)
      let(T2,P2) = output_value (T1,["$OUTPUT";(string t) ;t1]::P1)
      let(T3,P3) = matchToken lexer.Tokens.Semicolon (T2,P2)
      (T3,P3)
    else
      failwith ("Invalid Output")
      
  let assignment b (tokens,program) = 
    let(nextToken,_) = List.head tokens
    
    let (t,t1) = List.head(List.tail tokens) 
    let (t2,t3) = List.head(List.tail (List.tail tokens) )
    
    let a = int t2
    let n = checkForOp a ([7..17]@[25])
    
    if n = 0 then 
      let l = ["$ASSIGN";b;(string t);t1;]
      if nextToken = lexer.Tokens.Assign then
        let(T1,P1) = matchToken lexer.Tokens.Assign (tokens,program)
        let(T2,P2) = expr (T1,P1)
        let(T3,P3) = matchToken lexer.Tokens.Semicolon (T2,l::P2)
        (T3,P3)
      else
        failwith ("Invalid assignment")
      
    else
      let (t4,t5) = List.head(List.tail (List.tail (List.tail tokens) ) )
      let l = ["$ASSIGN"; b;(string t);t1;t3;(string t4);t5]
      if nextToken = lexer.Tokens.Assign then
        let(T1,P1) = matchToken lexer.Tokens.Assign (tokens,program)
        let(T2,P2) = expr (T1,P1)
        let(T3,P3) = matchToken lexer.Tokens.Semicolon (T2,l::P2)
        (T3,P3)
      else
        failwith ("Invalid assignment")
      
    
      
 
 //START Of the Statemnts 
  let rec stmt (tokens, program) = 
    let(nextToken,test) = List.head tokens
    
    if nextToken = lexer.Tokens.If then
      let (T1,P1) = matchToken lexer.Tokens.If (tokens,program)
      let (T2,P2) = ifstmt (T1,P1)
      (T2,P2) 
    else if nextToken = lexer.Tokens.Cout then
      let (T1,P1) = matchToken lexer.Tokens.Cout (tokens,program)
      let (T2,P2) = output (T1,P1)
      (T2,P2)
    else if nextToken = lexer.Tokens.Semicolon then
      let (T1,P1) = matchToken lexer.Tokens.Semicolon (tokens,["$EMPTY"]::program)
      (T1,P1)
    else if nextToken = lexer.Tokens.Int then
      let (T1,P1) = matchToken lexer.Tokens.Int (tokens,program)
      let (T2,P2) = vardecl (T1,P1)
      (T2,P2)
    else if nextToken = lexer.Tokens.ID then
      let (T1,P1) = matchToken lexer.Tokens.ID (tokens,program)
      let (T2,P2) = assignment test (T1,P1)
      (T2,P2)
    else if nextToken = lexer.Tokens.Cin then
      let (T1,P1) = matchToken lexer.Tokens.Cin (tokens,program)
      let (T2,P2) = input (T1,P1)
      (T2,P2)
    else
      failwith("expected statement, but found " + (string tokens))
  
  and private condition (tokens,program) =
    let(T1,P1) = expr(tokens,program)
    (T1,P1)
    
  and private then_part (tokens,program) =
    let(T1,P1) = stmt (tokens,program)
    (T1,P1)
    
  and private else_part (tokens,program) = 
    let(nextToken,_) = List.head tokens

    if nextToken = lexer.Tokens.Else then
      let (T1,P1) = matchToken lexer.Tokens.Else (tokens,program)
      let (T2,P2) = stmt (T1,P1)
      (T2,P2)
    else
      (tokens, ["$EMPTY"]::program)
      
  and private ifstmt (tokens,program) =
    let(nextToken,_) = List.head tokens
    
    let (t,t1) = List.head(List.tail tokens) 
    let (t2,t3) = List.head(List.tail (List.tail tokens) )
    let (t4,t5) = List.head(List.tail (List.tail (List.tail tokens) ) )


    
    //Match Open Paren
    let (T1,P1) = matchToken lexer.Tokens.OpenParen (tokens,program)
    //Match Condition
    let (T2,P2) = condition (T1,P1)
    if(t3 = ")") then
      let (T3,P3) = matchToken lexer.Tokens.CloseParen (T2,["$IF";(string t); t1;]::P2)
      let (T4,P4) = then_part (T3,P3)
      let (T5,P5) = else_part (T4,P4)
      (T5,P5)
    else
      //Match Close Paren
      let (T3,P3) = matchToken lexer.Tokens.CloseParen (T2,["$IF";(string t); t1; t3;(string t4); t5 ]::P2)
    //Match Then Part
      let (T4,P4) = then_part (T3,P3)
    //Match Else Part
      let (T5,P5) = else_part (T4,P4)
      (T5,P5)
      
  let rec morestmts (tokens,program) =
    let(nextToken,_) = List.head tokens
        
    if nextToken = lexer.Tokens.If then
      morestmts(stmt (tokens,program))
    else if nextToken = lexer.Tokens.Cout then
      morestmts(stmt (tokens,program))
    else if nextToken = lexer.Tokens.Int then
      morestmts(stmt (tokens,program))
    else if nextToken = lexer.Tokens.Cin then 
      morestmts(stmt (tokens,program))
    else if nextToken = lexer.Tokens.ID then
      morestmts(stmt (tokens,program))
    else if nextToken = lexer.Tokens.Semicolon then
      morestmts(stmt (tokens,program))
    else
      (tokens,program)

  let rec stmts (tokens,program) = 
    let (T1,P1) = stmt (tokens, program)
    let (T2,P2) = morestmts(T1,P1)
    (T2,P2)
   
    
  //
  // simpleC
  // 
  let private simpleC (tokens, program) = 
    let (T1, P1) = matchToken lexer.Tokens.Void (tokens, program)
    let (T2, P2) = matchToken lexer.Tokens.Main (T1, P1)
    let (T3, P3) = matchToken lexer.Tokens.OpenParen (T2, P2)
    let (T4, P4) = matchToken lexer.Tokens.CloseParen (T3, P3)
    let (T5, P5) = matchToken lexer.Tokens.OpenBrace (T4, P4)
    let (T6, P6) = stmts (T5,P5)
    let (T7, P7) = matchToken lexer.Tokens.CloseBrace (T6, P6)
    let (T8, P8) = matchToken lexer.Tokens.EOF (T7, P7)
    (T8, P8)

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // a tuple containing 3 values:  
  //
  //   (result, msg, program)
  //
  // where result is true or false (legal or not legal), 
  // msg is a success or syntax error message, and program
  // is a list of instructions if parsing was successful.
  //
  let parse tokens = 
    try
      let (_, program) = simpleC (tokens, [])
      (true, "success", List.rev program)
    with 
      | ex -> (false, ex.Message, [])

    
