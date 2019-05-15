# C-Compiler-
C program Compiler written in F#

1. Is the input program a valid simple C program? True or false
2. If the program is not valid, a syntax error message of the form “expecting X, but found Y”.
3. If the program is valid, an equivalent simple ASM program.Note that “simple C” is really very simple C --- no pointers, no arrays, no types other than integer, no loops, very simple expressions, etc. Here’s an example simple C program 

What A simple C program can look like:


What the output of this F# program will be in case of #3 above: 


Getting this output first we take the Simple C like above, and create the BNF Definition shown below:


Then we write a function for each rule or "Recursive descent parsing" which is done in F# in this case. Finally giving us the proper ASM program which was shown above as well. 