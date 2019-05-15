# C-Compiler-
C program Compiler written in F#

1. Is the input program a valid simple C program? True or false

2. If the program is not valid, a syntax error message of the form “expecting X, but found Y”.

3. If the program is valid, an equivalent simple ASM program.
Note that “simple C” is really very simple C --- no pointers, no arrays, no types other than integer, no loops, very simple expressions, etc. Here’s an example simple C program 

What A simple C program can look like:

<img width="371" alt="Screen Shot 2019-05-15 at 7 58 21 AM" src="https://user-images.githubusercontent.com/23105576/57777912-96712b00-76e8-11e9-967f-a81ea1de1299.png">

What the output of this F# program will be in case of #3 above:
<img width="951" alt="Screen Shot 2019-05-15 at 7 58 27 AM" src="https://user-images.githubusercontent.com/23105576/57777913-97a25800-76e8-11e9-860d-f55e7acbd97e.png">

Getting this output first we take the Simple C like above, and create the BNF Definition shown below:
<img width="430" alt="Screen Shot 2019-05-15 at 7 58 38 AM" src="https://user-images.githubusercontent.com/23105576/57777918-98d38500-76e8-11e9-94f2-5de44618685b.png">


Then we write a function for each rule or "Recursive descent parsing" which is done in F# in this case. Finally giving us the proper ASM program which was shown above as well. 
