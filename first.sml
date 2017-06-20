/Applications/Emacs.app/Contents/MacOS/Emacs-x86_64-10_9: /bin/sml: No such file or directory

Process sml exited abnormally with code 127
Standard ML of New Jersey v110.80 [built: Sun Aug 28 21:15:10 2016]
- use first.sml
= ;
stdIn:1.6-1.15 Error: unbound structure: first in path first.sml
- use "first.sml"
= ;
[opening first.sml]
first.sml:1.52-1.57 Error: syntax error: deleting  COMMA AND
first.sml:2.1-2.3 Error: syntax error: deleting  SEMICOLON SEMICOLON
first.sml:2.7 Error: syntax error: inserting  LPAREN
first.sml:2.50-2.54 Error: syntax error: replacing  WITH with  EQUALOP
first.sml:3.1-3.8 Error: syntax error: deleting  SEMICOLON SEMICOLON THEN
first.sml:3.24 Error: syntax error: inserting  LET
first.sml:5.1-5.4 Error: syntax error: replacing  FUN with  EQUALOP
first.sml:6.5 Error: syntax error: inserting  VAL
first.sml:8.1 Error: syntax error found at EOF

uncaught exception Compile [Compile: "syntax error"]
  raised at: ../compiler/Parse/main/smlfile.sml:15.24-15.46
             ../compiler/TopLevel/interact/evalloop.sml:42.54
             ../compiler/TopLevel/interact/evalloop.sml:299.20-299.23
- use "first.sml"
= use "first.sml"
= foo 2;
stdIn:6.1-6.4 Error: unbound variable or constructor: foo
stdIn:4.1-6.6 Error: operator is not a function [tycon mismatch]
  operator: unit
  in expression:
    (use "first.sml") use
- use "first.sml";
[opening first.sml]
val foo = fn : int -> int
val it = () : unit
- foo 2;
val it = 4 : int


Process sml finished
