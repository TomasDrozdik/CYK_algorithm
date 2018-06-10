# CYK_algorithm in haskell

A ghc haskell implementation of CYK parser on given context free grammar in 
chomsky normal form.

Compilation:
    either use ghc on CYK.hs and follow the instructions.
    or use ghci

API:
    From CFG.hs: 
        - function grammarFromFile takes file with CFG in CHNF
          (see example file grammar.txt) as an input with S as
          a start non terminal
        - also provides data type for representation of CFG

    From CYK.hs
        - function cyk' produces CYK matrix for given string
        - function cyk checks whether non terminal S (as a start NT)
          can produce given word


This is a school project for course credit.
