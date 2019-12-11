# Synchronous Model Checking


## Dependencies

 - Parsec (should be included with Haskell)
 - GHC version > 8 is preferred. 
 - Also needs collections > 0.5.11 (included with GHC version > 8)

Otherwise should be none. 

## Usage

The structure included is presburger arithmetic. To add more you would need to 
create some new DFAs (as seen in `src/SampleStructure.hs`) and then package it up
into a structure (defined in `src/ModelChecker/Structure.hs`, example in `SampleStructure`
and then switch `defaultStructure` in `src/Main.hs` to the new one. 

```
% make 
% ./mcheck # also bin/mcheck
```

You can enter a quantified sentence
```
forall a b c. a + b = c
```
or just the matrix portion to see if the DFA corresponding to that
accepts anything or not
```
a -> b && b -> c && c -> d && d -> e 
```

You can run `help` in the interactive prompt for some more examples


Variables can be any alphanumeric string + single quotes,
as long as they are not the reserved words "forall, exists, and, or" 

The operators are 
    -> (binary operator, default is a -> b <=> a + 1 = b)
    _ + _ = _ (ternary operator, default is just addition a + b = c)
    = (equality) 
    
    &&, and (can also use the word and instead)
    ||, or (can also use the word or instead)
    => (implication)
    ! (negation. Can also be used in front of = in ternary/equality) 

Quantifiers are forall and exists. They must appear before the matrix
