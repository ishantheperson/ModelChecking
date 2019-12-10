# Synchronous Model Checking


## Dependencies

 - Parsec (should be included with Haskell)
 - GHC version > 8 is preferred. 
 - Also needs collections > 0.5.11 (included with GHC version > 8)

Otherwise should be none. 

## Usage

The structure included is presburger arithmetic. To add more you would need to 
create some new DFAs (as seen in `src/SampleStructure.hs`) and then package it up
into a structure (as seen at the end of `src/ModelChecker/Structure.hs`) and then 
switch `defaultStructure` in `src/Main.hs` to the new one. 

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
a + b != c
```

You can run `help` in the interactive prompt for some more examples
