# swindle
**swindle** is a R5RS scheme interpreter written in Haskell.

### Installation
```
$ cabal install
$ cabal run
```

### Usage

``` scheme
(define numlist '(23 54 67 48))
(define (second x) (first (rest x)))
(second numlist)

```


