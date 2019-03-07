## SP

    $ stack build
    $ stack exec sp
    Enter some lambda calculus:
    λ>(((pair a) b) true)
    "(((pair a) b) true)" >(((λx.(λy.x)) a) b)
    >a
    end.
    λ>(((pair a) b) false)
    "(((pair a) b) false)" >(((λx.(λy.y)) a) b)
    >b
    end.