{ /x /y x} /const

{ /x x x } /dup

{ dup apply mulf } /square

{ dup apply 
    0.0 lessf
    {}
    {negf}
    if
} /abs
    
%4.0 square apply
%-4.0 abs apply
2.0 /x
[x]
