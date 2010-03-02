{ /a /b /c
    b b mulf -4.0 a c mulf mulf addf /d
    d 0.0 lessf
    { %d<0
        []
    }
    {
        b negf /bmin
        a a addf /atwo
        0.0 d lessf    
        { %d>0
            d sqrt /dsq
            bmin dsq negf addf atwo divf /x1
            bmin dsq addf atwo divf /x2            
            [x1 x2]
        }   
        { %d==0
            bmin atwo div /x
            [x]  
        }
        if
    }
    if
} /solveRoot

1.0 -3.0 2.0 solveRoot apply
