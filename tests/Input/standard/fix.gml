
{ /f 
    { /x {x x apply} f apply }
    { /x {x x apply} f apply }
    apply
} /fix

{ /frec /n 
    0 n lessi
    { n -1 addi frec apply
      n muli }
    { 1 }
    if
} /facf

{ /n 
    facf fix apply
    n apply
} /fac

5.0 fac apply
