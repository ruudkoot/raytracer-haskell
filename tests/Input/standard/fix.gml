{ /frec /n 
    0 n lessi
    { n -1 addi frec frec apply
      n muli }
    { 1 }
    if
} /facf

5 facf facf  apply
