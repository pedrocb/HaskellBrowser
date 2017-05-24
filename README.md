# Html Browser

## Building
```
git clone git@gitlab.com:pedrocb/Haskell_Browser.git
cd Haskell_Browser
stack install
haskell-browser-exe
```

## Flags
### File
Flags -f or --file read html from a file
Example: 
```
haskell-browser-exe -f html_files/index.html                                                                                                         21:32:47  ☁  master ☂ ⚡

#  -#Text  [[0 www.alunos.dcc.up.pt/~up201602876/text]] 
#
#  -#Styles  [[1 www.alunos.dcc.up.pt/~up201602876/styles]] 
#
#  -#Lists  [[2 www.alunos.dcc.up.pt/~up201602876/lists]] 


```

### String
Flags -s or --string read html from argument string
```
# haskell-browser-exe -s "<html><ol><li></li><ul><li></li></ul></ol></html>"                                                                           21:32:49  ☁  master ☂ ⚡
#
#  1. 
#
#    -
#
```