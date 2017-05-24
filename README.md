# Html Browser

## Building
```
git clone git@gitlab.com:pedrocb/Haskell_Browser.git
cd Haskell_Browser
stack install
haskell-browser-exe
```

## How to use
Without arguments it will ask for a url and then show the html processed.
```
[haskell-browser] haskell-browser-exe                                                                                                                                  21:39:34  ☁  master ☂ ⚡
Enter website (or link): 
http://www.alunos.dcc.fc.up.pt/~up201602876/
#######################################################

  -#Text  [[0 www.alunos.dcc.fc.up.pt/~up201602876/text.html]] 

  -#Styles  [[1 www.alunos.dcc.fc.up.pt/~up201602876/styles.html]] 

  -#Lists  [[2 www.alunos.dcc.fc.up.pt/~up201602876/lists.html]] 


#######################################################
```

It is also possible to go through hyperlinks (represented by [[id url]]) using the id

```
[haskell-browser] haskell-browser-exe                                                                                                                                  21:41:56  ☁  master ☂ ⚡
Enter website (or link): 
http://www.alunos.dcc.fc.up.pt/~up201602876/ 
#######################################################

  -#Text  [[0 www.alunos.dcc.fc.up.pt/~up201602876/text.html]] 

  -#Styles  [[1 www.alunos.dcc.fc.up.pt/~up201602876/styles.html]] 

  -#Lists  [[2 www.alunos.dcc.fc.up.pt/~up201602876/lists.html]] 


#######################################################
Enter website (or link): 
http://2
#######################################################

**Ordered List **
  1. 
_Nested ordered list_
    1. 

    2. 
_**Nested^2 Ordered list**_
      1. 1

      2. 2
 [[0 www.alunos.dcc.fc.up.pt/~up201602876/]] Unordered Bold List   
  -**1st Item**

  -**2nd Item**


#######################################################
```

## Flags
### File
Flags -f or --file reads html from a file
Example: 
```
[haskell-browser] haskell-browser-exe -f html_files/index.html                                                                                                         21:32:47  ☁  master ☂ ⚡

  -#Text  [[0 www.alunos.dcc.up.pt/~up201602876/text]] 

  -#Styles  [[1 www.alunos.dcc.up.pt/~up201602876/styles]] 

  -#Lists  [[2 www.alunos.dcc.up.pt/~up201602876/lists]] 


```

### String
Flags -s or --string reads html from argument string
```
[haskell-browser] haskell-browser-exe -s "<html><ol><li></li><ul><li></li></ul></ol></html>"                                                                           21:32:49  ☁  master ☂ ⚡

  1. 

    -

```

### Tree
Flags -t or --tree shows tree instead of processed html
```
[haskell-browser] haskell-browser-exe -s "<html><ol><li></li><ul><li></li></ul></ol></html>" -t                                                                        21:37:23  ☁  master ☂ ⚡
Tag: html
..Tag: ol
....Tag: li
....Tag: ul
......Tag: li
```

# External packages used
- http://hackage.haskell.org/package/cli
- http://hackage.haskell.org/package/parsec
- http://hackage.haskell.org/package/HTTP

# Supported tags
```
- <p></p>
- <ol></ol>
- <ul></ul>
- <li></li>
- <h1-6></h1-6>
- <b></b>
- <html>
- <a>hyperlink</a>
```