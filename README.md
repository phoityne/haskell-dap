
# haskell-dap

The goal is "let GHCi speak [DAP](https://code.visualstudio.com/docs/extensions/example-debuggers)".

![goal.png](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/goal.png)  

## Information

* [2018/12/15] Release haskel-dap-0.0.10.0.  
  * [MODIFY] supported ghc-8.6


## Limitation
Currently this project is an experimental design and implementation.

* supporting ghc-8.0, ghc-8.2, ghc-8.4, ghc-8.6
* need stack project.
* checked on windows7, centos7

## Features

### inspect watch

![01_inspect_variables.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/01_inspect_variables.gif)  

### inspect hoover

![02_inspect_hoover.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/02_inspect_hoover.gif)  

### inspect repl

![03_inspect_repl.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/03_inspect_repl.gif)  

### logpoint

![04_log_point.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/04_log_point.gif)  

### break condition

![05_break_exp.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/05_break_exp.gif)  

### break hit count

![06_break_hitcount.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/06_break_hitcount.gif)  

## Install

install vscode and [phoityne](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode) extension.

### Run stack install

    % stack install haskell-dap
      . . . . .
    Copied executables to C:\Users\[USERNAME]\AppData\Roaming\local\bin:
    - haskell-dap.exe

    %

## Configuration

Add `--with-ghc` option to `ghciCmd` variable in the vscode launch.json.  


    "ghciCmd": "stack ghci --with-ghc=haskell-dap --test --no-load --no-build --main-is TARGET --ghci-options -fghci-hist-size=5 --ghci-options -fprint-evld-with-show ",



## IFData Design

![if_data.png](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/if_data.png)  


## Sequences

### Overview.
![seq1.png](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/seq1.png) 

### Debugging details.
![seq2.png](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/seq2.png) 
