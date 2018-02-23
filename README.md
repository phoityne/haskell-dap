
# haskell-dap

The goal is "let GHCi speak [DAP](https://code.visualstudio.com/docs/extensions/example-debuggers)".

![goal.png](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/goal.png)  

## Information

* [2018/02/25] Release haskel-dap-0.0.3.0.  
  * [ADD] setBreakpoint, continue, scopes, stackTrace, evaluate commands.


## Limitation
Currently this project is an experimental design and implementation.

* supporting ghc-8.0.2, ghc-8.2.2
* need stack project.
* checked on windows7, centos7

## Features

### inspect variables

![01_inspect_variables.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/01_inspect_variables.gif)  

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


    "ghciCmd": "stack ghci --with-ghc=haskell-dap --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show ",



## IFData Design

![if_data.png](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/if_data.png)  



