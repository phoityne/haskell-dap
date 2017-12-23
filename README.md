
# haskell-dap

The goal is "let GHCi speak [DAP](https://code.visualstudio.com/docs/extensions/example-debuggers)".

## Limitation
currently this project is experimental.

* supporting ghc-8.0.2, ghc-8.2.2
* need stack project.
* checked on windows7, centos7

## Features

### inspect variables

![01_inspect_variables.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/01_inspect_variables.gif)  

## Install

install vscode and [phoityne](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode) extension.

### Run stack install

    % git clone https://github.com/phoityne/haskell-dap
    %
    % cd haskell-dap
    %
    % stack install
      . . . . .
    Copied executables to C:\Users\[username]\AppData\Roaming\local\bin:
    - haskell-dap.exe

    %

## Configuration

Set ghciCmd variable in the launch.json.  
Add these options.
+ --with-ghc
+ --ghc-options -B
  

### windows

    "ghciCmd": "stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show --with-ghc=C:/Users/[USERNAME]/AppData/Roaming/Local/bin/haskell-dap --ghc-options -BC:/Users/[USERNAME]/AppData/Local/Programs/stack/x86_64-windows/ghc-[VERSION]/lib",


### linux


    "ghciCmd": "stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show --with-ghc=/home/[USERNAME]/.local/bin/haskell-dap --ghc-options -B/home/[USERNAME]/.stack/programs/x86_64-linux/ghc-[VERSION]/lib/[VERSION]",
  



