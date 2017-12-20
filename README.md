
# haskell-dap

The goal is "let GHCi talk Debug Adapter Protocol".

## Limitation
currently this project is experimental.

* supporting ghc-8.0.2
* need stack project.
* checked on windows and linux.

## Features

### inspect variables

![01_inspect_variables.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/01_inspect_variables.gif)  

## Install

install vscode and phoityne extension.

### Run stack install

    % git clone https://github.com/phoityne/haskell-dap
    %
    % cd haskell-dap
    %
    % stack install
      . . . . .
    %

## Configuration

Set ghciCmd variable in the launch.json.  
Add these options.
+ --with-ghc
+ --ghc-options -B
  

### windows

    "ghciCmd": "stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show --with-ghc=C:/Users/[username]/AppData/Roaming/Local/bin/haskell-dap --ghc-options -BC:/Users/[username]/AppData/Local/Programs/stack/x86_64-windows/ghc-8.0.2/lib",


### linux


    "ghciCmd": "stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show --with-ghc=/home/[username]/.local/bin/haskell-dap --ghc-options -B/home/[username]/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2",
  



