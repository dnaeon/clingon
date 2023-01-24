# clingon-demo

A demo CLI application showing some of the features of the Common Lisp system
for parsing command-line arguments -- clingon.

## Usage

``` shell
clingon-demo [global-options] [<command>] [command-options] [arguments ...]
```

## Options

`clingon-demo` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit
  -v, --verbose                 how noisy we want to be [default: 0]

```

## Sub Commands

`clingon-demo` provides the following sub commands:

``` shell
  greet, hi, hey  greets people
  logging, log    configure the logging system
  math            perform basic math on integers
  echo            echoes back each argument on a newline
  engine          start or stop an imaginary engine
  print-doc       print the documentation
  sleep           sleeps for the given period of time
  zsh-completion  generate the Zsh completion script
  dot             generate tree representation in Dot format

```

## Authors

* Marin Atanasov Nikolov <dnaeon@gmail.com>

## License

BSD 2-Clause

# clingon-demo greet

`clingon-demo greet` -- greets people

## Usage

``` shell
clingon-demo [global-options] greet [options] [arguments ...]
```

## Options

`clingon-demo greet` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit
  -u, --user <VALUE>            Person to greet [default: stranger] [env: $USER]

```

## Examples

Greet someone:

``` shell
clingon-demo greet --user Lisper
```

# clingon-demo logging

`clingon-demo logging` -- configure the logging system

## Usage

``` shell
clingon-demo [global-options] logging [<command>] [command-options] [arguments ...]
```

## Options

`clingon-demo logging` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit
  -l, --level <LEVEL>           level to configure [default: info] [env: $LOG_LEVEL] [choices: info,
                                warn, error, debug]

```

## Sub Commands

`clingon-demo logging` provides the following sub commands:

``` shell
  enable   enables logging
  disable  disables logging

```

## Examples

Configure logging level:

``` shell
clingon-demo -vvv logging --level=debug
```

Enable logging:

``` shell
clingon-demo logging enable
```

Disable logging:

``` shell
clingon-demo logging disable
```

# clingon-demo logging enable

`clingon-demo logging enable` -- enables logging

## Usage

``` shell
clingon-demo logging [global-options] enable [options] [arguments ...]
```

## Options

`clingon-demo logging enable` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit

```

# clingon-demo logging disable

`clingon-demo logging disable` -- disables logging

## Usage

``` shell
clingon-demo logging [global-options] disable [options] [arguments ...]
```

## Options

`clingon-demo logging disable` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit

```

# clingon-demo math

`clingon-demo math` -- perform basic math on integers

## Usage

``` shell
clingon-demo math -o <OPERATION> -i <INT> ...
```

## Options

`clingon-demo math` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit
  -i, --int <ITEM>              integers to work on
  -o, --operation <VARIANT>     operation to perform [choices: add, sub, mul, div]

```

## Examples

Sum some numbers:

``` shell
clingon-demo math -o add -i 1 -i 42 -i 84
```

Multiply some numbers:

``` shell
clingon-demo math -o mul -i 2 -i 3 -i 4
```

# clingon-demo echo

`clingon-demo echo` -- echoes back each argument on a newline

## Usage

``` shell
clingon-demo echo [ARGUMENT ...]
```

## Options

`clingon-demo echo` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit

```

## Examples

Echo back each argument on a new line:

``` shell
clingon-demo echo foo bar baz
```

# clingon-demo engine

`clingon-demo engine` -- start or stop an imaginary engine

## Usage

``` shell
clingon-demo engine -s <STATE>
```

## Options

`clingon-demo engine` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit
  -s, --state <STATE>           state of our engine

```

## Examples

Start engine:

``` shell
clingon-demo engine --state=on
```

Stop engine:

``` shell
clingon-demo engine --state=off
```

# clingon-demo print-doc

`clingon-demo print-doc` -- print the documentation

## Usage

``` shell
clingon-demo print-doc 
```

## Options

`clingon-demo print-doc` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit

```

# clingon-demo sleep

`clingon-demo sleep` -- sleeps for the given period of time

## Usage

``` shell
clingon-demo [global-options] sleep [options] [arguments ...]
```

## Options

`clingon-demo sleep` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit
  -s, --seconds <INT>           number of seconds to sleep [default: 60]

```

## Examples

Sleep for 60 seconds. Send SIGINT via CTRL-C to catch the signal

``` shell
clingon-demo sleep --seconds 60
```

# clingon-demo zsh-completion

`clingon-demo zsh-completion` -- generate the Zsh completion script

## Usage

``` shell
clingon-demo zsh-completion 
```

## Options

`clingon-demo zsh-completion` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit

```

# clingon-demo dot

`clingon-demo dot` -- generate tree representation in Dot format

## Usage

``` shell
clingon-demo dot 
```

## Options

`clingon-demo dot` accepts the following options:

``` shell
      --help                    display usage information and exit
      --persistent-opt <VALUE>  example persistent option
      --version                 display version and exit

```

