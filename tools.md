# JPP - Tools guide

<br />

### C/C++

```shell
# Installation:
# gcc
sudo apt install gcc
gcc --version

# g++
sudo apt install g++
g++ --version
```

```shell
# Usage:
# gcc
gcc -o <binary-name> <program>.c
./<program>

# g++
g++ -o <binary-name> <program>.cpp
./<program>
```



<br />

### Ada

```shell
# Installation:
sudo apt install gnat
gnat compile --version
```

```shell
# Usage:
# Compile (out: binary) & run
gnatmake <program>.adb
./<program>

# Clean
gnatclean <program>
```

<br />

### Java

```shell
# Installation:
sudo apt install openjdk-17-jdk
java --version
```

```shell
# Usage:
javac <program>.java
java <program>
```

```shell
# Building a simple maven project:
mvn archetype:generate -DgroupId="com.PROJECT_NAME.app" -DartifactId="APP_NAME" -DarchetypeArtifactId="maven-archetype-quickstart" -DarchetypeVersion="1.4" -DinteractiveMode="false"

# Running tests:
cd <project-dir>/<APP_NAME>
mvn test
```

<br />

### GoLang

```shell
# Installation:
sudo apt install go
go version
```

```shell
# Install specific version, eg. 1.21.2:
wget https://go.dev/dl/go1.21.2.linux-amd64.tar.gz
sudo tar -xvf go1.21.0.linux-amd64.tar.gz
sudo mv go /usr/local

# Add the following to ~/.bashrc
export GOPATH="/usr/local/go"
export PATH="$PATH:$GOPATH/bin"

# In terminal
source ~/.bashrc
go version
```

```shell
# Usage:
go mod init <module>

# Compile (out: binary)
go build -o <binary-name> <program>.go
./<program>

# Run (without compilation)
go run <program>.go
```

<br />

### Haskell

```shell
# Installation:
sudo apt install ghc
ghc --version
```

```shell
# Usage:
# Compile (out: binary)
ghc <program>.hs
./<progam>
```

<br />

### Common Lisp

```shell
# Installation:
sudo apt install clisp
clisp --version
```

```shell
# Usage:
# Compile & run
clisp -c <program>.lisp
clisp -x '(load "<program>.fas")'

# Run (without compilation)
clisp <program>.lisp
```

<br />

### Standard ML

```shell
# Installation:
sudo apt install smlnj
sml --version # Ctrl+D to exit
```

```shell
# Usage:
sml -m < <program>.sml
```

<br />

### Prolog

```shell
# Installation:
sudo apt install swi-prolog
swipl --version
```

```shell
# Usage:
swipl -s <program>.pl
# In the interpreter:
?- <main-func>.
```

> **NOTE:** This can be encapsulated to a `run.sh` bash script
> ```bash
> #!/bin/bash
> # Load the Prolog file and execute the main function
> swipl -g "consult('$1'), $2, halt." -t 'halt.'
> ```
>
> ```shell
> # Usage:
> ./run.sh <program>.pl <main-func>
> ```
