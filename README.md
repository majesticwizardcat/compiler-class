# EEL compiler

## Compiling

Run with:

```
./compiler.py somecode.eel
```

## Testing

### Unit tests

To run the unit tests:

```
python3 test.py
```

### End-to-end

You need `runhaskell` installed:

```
sudo apt update && sudo apt install ghc
```

Run with:

```
./TestRunner.hs
```

## Running MIPS assembly code

You need the gcc MIPS toolchain installed:

```
sudo apt update && sudo apt install {cpp,gcc}-mips-linux-gnu qemu-{user,-system-mips}
```

To assemble a `demo.s` file:

```
mips-linux-gnu-gcc -static -mips32r5 demo.s -o demo
```

Run the binary with:

```
qemu-mips demo
```
