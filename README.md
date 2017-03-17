# unpacker

## Build
make

This produces a self-contained escript called 'unpacker' under _build/default/bin

## Usage
| Flags        | Requirement     | Description  |
| ------------- |:-------------:| -----:|
|\<directory\>|[M]|Path to directory containing files to be moved or unpacked|
|-test yes|[O]|To execute unpacker without actually unpacking/moving files|
|-config \<path to config.yaml\>|[O]|Path to config files|
|-type \<tv\|movie\>|[O]|Override guessit type|
