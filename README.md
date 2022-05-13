# zagain

Have a play with the Z-machine again. This time in Haskell. With an eye to doing some static recompilation. Perhaps using this a learning opportunity to target WASM.

# running regression tests
```
make reg; git diff gen
```


# installing `emscripten`

This is what I did:

```
cd ~/code
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk/
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh
```
