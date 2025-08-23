# haskell-code

《Learn You a Haskell for Great Good!》

```sh
# cabal 是用于管理 Haskell 依赖库的程序
sudo apt-get install cabal
# 安装 stack
curl -sSL https://get.haskellstack.org/ | sh
# build project
stack build
# format code
stack exec -- ormolu --mode inplace src/Chapter5.hs
# lint code
stack exec -- hlint src/
# exec main
stack exec haskell-code-exe
```
