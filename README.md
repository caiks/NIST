# MNIST - handwritten digits

This repository contains tests of the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa) using data from the [MNIST dataset](http://yann.lecun.com/exdb/mnist/). The AlignmentRepa repository is a fast Haskell implementation of some of the *practicable inducers* described in the paper *The Theory and Practice of Induction by Alignment* at https://greenlake.co.uk/. 

## Documentation

There is an analysis of this dataset [here](https://greenlake.co.uk/pages/dataset_NIST). 

## Installation

The `NIST` executables require the `AlignmentRepa` module which is in the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa). The `AlignmentRepa` module requires the [Haskell platform](https://www.haskell.org/downloads#platform) to be installed. The project is managed using [stack](https://docs.haskellstack.org/en/stable/).

Download the zip files or use git to get the NIST repository and the underlying Alignment and AlignmentRepa repositories -
```
cd
git clone https://github.com/caiks/Alignment.git
git clone https://github.com/caiks/AlignmentRepa.git
git clone https://github.com/caiks/NIST.git
```
Then download the dataset files, for example -
```
cd ~/NIST
wget http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz
```
Then build with the following -
```
cd ~/NIST
stack build --ghc-options -w

```

## Usage

The *practicable model induction* is described [here](https://greenlake.co.uk/pages/dataset_NIST_model#model3).

`NIST_engine3` Ubuntu 16.04 Intel(R) Xeon(R) Platinum 8175M CPU @ 2.50GHz using 1756 MB memory in 11505 seconds,

```
cd ~/NIST
stack exec NIST_engine3.exe +RTS -s >NIST_engine3.log 2>&1 &

tail -f NIST_engine3.log

```
To experiment with the dataset in the interpreter use `stack ghci` or `stack repl` for a run-eval-print loop (REPL) environment, 
```
cd ~/NIST
stack ghci --ghci-options -w

```
Press return when prompted to choose the main executable. Load `NISTDev` to import the modules and define various useful abbreviated functions,
```hs
:l NISTDev

(uu,hrtr) <- nistTrainBucketedIO 2

let digit = VarStr "digit"
let vv = uvars uu
let vvl = sgl digit
let vvk = vv `minus` vvl

let hr = hrev [i | i <- [0.. hrsize hrtr - 1], i `mod` 8 == 0] hrtr 

hrsize hr

let hrtr = undefined

let (wmax,lmax,xmax,omax,bmax,mmax,umax,pmax,fmax,mult,seed) = (2^10, 8, 2^10, 10, (10*3), 3, 2^8, 1, 15, 1, 5)

Just (uu1,df) <- decomperIO uu vvk hr wmax lmax xmax omax bmax mmax umax pmax fmax mult seed

summation mult seed uu1 df hr
(148378.04791361679,74189.02395680839)

BL.writeFile ("NIST_model1.json") $ decompFudsPersistentsEncode $ decompFudsPersistent df

```
If you wish to use compiled code rather than interpreted you may specify the following before loading `MUSHDev` -
```
:set -fobject-code

```
Note that some modules may become [unresolved](https://downloads.haskell.org/~ghc/7.10.3-rc1/users_guide/ghci-obj.html), for example,
```hs
rp $ Set.fromList [1,2,3]

<interactive>:9:1: Not in scope: ‘Set.fromList’
```
In this case, re-import the modules explicitly as defined in `NISTDev`, for example,
```hs
import qualified Data.Set as Set
import qualified Data.Map as Map
import Alignment
import AlignmentRepa
import AlignmentDevRepa hiding (aahr)

rp $ Set.fromList [1,2,3]
"{1,2,3}"

rp $ fudEmpty
"{}"
```

