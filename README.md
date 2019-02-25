# MNIST - handwritten digits

This repository contains tests of the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa) using data from the [MNIST dataset](http://yann.lecun.com/exdb/mnist/). The AlignmentRepa repository is a fast Haskell implementation of some of the *practicable inducers* described in the paper *The Theory and Practice of Induction by Alignment* at https://greenlake.co.uk/. 

## Documentation

There is an analysis of this dataset [here](https://greenlake.co.uk/pages/dataset_NIST), with sections (a) [predicting digit without modelling](https://greenlake.co.uk/pages/dataset_NIST#Predicting_digit_without_modelling) and (b) [induced modelling of digit](https://greenlake.co.uk/pages/dataset_NIST#Induced_modelling_of_digit). 

## Installation

The `NIST` executables require the `AlignmentRepa` module which is in the [AlignmentRepa repository](https://github.com/caiks/AlignmentRepa). See the AlignmentRepa repository for installation instructions of the Haskell compiler and libraries.

Then download the zip files or use git to get the NIST repository and the underlying Alignment and AlignmentRepa repositories -
```
cd
git clone https://github.com/caiks/Alignment.git
git clone https://github.com/caiks/AlignmentRepa.git
git clone https://github.com/caiks/NIST.git
```
Then download the dataset files, for example -
```
cd NIST
wget http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
wget http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz
```

## Usage

The *practicable model induction* is described [here](https://greenlake.co.uk/pages/dataset_NIST_model1).

`NIST_engine1` runs on a Ubuntu 16.04 Pentium CPU G2030 @ 3.00GHz using 1883 MB total memory and takes 6454 seconds,

```
cd ../Alignment
rm *.o *.hi

cd ../AlignmentRepa
rm *.o *.hi

gcc -fPIC -c AlignmentForeign.c -o AlignmentForeign.o -O3

cd ../NIST
rm *.o *.hi

ghc -i../Alignment -i../AlignmentRepa ../AlignmentRepa/AlignmentForeign.o NIST_engine1.hs -o NIST_engine1.exe -rtsopts -O2

./NIST_engine1.exe +RTS -s >NIST_engine1.log 2>&1 &

tail -f NIST_engine1.log

```

To experiment with the dataset in the interpreter,
```
cd ../Alignment
rm *.o *.hi

cd ../AlignmentRepa
rm *.o *.hi

gcc -fPIC -c AlignmentForeign.c -o AlignmentForeign.o -O3

cd ../NIST

ghci -i../Alignment -i../AlignmentRepa ../AlignmentRepa/AlignmentForeign.o
```

```hs
:set -fobject-code
:l NISTDev
```
Then exit the interpreter,
```
rm NISTDev.o

ghci -i../Alignment -i../AlignmentRepa ../AlignmentRepa/AlignmentForeign.o
```

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
(145211.97758613623,72018.94728089121)

BL.writeFile ("NIST_model1.json") $ decompFudsPersistentsEncode $ decompFudsPersistent df

```

