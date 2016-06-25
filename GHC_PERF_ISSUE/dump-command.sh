#!/bin/bash

stack ghc -- -Wall -O2 -funbox-strict-fields -ddump-stg -ddump-simpl -ddump-cmm -ddump-asm -ddump-to-file -dsuppress-all ../Data/Text/NormalizeNative.hs
