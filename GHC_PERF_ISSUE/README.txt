Performance issue:
  Some heap checks and allocations are placed in common paths of multiple
  branches. These checks can be made more specific and moved to only those
  paths which really need them.

By default ghc-7.10.3 is being used in this build. It can be changed to 8.0.1
and more or less everything remains the same.

--------------------------------------------------------------------------

Source/cmm/asm examination:

The loop in question is: Data/Text/NormalizeNative:normalize
This calls the code in Data/Unicode/Internal/NormalizeStream.hs (unstream)

Generated simpl/stg/cmm/asm files are committed in this repo in Data/Text
directory.

To create simpl/stg/cmm/asm/dumps again for NormalizeNative.hs:
  stack build
  ./dump-command.sh

To search for the relevant parts in:
  ASM: go to the end of file and reverse search for "192". It will lead to cmp
  instruction in the code block we are interested in.
  CMM: search for the label of the asm block without the leading "_"

--------------------------------------------------------------------------

Execution Flow Tracing (assembly level):

For execution flow of loop asm instructions with comments see
"decompose-loop*.asm" files in this directory.

See GDB-TRACE.txt for instructions to trace execution in gdb.

--------------------------------------------------------------------------

Benchmarks:
  stack build :bench --benchmark-arguments unicode-transforms-text/NFD/English
