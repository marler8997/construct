Miscellaneous Documentation
================================================================================

## import and import relative

The import construct takes an optional keyword "relative" and a string.
```C
import "std/io"
import relative "../util"
```
A non-relative import will search the import paths for the file.  In this
example, it would search for
```
<importpath1>/std/io.con
<importpath2>/std/io.con
...
```
A relative import will only search relative to the file that it appears in. So
if this file was in "/mysource/program/main.con", it would search for "../util"
at "/mysource/util.con".

A relative import is recommended ONLY WHEN the source file being imported will
always be in the same relative location.
