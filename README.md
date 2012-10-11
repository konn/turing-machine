README
=======

> $ cabal install
$ turing-machine-example

Running
-------
```
mark H at Head and T at Tail on "11111", started from the third
11[0]111
1[0]1111
[0]11111
[0]_11111
[1]H11111
H[1]11111
H1[1]1111
H11[1]111
H111[1]11
H1111[1]1
H11111[1]_
H11111[2]T
==========

parses 0^{n}1^{n}
XXYY_OK![9]_
==========

parses 0^{n}1^{2n}; but fail.
[0]00111
[1]X0111
X[1]0111
X0[1]111
X0[2]Y11
X0Y[2]11
X0Y[3]Y1
X0[3]YY1
X[3]0YY1
[3]X0YY1
X[0]0YY1
X[1]XYY1
XX[1]YY1
XXY[1]Y1
XXYY[1]1
XXYY[2]Y
XXYYY[2]_
XXYYY[a]_
XXYYY_[b]_
XXYYY_[b]E
XXYYY_E[c]_
XXYYY_E[c]r
XXYYY_Er[d]_
XXYYY_Er[d]r
XXYYY_Err[e]_
XXYYY_Err[e]!
XXYYY_Err![f]_
==========

parses 0^{n}1^{2n}; but fail again.
[0]0111
[1]X111
X[1]111
X[2]Y11
XY[2]11
XY[3]Y1
X[3]YY1
[3]XYY1
X[0]YY1
XY[4]Y1
XYY[4]1
XYY[a]1
XYY1[a]_
XYY1_[b]_
XYY1_[b]E
XYY1_E[c]_
XYY1_E[c]r
XYY1_Er[d]_
XYY1_Er[d]r
XYY1_Err[e]_
XYY1_Err[e]!
XYY1_Err![f]_
==========

parses 0^{n}1^{2n}; success!.
[0]001111
[1]X01111
X[1]01111
X0[1]1111
X0[2]Y111
X0Y[2]111
X0Y[3]Y11
X0[3]YY11
X[3]0YY11
[3]X0YY11
X[0]0YY11
X[1]XYY11
XX[1]YY11
XXY[1]Y11
XXYY[1]11
XXYY[2]Y1
XXYYY[2]1
XXYYY[3]Y
XXYY[3]YY
XXY[3]YYY
XX[3]YYYY
X[3]XYYYY
XX[0]YYYY
XXY[4]YYY
XXYY[4]YY
XXYYY[4]Y
XXYYYY[4]_
XXYYYY[5]_
XXYYYY_[6]_
XXYYYY_[6]O
XXYYYY_O[7]_
XXYYYY_O[7]K
XXYYYY_OK[8]_
XXYYYY_OK[8]!
XXYYYY_OK![9]_
```
