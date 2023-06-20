- Typing:

```
./mixc examples/collatz/collatz.ml -ty
val collatz : int<'a> -> int<'b>
val max : (int<'a> * int<'a>) => int<'a>
val main : (int<8> * int<8>) => int<8>

(int<8> * int<8>) => int<8>
```

- Interpretation:

```
$ ./mixc examples/collatz/collatz.ml -arg "(1,5);(3,2);(7,6);(1,4);(2,2);(3,2);(2,3)" -interp
val collatz : int<'a> -> int<'b>
val max : (int<'a> * int<'a>) => int<'a>
val main : (int<8> * int<8>) => int<8>

(4, 1) --> 0 
(3, 2) --> 1 
(7, 6) --> 0 
(1, 4) --> 3 
(2, 2) --> 1 

(3, 2) --> 0 
(2, 3) --> 3 
```

- Compilation to VHDL:

```
$ ./mixc examples/collatz/collatz.ml -arg "(4,1);(3,2);(7,6);(1,4);(2,2);(3,2);(2,3)"
val collatz : int<'a> -> int<'b>
val max : (int<'a> * int<'a>) => int<'a>
val main : (int<8> * int<8>) => int<8>

vhdl code generated in vhdl/main.vhdl 
testbench generated in vhdl/tb_main.vhdl for simulation on a PC using GHDL.
```

- software RTL simulation using GHDL:

```
$ make simul
cd vhdl; make NS=1000
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=1000ns
ghdl:info: simulation stopped by --stop-time @1us
```

The generated trace `tb.vcd` is used to obtain the electronic waveform view `collatz.png` using the tool GTKWave.


