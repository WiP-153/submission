# ABCRO is a classical reactive program which waits A, B and C to emit O reinitialized with R.

- Typing:
```
$  ./mixc examples/abcro/abcro.ml -main abcro -ty
val fby : ('a * 'a) => 'a
val edge : bool => bool
val aro : (bool * bool) => bool
val abro : (bool * bool * bool) => bool
val abcro : (bool * bool * bool * bool) => bool

(bool * bool * bool * bool) => bool
```

- Interpretation:
```
$  ./mixc examples/abcro/abcro.ml -main abcro -arg "(false,false,false,false);(true,false,false,false);(false,true,true,false);(false,false,false,false);(true,true,true,false);(true,true,true,true);(true,true,true,false)" -interp
val fby : ('a * 'a) => 'a
val edge : bool => bool
val aro : (bool * bool) => bool
val abro : (bool * bool * bool) => bool
val abcro : (bool * bool * bool * bool) => bool

(false, false, false, false) --> false 
(true, false, false, false) --> false 

(false, true, true, false) --> true 
(false, false, false, false) --> false 

(true, true, true, false) --> false 
(true, true, true, true) --> false 

(true, true, true, false) --> true 
```

- Compilation to VHDL:
```
$  ./mixc examples/abcro/abcro.ml -main abcro -arg "(false,false,false,false);(true,false,false,false);(false,true,true,false);(false,false,false,false);(true,true,true,false);(true,true,true,true);(true,true,true,false)"
val fby : ('a * 'a) => 'a
val edge : bool => bool
val aro : (bool * bool) => bool
val abro : (bool * bool * bool) => bool
val abcro : (bool * bool * bool * bool) => bool

vhdl code generated in vhdl/main.vhdl 
testbench generated in vhdl/tb_main.vhdl for software RTL simulation using GHDL.
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

The generated trace `tb.vcd` is used to obtain the electronic waveform view `abcro.png` using the tool GTKWave.
