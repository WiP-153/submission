add_instance main_cc MAIN_CC 1.0

add_connection cpu.data_master main_cc.s0 avalon
set_connection_parameter_value cpu.data_master/main_cc.s0 arbitrationPriority {1}
set_connection_parameter_value cpu.data_master/main_cc.s0 baseAddress {0x10000000}
set_connection_parameter_value cpu.data_master/main_cc.s0 defaultConnection {0}
add_connection clk.clk main_cc.clock clock
add_connection clk.clk_reset main_cc.reset reset

