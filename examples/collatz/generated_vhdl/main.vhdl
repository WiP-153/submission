library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal run    : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 15);
       signal result : out value(0 to 7));
       
end entity;
architecture rtl of main is

  type t_state is (Compute);
  signal state: t_state;
  type t_vstate_var139 is (vcompute140, vvloop2396);
  signal vstate_var139: t_vstate_var139;
  type t_vstate_var137 is (vcompute138, vvloop2355);
  signal vstate_var137: t_vstate_var137;
  type t_vvloop2355 is (I1);
  signal vvloop2355_id : t_vvloop2355;
  type t_vvloop2396 is (I2);
  signal vvloop2396_id : t_vvloop2396;
  signal vvloop2355_arg : value(0 to 15);
  signal v133 : value(0 to 31);
  signal v92 : value(0 to 31);
  signal vvloop2396_arg : value(0 to 15);
  
  begin
    process(reset, clk)
      variable v54 : value(0 to 8);
      variable v45 : value(0 to 8);
      variable vvanf32114 : value(0 to 7);
      variable vvanf3169 : value(0 to 0);
      variable v87 : value(0 to 15);
      variable v44 : value(0 to 8);
      variable vx10 : value(0 to 7);
      variable v86 : value(0 to 15);
      variable vt112 : value(0 to 7);
      variable v95 : value(0 to 8);
      variable vvanf29108 : value(0 to 7);
      variable v85 : value(0 to 15);
      variable vvn2564 : value(0 to 7);
      variable v126 : value(0 to 15);
      variable v129 : value(0 to 15);
      variable v53 : value(0 to 8);
      variable v_13 : value(0 to 0);
      variable vvanf2866 : value(0 to 0);
      variable vvanf2967 : value(0 to 7);
      variable v91 : value(0 to 15);
      variable v122 : value(0 to 7);
      variable vvanf34116 : value(0 to 7);
      variable v51 : value(0 to 7);
      variable v84 : value(0 to 15);
      variable v41_init : value(0 to 0);
      variable v88 : value(0 to 15);
      variable v127 : value(0 to 15);
      variable vvanf37111 : value(0 to 7);
      variable v52 : value(0 to 15);
      variable vvanf3576 : value(0 to 7);
      variable vvanf3475 : value(0 to 7);
      variable vt214 : value(0 to 7);
      variable vanf27 : value(0 to 0);
      variable v83 : value(0 to 15);
      variable v135 : value(0 to 15);
      variable vvanf3871 : value(0 to 7);
      variable v89 : value(0 to 15);
      variable vvt2665 : value(0 to 7);
      variable v128 : value(0 to 15);
      variable vvanf28107 : value(0 to 0);
      variable vvn25105 : value(0 to 7);
      variable vvanf3273 : value(0 to 7);
      variable v81 : value(0 to 7);
      variable vvanf31110 : value(0 to 0);
      variable v132 : value(0 to 15);
      variable v124 : value(0 to 15);
      variable vvanf3770 : value(0 to 7);
      variable vvv21758 : value(0 to 7);
      variable vvanf35117 : value(0 to 7);
      variable v131 : value(0 to 15);
      variable vvt26106 : value(0 to 7);
      variable v94 : value(0 to 8);
      variable v136 : value(0 to 15);
      variable v_15 : value(0 to 0);
      variable v41 : value(0 to 8);
      variable vvanf38112 : value(0 to 7);
      variable v49 : value(0 to 31);
      variable v130 : value(0 to 15);
      variable v123 : value(0 to 15);
      variable v125 : value(0 to 15);
      variable v90 : value(0 to 15);
      variable vvv51999 : value(0 to 7);
      variable v82 : value(0 to 15);
      variable v42 : value(0 to 8);
      variable v50 : value(0 to 31);
      variable v42_init : value(0 to 0);
      variable vy11 : value(0 to 7);
      
    begin
      if (reset = '1') then
        v54 := "0"& X"00";
        v45 := "0"& X"00";
        vvanf32114 := X"00";
        vvanf3169 := "0";
        v87 := X"0000";
        v44 := "0"& X"00";
        vx10 := X"00";
        v86 := X"0000";
        vt112 := X"00";
        v95 := "0"& X"00";
        vvanf29108 := X"00";
        v85 := X"0000";
        vvn2564 := X"00";
        v126 := X"0000";
        v129 := X"0000";
        v53 := "0"& X"00";
        v_13 := "0";
        vvanf2866 := "0";
        vvanf2967 := X"00";
        v91 := X"0000";
        v122 := X"00";
        vvanf34116 := X"00";
        v51 := X"00";
        v84 := X"0000";
        v41_init := "0";
        v88 := X"0000";
        v127 := X"0000";
        vvanf37111 := X"00";
        vvloop2355_arg <= X"0000";
        v133 <= X"00000000";
        result <= X"00";
        v52 := X"0000";
        vvanf3576 := X"00";
        vvanf3475 := X"00";
        vt214 := X"00";
        vanf27 := "0";
        v83 := X"0000";
        v135 := X"0000";
        vvanf3871 := X"00";
        v89 := X"0000";
        vvt2665 := X"00";
        v128 := X"0000";
        vvanf28107 := "0";
        vvn25105 := X"00";
        vvanf3273 := X"00";
        v81 := X"00";
        vvanf31110 := "0";
        v132 := X"0000";
        v124 := X"0000";
        vvanf3770 := X"00";
        vvv21758 := X"00";
        vvanf35117 := X"00";
        v131 := X"0000";
        vvt26106 := X"00";
        v92 <= X"00000000";
        v94 := "0"& X"00";
        v136 := X"0000";
        v_15 := "0";
        v41 := "0"& X"00";
        vvanf38112 := X"00";
        v49 := X"00000000";
        v130 := X"0000";
        v123 := X"0000";
        v125 := X"0000";
        v90 := X"0000";
        vvv51999 := X"00";
        vvloop2396_arg <= X"0000";
        v82 := X"0000";
        v42 := "0"& X"00";
        v50 := X"00000000";
        v42_init := "0";
        vy11 := X"00";
        rdy <= "1";
        state <= Compute;
        vstate_var139 <= vcompute140;
        vstate_var137 <= vcompute138;
        
      elsif rising_edge(clk) then
        if run = '1' then
          case state is
          when Compute =>
            rdy <= "0";
            v136 := argument;
            vx10 := v136(0 to 7);
            v135 := argument;
            vy11 := v135(8 to 15);
            if mixc_not(v42_init)(0) = '1' then
              v42 := "0000" & X"0" & "0";
              v42_init := "1";
            end if;
            case vstate_var139 is
            when vvloop2396 =>
              v132 := vvloop2396_arg;
              vvn25105 := v132(0 to 7);
              v131 := vvloop2396_arg;
              vvt26106 := v131(8 to 15);
              v130 := vvn25105 & "0000" & X"1";
              vvanf28107 := mixc_eq(v130);
              v129 := vvn25105 & "0000" & X"2";
              vvanf29108 := mixc_mod(v129);
              v128 := vvanf29108 & "0000" & X"0";
              vvanf31110 := mixc_eq(v128);
              v127 := vvn25105 & "0000" & X"2";
              vvanf37111 := mixc_div(v127);
              v126 := vvt26106 & "0000" & X"1";
              vvanf38112 := mixc_add(v126);
              if vvanf28107(0) = '1' then
                v122 := vvt26106;
                case vvloop2396_id is
                when I2 =>
                  vvv51999 := v122;
                  v42 := vvv51999 & "1";
                  v50 := v133;
                  vstate_var139 <= vcompute140;
                end case;
              else
                v125 := "0000" & X"3" & vvn25105;
                vvanf32114 := mixc_mult(v125);
                v124 := vvanf32114 & "0000" & X"1";
                vvanf34116 := mixc_add(v124);
                v123 := vvt26106 & "0000" & X"1";
                vvanf35117 := mixc_add(v123);
                if vvanf31110(0) = '1' then
                  vvloop2396_arg <= vvanf37111 & vvanf38112;
                  vstate_var139 <= vvloop2396;
                else
                  vvloop2396_arg <= vvanf34116 & vvanf35117;
                  vstate_var139 <= vvloop2396;
                end if;
              end if;
            when vcompute140 =>
              v42 := "0000" & X"0" & "0";
              vvloop2396_arg <= vx10 & "0000" & X"1";
              vvloop2396_id <= I2;
              vstate_var139 <= vvloop2396;
            end case;
            v44 := v42;
            v95 := v44;
            vt112 := v95(0 to 7);
            v94 := v44;
            v_13 := v94(8 to 8);
            if mixc_not(v41_init)(0) = '1' then
              v41 := "0000" & X"0" & "0";
              v41_init := "1";
            end if;
            case vstate_var137 is
            when vvloop2355 =>
              v91 := vvloop2355_arg;
              vvn2564 := v91(0 to 7);
              v90 := vvloop2355_arg;
              vvt2665 := v90(8 to 15);
              v89 := vvn2564 & "0000" & X"1";
              vvanf2866 := mixc_eq(v89);
              v88 := vvn2564 & "0000" & X"2";
              vvanf2967 := mixc_mod(v88);
              v87 := vvanf2967 & "0000" & X"0";
              vvanf3169 := mixc_eq(v87);
              v86 := vvn2564 & "0000" & X"2";
              vvanf3770 := mixc_div(v86);
              v85 := vvt2665 & "0000" & X"1";
              vvanf3871 := mixc_add(v85);
              if vvanf2866(0) = '1' then
                v81 := vvt2665;
                case vvloop2355_id is
                when I1 =>
                  vvv21758 := v81;
                  v41 := vvv21758 & "1";
                  v49 := v92;
                  vstate_var137 <= vcompute138;
                end case;
              else
                v84 := "0000" & X"3" & vvn2564;
                vvanf3273 := mixc_mult(v84);
                v83 := vvanf3273 & "0000" & X"1";
                vvanf3475 := mixc_add(v83);
                v82 := vvt2665 & "0000" & X"1";
                vvanf3576 := mixc_add(v82);
                if vvanf3169(0) = '1' then
                  vvloop2355_arg <= vvanf3770 & vvanf3871;
                  vstate_var137 <= vvloop2355;
                else
                  vvloop2355_arg <= vvanf3475 & vvanf3576;
                  vstate_var137 <= vvloop2355;
                end if;
              end if;
            when vcompute138 =>
              v41 := "0000" & X"0" & "0";
              vvloop2355_arg <= vy11 & "0000" & X"1";
              vvloop2355_id <= I1;
              vstate_var137 <= vvloop2355;
            end case;
            v45 := v41;
            v54 := v45;
            vt214 := v54(0 to 7);
            v53 := v45;
            v_15 := v53(8 to 8);
            v52 := vt112 & vt214;
            vanf27 := mixc_gt(v52);
            v51 := mixc_if(vanf27 & vt112 & vt214);
            result <= v51;
            rdy <= "1";
            state <= Compute;
          end case;
        end if;
    end if;
  end process;
end architecture;
