library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal run    : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 3);
       signal result : out value(0 to 0));
       
end entity;
architecture rtl of main is

  type t_state is (Compute);
  signal state: t_state;
  
  begin
    process(reset, clk)
      variable v50_init : value(0 to 0);
      variable v54 : value(0 to 1);
      variable v64 : value(0 to 0);
      variable v68 : value(0 to 1);
      variable v87 : value(0 to 3);
      variable v86 : value(0 to 1);
      variable vb11 : value(0 to 0);
      variable v52_init : value(0 to 0);
      variable v85 : value(0 to 0);
      variable v79 : value(0 to 1);
      variable vr13 : value(0 to 0);
      variable v51 : value(0 to 0);
      variable v84 : value(0 to 1);
      variable v71 : value(0 to 1);
      variable vanf45 : value(0 to 1);
      variable v65 : value(0 to 1);
      variable v77 : value(0 to 1);
      variable v88 : value(0 to 3);
      variable v66 : value(0 to 1);
      variable v58 : value(0 to 1);
      variable v74 : value(0 to 1);
      variable v52 : value(0 to 0);
      variable v67 : value(0 to 1);
      variable v48_init : value(0 to 0);
      variable v75 : value(0 to 1);
      variable v_28 : value(0 to 0);
      variable v49_init : value(0 to 0);
      variable v83 : value(0 to 1);
      variable v62 : value(0 to 0);
      variable v89 : value(0 to 3);
      variable va10 : value(0 to 0);
      variable v81 : value(0 to 1);
      variable vanf40 : value(0 to 0);
      variable vanf39 : value(0 to 0);
      variable vx29 : value(0 to 0);
      variable v72 : value(0 to 1);
      variable vanf35 : value(0 to 0);
      variable vanf37 : value(0 to 0);
      variable v76 : value(0 to 0);
      variable v69 : value(0 to 1);
      variable v73 : value(0 to 0);
      variable vc12 : value(0 to 0);
      variable v49 : value(0 to 0);
      variable v80 : value(0 to 1);
      variable vt14 : value(0 to 0);
      variable v78 : value(0 to 1);
      variable vanf38 : value(0 to 0);
      variable v48 : value(0 to 0);
      variable v90 : value(0 to 3);
      variable v70 : value(0 to 0);
      variable vanf43 : value(0 to 0);
      variable v47 : value(0 to 1);
      variable v47_init : value(0 to 0);
      variable v82 : value(0 to 0);
      variable v63 : value(0 to 1);
      variable v50 : value(0 to 1);
      variable v51_init : value(0 to 0);
      variable vanf34 : value(0 to 0);
      
    begin
      if (reset = '1') then
        v50_init := "0";
        v54 := "00";
        v64 := "0";
        v68 := "00";
        v87 := X"0";
        v86 := "00";
        vb11 := "0";
        v52_init := "0";
        v85 := "0";
        v79 := "00";
        vr13 := "0";
        v51 := "0";
        v84 := "00";
        v71 := "00";
        vanf45 := "00";
        v65 := "00";
        v77 := "00";
        v88 := X"0";
        v66 := "00";
        v58 := "00";
        result <= "0";
        v74 := "00";
        v52 := "0";
        v67 := "00";
        v48_init := "0";
        v75 := "00";
        v_28 := "0";
        v49_init := "0";
        v83 := "00";
        v62 := "0";
        v89 := X"0";
        va10 := "0";
        v81 := "00";
        vanf40 := "0";
        vanf39 := "0";
        vx29 := "0";
        v72 := "00";
        vanf35 := "0";
        vanf37 := "0";
        v76 := "0";
        v69 := "00";
        v73 := "0";
        vc12 := "0";
        v49 := "0";
        v80 := "00";
        vt14 := "0";
        v78 := "00";
        vanf38 := "0";
        v48 := "0";
        v90 := X"0";
        v70 := "0";
        vanf43 := "0";
        v47 := "00";
        v47_init := "0";
        v82 := "0";
        v63 := "00";
        v50 := "00";
        v51_init := "0";
        vanf34 := "0";
        rdy <= "1";
        state <= Compute;
        
      elsif rising_edge(clk) then
        if run = '1' then
          case state is
          when Compute =>
            rdy <= "0";
            v90 := argument;
            va10 := v90(0 to 0);
            v89 := argument;
            vb11 := v89(1 to 1);
            v88 := argument;
            vc12 := v88(2 to 2);
            v87 := argument;
            vr13 := v87(3 to 3);
            if mixc_not(v52_init)(0) = '1' then
              v52 := "0";
              v52_init := "1";
            end if;
            v86 := v52 & va10;
            vanf39 := mixc_or(v86);
            v85 := vr13;
            vanf40 := mixc_not(v85);
            v84 := vanf39 & vanf40;
            vanf38 := mixc_and(v84);
            v52 := vanf38;
            vanf34 := v52;
            if mixc_not(v51_init)(0) = '1' then
              v51 := "0";
              v51_init := "1";
            end if;
            v83 := v51 & vb11;
            vanf39 := mixc_or(v83);
            v82 := vr13;
            vanf40 := mixc_not(v82);
            v81 := vanf39 & vanf40;
            vanf38 := mixc_and(v81);
            v51 := vanf38;
            vanf35 := v51;
            v80 := vanf34 & vanf35;
            vanf37 := mixc_and(v80);
            if mixc_not(v50_init)(0) = '1' then
              v50 := "0" & vanf37;
              v50_init := "1";
            end if;
            v79 := v50;
            vanf45 := vanf37 & v79(0 to 0);
            v50 := vanf45;
            v58 := v50;
            v78 := v58;
            v_28 := v78(0 to 0);
            v77 := v58;
            vx29 := v77(1 to 1);
            v76 := vx29;
            vanf43 := mixc_not(v76);
            v75 := vanf43 & vanf37;
            vt14 := mixc_and(v75);
            if mixc_not(v49_init)(0) = '1' then
              v49 := "0";
              v49_init := "1";
            end if;
            v74 := v49 & vt14;
            vanf39 := mixc_or(v74);
            v73 := vr13;
            vanf40 := mixc_not(v73);
            v72 := vanf39 & vanf40;
            vanf38 := mixc_and(v72);
            v49 := vanf38;
            vanf34 := v49;
            if mixc_not(v48_init)(0) = '1' then
              v48 := "0";
              v48_init := "1";
            end if;
            v71 := v48 & vc12;
            vanf39 := mixc_or(v71);
            v70 := vr13;
            vanf40 := mixc_not(v70);
            v69 := vanf39 & vanf40;
            vanf38 := mixc_and(v69);
            v48 := vanf38;
            vanf35 := v48;
            v68 := vanf34 & vanf35;
            vanf37 := mixc_and(v68);
            if mixc_not(v47_init)(0) = '1' then
              v47 := "0" & vanf37;
              v47_init := "1";
            end if;
            v67 := v47;
            vanf45 := vanf37 & v67(0 to 0);
            v47 := vanf45;
            v54 := v47;
            v66 := v54;
            v_28 := v66(0 to 0);
            v65 := v54;
            vx29 := v65(1 to 1);
            v64 := vx29;
            vanf43 := mixc_not(v64);
            v63 := vanf43 & vanf37;
            v62 := mixc_and(v63);
            result <= v62;
            rdy <= "1";
            state <= Compute;
          end case;
        end if;
    end if;
  end process;
end architecture;
