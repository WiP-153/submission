
library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package runtime is
    alias value is std_logic_vector;
    function mixc_add (arg: value) return value;
    function mixc_sub (arg: value) return value;
    function mixc_mult (arg: value) return value;
    function mixc_div (arg: value) return value;
    function mixc_mod (arg: value) return value;
    function mixc_eq (arg: value) return value;
    function mixc_neq (arg: value) return value;
    function mixc_lt (arg: value) return value;
    function mixc_gt (arg: value) return value;
    function mixc_le (arg: value) return value;
    function mixc_ge (arg: value) return value;
    function mixc_if(arg : value) return value;
    function mixc_and(arg : value) return value;
    function mixc_or(arg : value) return value;
    function mixc_not(arg : value) return value;
    function mixc_id(arg : value) return value;
    function of_string(s: string) return value; 
    function to_string (a: std_logic_vector) return string;
    function integer_of_value(arg: value) return integer; 
    function mixc_compute_address(caml_heap_base:value;a:value) return value;
end package;

package body runtime is
  function mixc_add (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := signed(arg(0 to length-1)) + signed(arg(length to arg'length - 1)); -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
      return value(r);
    end;

  function mixc_sub (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := signed(arg(0 to length-1)) - signed(arg(length to arg'length - 1)); -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
      return value(r);
    end;

  function mixc_mult (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) * signed(arg(length to arg'length - 1)),length); -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
      return value(r);
    end;

  function mixc_div (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) / signed(arg(length to arg'length - 1)),length); -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
      return value(r);
    end;

  function mixc_mod (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) mod signed(arg(length to arg'length - 1)),length); -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
      return value(r);
    end;


  function mixc_eq (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) = signed(arg(length to arg'length - 1)) then -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

    function mixc_neq (arg: value) return value is
      constant length: natural := arg'length / 2;
      variable r : value (0 to 0);
      begin
        if signed(arg(0 to length-1)) = signed(arg(length to arg'length - 1)) then -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
          r := "0";
        else
          r:= "1";
        end if;
        return r;
      end;

  function mixc_lt (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) < signed(arg(length to arg'length - 1)) then -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function mixc_gt (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) > signed(arg(length to arg'length - 1)) then -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function mixc_le (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) <= signed(arg(length to arg'length - 1)) then -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function mixc_ge (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) >= signed(arg(length to arg'length - 1)) then -- GHDL error if I use downto here. Unwanted behavior if I swap the two arguments.
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function mixc_if(arg : value) return value is
    constant length: natural := (arg'length - 1) / 2;
    variable r : value (0 to length-1);
    begin 
      if arg(0) = '1' then
         r := arg(1 to length);
      else 
         r := arg(length + 1 to length * 2);
      end if;
      return r;
    end;
  function mixc_and(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if (arg(0) = '1' and arg(1) = '1') then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;
  function mixc_or(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if (arg(0) = '1' or arg(1) = '1') then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;
  function mixc_not(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if arg(0) = '1' then
        r := "0";
      else
        r := "1";
      end if;
      return r;
    end;
  function mixc_id(arg : value) return value is
    begin 
      return arg;
    end;

  function of_string(s: string) return value is 
        constant ss: string(1 to s'length) := s; 
        variable answer: std_logic_vector(0 to 8 * s'length - 1); 
        variable p: integer; 
        variable c: integer; 
    begin 
        for i in ss'range loop
            p := 8 * i;
            c := character'pos(ss(i));
            answer(p - 8 to p-1) := std_logic_vector(to_unsigned(c,8)); 
        end loop; 
        return answer;
    end function; 

  function to_string (a: std_logic_vector) return string is
    variable b : string (1 to a'length) := (others => NUL);
    variable stri : integer := 1; 
    begin
        for i in a'range loop
            b(stri) := std_logic'image(a((i)))(2);
        stri := stri+1;
        end loop;
    return b;
    end function;

    function integer_of_value(arg: value) return integer is
      variable r : unsigned (0 to arg'length - 1);
    begin 
       r := unsigned(arg);
       return to_integer(r);
    end function;

    function mixc_compute_address(caml_heap_base:value;a:value) return value is
      begin
        return value(signed(caml_heap_base) + signed(a(0 to 31)) + (signed(a(32 to 63)) * 4));
      end function;

end runtime;
