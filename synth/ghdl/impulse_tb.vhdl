library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.NUMERIC_STD.ALL;

entity impulse_tb is
end impulse_tb;

architecture Behavioral of impulse_tb is

  -- CHANGE THESE AS PER YOUR DESIGN
  constant P : integer := 4;
  constant WIDTH_X : integer := 16;
  constant WIDTH_Y : integer := 44;

  component fir_dut is
    port(clk : std_logic;
         rst : in std_logic;
         x   : in std_logic_vector(63 downto 0);
         y   : out std_logic_vector(175 downto 0));
  end component;
  -- END CHANGES

  constant CLKT : time := 6 ns;
  type t_input is array(0 to (P-1)) of integer;
  type t_output is array(0 to (P-1)) of signed((WIDTH_Y-1) downto 0);

  signal clk : std_logic := '0';
  signal rst : std_logic := '0';
  signal x : std_logic_vector((P*WIDTH_X)-1 downto 0) := (others => '0');
  signal y : std_logic_vector((P*WIDTH_Y)-1 downto 0);

  signal xs : t_input := (others => 0);
  signal ys : t_output;

begin

clk_process :process
   begin
        clk <= '0';
        wait for CLKT/2;
        clk <= '1';
        wait for CLKT/2;
   end process;

rst_process :process
   begin
        wait until rising_edge(clk);
        rst <= '0';
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        rst <= '1';
        wait;
   end process;

xs_process :process
   begin
        wait until rising_edge(clk);
        if rst = '1' then
            xs <= (0 => 1, others => 0);
            wait until rising_edge(clk);
            xs <= (others => 0);
            wait;
        end if;
   end process;

x_process :process(xs)
  begin
    for i in 0 to (P-1) loop
        x((WIDTH_X-1)+(i*WIDTH_X) downto (i*WIDTH_X)) <= std_logic_vector(to_signed(xs(P-1-i), WIDTH_X));
    end loop;
  end process;

y_process :process(y)
  begin
    for i in 0 to (P-1) loop
        ys(P-1-i) <= signed(y((WIDTH_Y-1)+(i*WIDTH_Y) downto (i*WIDTH_Y)));
    end loop;
  end process;

dut : fir_dut port map(
  clk => clk,
  rst => rst,
  x   => x,
  y   => y
);

end Behavioral;

