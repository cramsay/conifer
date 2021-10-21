from textwrap import dedent
import fileinput
from IPython import get_ipython
import coeffs

_ipython = get_ipython()

def _run_cmd(cmd):
    return _ipython.run_line_magic('sx',cmd)


class Filter():
    """ An interface for our Filter implementations.
    
        Instances must provide an `impl` method to implement the
        filter and report util and timing results. Set `fir_name`
        to something uniuqe too please.
    """
    
    def __init__(self, parallelism, width, coeffs, fclk, output_dir):
        """ `parallelism` is a power-of-2 integer
            `width` is bitwidth of the input signal (int)
            `coeffs` is an instance of a `Coeffs` subclass
            `fclk` is the target clock frequency (float; MHz)
            `output_dir` is the directory for outputing implementation results
        """
        self.p = parallelism
        self.width = width
        self.coeffs = coeffs
        self.fclk = fclk
        self.output_dir = output_dir
    
    @property
    def fir_name(self):
        """ Name of the FIR structure """
        pass
    
    @property
    def name(self):
        """ Full name for the filter under test """
        return self.fir_name + \
               '_P' + str(self.p) + \
               '_IB' + str(self.width) + \
               '_FCLK' + str(self.fclk) + \
               '_' + self.coeffs.name
    
    @property
    def synth_dir(self):
        """ Output subdirectory for synthesis and implementation results """
        return self.output_dir + '/' + self.name
    
    @property
    def subfilt_taps(self):
        """ Number of taps per subfilter """
        return int(self.coeffs.taps / self.p)
    
    def impl(self, run='out_of_context'):
        """ Run implementation for the filter design """
        pass
    
    def parse_fclk_actual(self, run='out_of_context'):
        """ Parse the achieved clock frequency (in MHz) from the output files """
        met_timing = _run_cmd(f'grep -c "All user specified timing constraints are met" {self.synth_dir}/{run}/post_route_timing.rpt')[0] == '1'
        wns_timing = _run_cmd(f'grep -B 3 " met." {self.synth_dir}/{run}/post_route_timing.rpt | head -n 1 | tr -s \' \' | cut -d\' \' -f2')
        clkT      = _run_cmd(f'grep -A 50 "Waveform(ns)" {self.synth_dir}/{run}/post_route_timing.rpt | grep "clk" | head -n 1 | tr -s \' \' | cut -d\' \' -f4')
    
        print(wns_timing)
        print(clkT)
        fgot = 1000/(float(clkT[0]) - float(wns_timing[0]))
        return (met_timing, fgot)

    def parse_results(self, run='out_of_context'):
        """ Parse and collate all the implementation results """
        lut_result= _run_cmd(f'grep "CLB LUTs" {self.synth_dir}/{run}/post_route_util.rpt | cut -d"|" -f3')
        reg_result= _run_cmd(f'grep "CLB Registers" {self.synth_dir}/{run}/post_route_util.rpt | head -n 1 | cut -d"|" -f3')
        clb_result= _run_cmd(f'grep "| CLB  " {self.synth_dir}/{run}/post_route_util.rpt | head -n 1 | cut -d"|" -f3')
        dsp_result= _run_cmd(f'grep "| DSPs  " {self.synth_dir}/{run}/post_route_util.rpt | head -n 1 | cut -d"|" -f3')
        
        (met_timing, fclk_actual) = self.parse_fclk_actual(run=run)

        try:
            return {
                'group': self.output_dir,
                'name': self.name,
                'structure': self.fir_name,
                'coeff_class': self.coeffs.__class__.__name__,
                'in_width': self.width,
                'c_width': self.coeffs.width,
                'parallelism': self.p,
                'taps': self.coeffs.taps,
                'met_timing': met_timing,
                'fclk_actual': fclk_actual,
                'luts' : int(lut_result.s),
                'regs' : int(reg_result.s),
                'clbs' : int(clb_result.s),
                'dsps' : int(dsp_result.s)
            }
        except:
            print(f"Couldn't parse results for {self.name}. Did it fail synthesis?")
            raise
            

class ClashFilter(Filter):
    """ Interface for Clash-generated filters.
        
        Instances must provide `_fir_src` and `fir_name` properties.
    """
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    @property
    def _fir_src():
        pass
    
    @property
    def out_p(self):
        if isinstance(self.coeffs, coeffs.CoeffsHalfBand):
            return self.p//2
        else:
            return self.p
    
    @property
    def pre_f(self):
        if isinstance(self.coeffs, coeffs.CoeffsHalfBand):
            return 'fmap (head . transpose . unconcat d2) $'
        else:
            return ''
        
    @property
    def src(self):
        return dedent("""
               import Clash.Prelude
               import Filter.FFA
               import Filter.Serial
               import Filter.Polyphase
               import Graph.MCM
               
               createDomain vSystem{vName="SystemNR", vResetPolarity=ActiveLow}
               
               """) + dedent(self._fir_src) + dedent("""
               {-# ANN topEntity
                 (Synthesize
                   { t_name   = "fir_dut"
                   , t_inputs = [ PortName "clk"
                                , PortName "rst"
                                , PortName "x" ]
                   , t_output = PortName "y"
                   }) #-}
               topEntity c r x = exposeClockResetEnable (fir @SystemNR x) c r (toEnable $ pure True)
               """)
    
    def impl(self, run='out_of_context'):
        _run_cmd(f'mkdir -p {self.synth_dir}')
        _run_cmd(f'cp -r ../synth/* {self.synth_dir}/')
        with open(f'{self.synth_dir}/Filter.hs', 'w') as f:
            f.write(self.src)
            
        with open(f'{self.synth_dir}/{run}/synth.xdc', 'w') as f:
            if run == 'loopback':
                make_target = 'impl_loop'
                f.write(f'create_clock -period {1000/self.fclk} -name clk [get_ports clk]')
            elif run == 'out_of_context':
                make_target = 'impl_ooc'
                f.write(f"""
create_clock -period {1000/self.fclk} -name clk [get_ports clk]
set_property HD.CLK_SRC BUFGCTRL_X0Y0 [get_ports clk]
set_clock_uncertainty 0.250 -setup [get_clocks]
""")
            else:
                raise ValueError('`run` argument not recognised')

        _run_cmd(f'nix-shell --command \'make -C {self.synth_dir} {make_target}\' ../shell.nix &> {self.synth_dir}/build.log')
        return super().parse_results(run=run)
    

class PolyDirectFilter(ClashFilter):
    """ Our Polyphase filter with direct-form subfilters
    """
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    @property
    def fir_name(self):
        return 'PolyDirect'
    
    @property
    def _fir_src(self):
        return f"""
               fir :: HiddenClockResetEnable dom
                   => Signal dom (Vec {self.p}     (Signed {self.width}))
                   -> Signal dom (Vec {self.out_p} (Signed ({self.width} + {self.coeffs.width} + CLog 2 {self.subfilt_taps} + CLog 2 {self.p})))
               fir xs = {self.pre_f} polyphase (SNat :: SNat {self.p}) firDirect (map resize $ {self.coeffs.coeffs_vec}) (fmap (map resize) xs)
               """

    
class PolyMcmFilter(ClashFilter):
    """ Our Polyphase filter with MCM-based subfilters
    """
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    @property
    def fir_name(self):
        return 'PolyMcm'
    
    @property
    def _fir_src(self):
        return f"""
               fir :: HiddenClockResetEnable dom
                   => Signal dom (Vec {self.p}     (Signed {self.width}))
                   -> Signal dom (Vec {self.out_p} (Signed ({self.width} + {self.coeffs.width} + CLog 2 {self.subfilt_taps} + CLog 2 {self.p})))
               fir xs = {self.pre_f} polyphase_MCM
                          (SNat :: SNat {self.p})
                          $(mcmPipelinedDepthHwTH HcubShallow 3 (toList $ map fromIntegral {self.coeffs.coeffs_vec}))
                          (fmap (map resize) xs)
               """

class FfaDirectFilter(ClashFilter):
    """ Our FFA filter with direct-form subfilters
    """
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    @property
    def fir_name(self):
        return 'FfaDirect'
    
    @property
    def _fir_src(self):
        return f"""
               fir :: HiddenClockResetEnable dom
                   => Signal dom (Vec {self.p}     (Signed {self.width}))
                   -> Signal dom (Vec {self.out_p} (Signed ({self.width} + {self.coeffs.width} + CLog 2 {self.subfilt_taps}+ 4*(CLog 2 {self.p}))))
               fir xs = {self.pre_f} $(genFFA (SNat :: SNat {self.p}))
                            firDirect
                            (map resize {self.coeffs.coeffs_vec})
                            (fmap (map resize) xs)
               """
    
    
class FfaMcmFilter(ClashFilter):
    """ Our FFA filter with MCM-based subfilters
    """
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    @property
    def fir_name(self):
        return 'FfaMcm'
    
    @property
    def _fir_src(self):
        return f"""
               fir :: HiddenClockResetEnable dom
                   => Signal dom (Vec {self.p}     (Signed {self.width}))
                   -> Signal dom (Vec {self.out_p} (Signed ({self.width} + {self.coeffs.width} + CLog 2 {self.subfilt_taps}+ 4*(CLog 2 {self.p}))))
               fir xs = {self.pre_f} $(genFFA_MCM (mcmPipelinedDepthHwTH HcubShallow 3)
                                     (SNat :: SNat {self.p})
                                     (map fromIntegral {self.coeffs.coeffs_vec})
                        ) (fmap (map resize) xs)
               """


class SsrFilter(Filter):
    """ SSR/FirCompiler generated filters.
    """
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    @property
    def fir_name(self):
        return 'Ssr'
    
    def impl(self, run='out_of_context'):
        _run_cmd(f'mkdir -p {self.synth_dir}')
        _run_cmd(f'cp -r ./logicore_synth/* {self.synth_dir}/')

        # Write coeffs file
        with open(f'{self.synth_dir}/weights.coe', 'w') as f:
                f.write(self.coeffs.coeffs_coe)
        
        # Write timing constraints
        with open(f'{self.synth_dir}/{run}/synth.xdc', 'w') as f:
            if run == 'loopback':
                make_target = 'impl_loop'
                f.write(f'create_clock -period {1000/self.fclk} -name aclk [get_ports aclk]\n')
            elif run == 'out_of_context':
                make_target = 'impl_ooc'
                f.write(f'create_clock -period {1000/self.fclk} -name aclk [get_ports aclk]\n')
                f.write(f'set_property HD.CLK_SRC BUFGCTRL_X0Y0 [get_ports aclk]')
                f.write(f'set_clock_uncertainty 0.250 -setup [get_clocks]')
            else:
                raise ValueError('`run` argument not recognised')


        # Set parallelism in build script
        filter_type = 'Decimation' if isinstance(self.coeffs, coeffs.CoeffsHalfBand) else 'Single_Rate'
        with fileinput.FileInput(f'{self.synth_dir}/{run}/synth.tcl', inplace=True) as f:
            for line in f:
                print(line.replace('CONFIG.Sample_Frequency {4000}'  , f'CONFIG.Sample_Frequency {{{self.fclk*self.p}}}'
                         ).replace('CONFIG.Clock_Frequency {500}'    , f'CONFIG.Clock_Frequency {{{self.fclk}}}'
                         ).replace('CONFIG.Coefficient_Width {16}'   , f'CONFIG.Coefficient_Width {{{self.coeffs.width}}}'
                         ).replace('CONFIG.Data_Width {16}'          , f'CONFIG.Data_Width {{{self.width}}}'
                         ).replace('CONFIG.Filter_Type {Single_Rate}', f'CONFIG.Filter_Type {{{filter_type}}}'
                         ), end='')
        
        # Go!
        _run_cmd(f'nix-shell --command \'make -C {self.synth_dir} {make_target} \' ../shell.nix &> {self.synth_dir}/build.log')
    
        return super().parse_results(run=run)