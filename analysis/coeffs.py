""" Module defining different coefficient set styles.
    
    Includes minimum phase, Type-I, Type-2/4, and Half-bands.
"""

import numpy as np
import enum
from scipy.signal import firwin, firwin2, minimum_phase

"""Type of filter response (low, high, or band-pass)"""
class ResponseType(enum.Enum):
   LP = 1
   BP = 2
   HP = 3


def _unpack_resp_type(resp):
    """Unpack a response type into amplitude list and antisymmetry flag for Type-2 vs Type-4 """
    if resp == ResponseType.LP:
        amps = [1,1,0,0]
        asym = False
    elif resp == ResponseType.HP:
        amps = [0,0,1,1]
        asym = True
    elif resp == ResponseType.BP:
        amps = [0,0,1,1,0,0]
        asym = True
    return (amps,asym)


class Coeffs():
    """An interface for defining filter coefficients.
    
       Subclasses must define a `coeffs` property to generate coefficients from
       any parameters and extend the name property with something unique.
    """
    
    def __init__(self, taps, width):
        self.taps = taps
        self.width = width
    
    @property
    def coeffs(self):
        pass
    
    @property
    def coeffs_no_pad(self):
        return self.coeffs
    
    @property
    def coeffs_vec(self):
        return '(' + ':>'.join([f'({c})' for c in self.coeffs]) + \
               f':> Nil :: Vec {len(self.coeffs)} (Signed {self.width}) )'
    
    @property
    def coeffs_coe(self):
        # Important to use coeffs_no_pad here because Fir Compiler doesn't check
        # for zeros while identifying symmetries.
        return 'RADIX = 10;\n' + \
               'COEFDATA = ' + ','.join([str(c) for c in self.coeffs_no_pad]) + ';'
    
    @property
    def name(self):
        return 'WS' + '_N' + str(self.taps) + '_CB' + str(self.width)

    
class CoeffsMinPhase(Coeffs):
    """ Coeffs for minimum phase filters """
    
    def __init__(self, resp, cutoffs, *args, **kwargs):
        """ `resp` is a ResponseType enum
            `cutoffs` is a list of cutoff frequencies, normalised between 0 and 1
        """
        self.resp = resp
        self.cutoffs = cutoffs
        super().__init__(*args, **kwargs)
    
    @property
    def name(self):
        return super().name + '_minphase_' + self.resp.name \
                            + '_F' + '-'.join(str(f) for f in self.cutoffs)
    
    @property
    def coeffs(self):
        (amps, asym) = _unpack_resp_type(self.resp)    
        ws = minimum_phase(firwin2(self.taps*2, self.cutoffs, amps))
        return np.round(ws * (2**(self.width-1)-1)).astype(int)

    
class CoeffsType1Pad(Coeffs):
    """ Coeffs for Type-I filters (padded with one zero) """
    
    def __init__(self, resp, cutoffs, *args, **kwargs):
        """ `resp` is a ResponseType enum
            `cutoffs` is a list of cutoff frequencies, normalised between 0 and 1
        """
        self.resp = resp
        self.cutoffs = cutoffs
        super().__init__(*args, **kwargs)
    
    @property
    def name(self):
        return super().name + '_type1p_' + self.resp.name \
                            + '_F' + '-'.join(str(f) for f in self.cutoffs)
    
    @property
    def coeffs(self):
        (amps, _) = _unpack_resp_type(self.resp)    
        ws = firwin2(self.taps-1, self.cutoffs, amps)
        ws = np.append(ws, [0])
        return np.round(ws * (2**(self.width-1)-1)).astype(int)
    
    @property
    def coeffs_no_pad(self):
        return self.coeffs[:-1]
    
    
class CoeffsType2(Coeffs):
    """ Coeffs for Type-2 filters (or Type-4, depending on `resp`) """
    
    def __init__(self, resp, cutoffs, *args, **kwargs):
        """ `resp` is a ResponseType enum
            `cutoffs` is a list of cutoff frequencies, normalised between 0 and 1
        """
        self.resp = resp
        self.cutoffs = cutoffs
        super().__init__(*args, **kwargs)
    
    @property
    def name(self):
        return super().name + '_type2_' + self.resp.name \
                            + '_F' + '-'.join(str(f) for f in self.cutoffs)
    
    @property
    def coeffs(self):
        (amps, asym) = _unpack_resp_type(self.resp)
        ws = firwin2(self.taps, self.cutoffs, amps, antisymmetric=asym)
        return np.round(ws * (2**(self.width-1)-1)).astype(int)

    
class CoeffsHalfBand(Coeffs):
    """ Coeffs for Half-band filters (with zero padding) """
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    
    @property
    def name(self):
        return super().name + '_hb'
    
    @property
    def coeffs(self):
        ws = firwin(self.taps+1, 0.5)[1:]
        return np.round(ws * (2**(self.width-1)-1)).astype(int)
    
    @property
    def coeffs_no_pad(self):
        return self.coeffs[:-1]