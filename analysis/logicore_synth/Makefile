IMPL = post_route.dcp

all: $(IMPL)

$(IMPL):
	vivado -mode batch -source synth.tcl

clean:
	rm -r hbs hd_visual ip ip_user_files vivado.jou vivado.log *.rpt *.dcp

shrink: # Remove intermediate files, keeping final results
	rm -r hbs hd_visual ip ip_user_files vivado.jou vivado.log

impl: $(IMPL)

