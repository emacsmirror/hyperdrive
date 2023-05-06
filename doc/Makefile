EMACS := emacs
EMACSQ := $(EMACS) -Q
BATCH := $(EMACSQ) --batch \
	  --eval '(setq vc-handled-backends nil org-startup-folded nil org-element-cache-persistent nil)' \
      --eval '(setq gc-cons-threshold (* 50 1000 1000))'
TEXI2HTML := makeinfo --html --number-sections --css-ref "https://www.gnu.org/software/emacs/manual.css"
TEXI2PDF := texi2pdf
TEXI2INFO := makeinfo

%.html: %.texi
	$(TEXI2HTML) --no-split -o $@ $<

%.pdf: %.texi
	$(TEXI2PDF) -o $@ $<

%.info: %.texi
	$(TEXI2INFO) -o $@ $<

hyperdrive.texi: hyperdrive-manual.org
	$(BATCH) --find-file $< --eval "(require 'ox-texinfo)" \
                            --eval '(org-texinfo-export-to-texinfo)'
