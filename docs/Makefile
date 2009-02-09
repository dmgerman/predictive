
all: user-manual auto-overlay-manual #programmer-manual


DIR=~/programming/predictive/docs
WEB=~/www.dr-qubit.org/htdocs/predictive
USERDIR=user-manual
PROGDIR=programmer-manual
AUTOODIR=auto-overlay-manual

test:
	echo $(DIR)

clean:
#	[[ -d $(DIR)/$(USERDIR) ]] && rm -r $(DIR)/$(USERDIR)
#	[[ -d $(DIR)/$(PROGDIR) ]] && rm -r $(DIR)/$(PROGDIR)
# 	if [[ -d $(DIR)/html ]]; then rm -r $(DIR)/html; fi
# 	if [[ -f $(DIR)/predictive.pdf ]]; then rm $(DIR)/predictive.pdf; fi
# 	if [[ -f $(DIR)/predictive.dvi ]]; then rm $(DIR)/predictive.dvi; fi
# 	if [[ -f $(DIR)/predictive.ps ]]; then rm $(DIR)/predictive.ps; fi
# 	if [[ -f $(DIR)/predictive.ps.gz ]]; then\
# 	  rm $(DIR)/predictive.ps.gz; fi
# 	if [[ -f $(DIR)/predictive.info ]]; then rm $(DIR)/predictive.info; fi
# 	if [[ -f $(DIR)/predictive.info.gz ]]; then\
# 	  rm $(DIR)/predictive.info.gz;	fi
# 	if [[ -f $(DIR)/predictive.txt ]]; then rm $(DIR)/predictive.txt; fi
# 	if [[ -f $(DIR)/predictive.txt.gz ]]; then\
# 	  rm $(DIR)/predictive.txt.gz; fi
# 	if [[ -f $(DIR)/predictive-html-docs.tar.gz ]]; then\
# 	  rm $(DIR)/predictive-html-docs.tar.gz; fi


install:
	cp -r $(USERDIR)/html $(WEB)/$(USERDIR)/
	cp $(USERDIR)/predictive-user-manual-html.tar.gz $(WEB)/$(USERDIR)/
	cp $(USERDIR)/predictive-user-manual.info.gz $(WEB)/$(USERDIR)/
	cp $(USERDIR)/predictive-user-manual.pdf $(WEB)/$(USERDIR)/
	cp $(USERDIR)/predictive-user-manual.ps.gz $(WEB)/$(USERDIR)/
	cp $(USERDIR)/predictive-user-manual.texinfo $(WEB)/$(USERDIR)/
	cp $(USERDIR)/predictive-user-manual.txt.gz $(WEB)/$(USERDIR)/
#	cp -r $(PROGDIR) $(WEB)
# 	cp -r html $(WEB)
	cp -r $(AUTOODIR)/html $(WEB)/$(AUTOODIR)/
	cp $(AUTOODIR)/auto-overlay-manual-html.tar.gz $(WEB)/$(AUTOODIR)/
	cp $(AUTOODIR)/auto-overlay-manual.info.gz $(WEB)/$(AUTOODIR)/
	cp $(AUTOODIR)/auto-overlay-manual.pdf $(WEB)/$(AUTOODIR)/
	cp $(AUTOODIR)/auto-overlay-manual.ps.gz $(WEB)/$(AUTOODIR)/
	cp $(AUTOODIR)/auto-overlay-manual-src.tar.gz $(WEB)/$(AUTOODIR)/
	cp $(AUTOODIR)/auto-overlay-manual.txt.gz $(WEB)/$(AUTOODIR)/


user-manual: $(USERDIR)/predictive-user-manual.info.gz \
            $(USERDIR)/predictive-user-manual-html \
	    $(USERDIR)/predictive-user-manual-html.tar.gz \
            $(USERDIR)/predictive-user-manual.txt.gz \
            $(USERDIR)/predictive-user-manual.ps.gz \
            $(USERDIR)/predictive-user-manual.pdf


programmer-manual: $(PROGDIR)/predictive-prog-manual.info.gz \
                  $(PROGDIR)/predictive-programmer-manual-html \
	          $(PROGDIR)/predictive-programmer-manual-html.tar.gz \
                  $(PROGDIR)/predictive-programmer-manual.txt.gz \
                  $(PROGDIR)/predictive-programmer-manual.ps.gz \
                  $(PROGDIR)/predictive-programmer-manual.pdf


auto-overlay-manual: $(AUTOODIR)/auto-overlay-manual.info.gz \
               $(AUTOODIR)/auto-overlay-manual-html \
	       $(AUTOODIR)/auto-overlay-manual-html.tar.gz \
               $(AUTOODIR)/auto-overlay-manual.txt.gz \
               $(AUTOODIR)/auto-overlay-manual.ps.gz \
               $(AUTOODIR)/auto-overlay-manual.pdf \
               auto-o-src

auto-o-src:
	tar -cvzf $(AUTOODIR)/auto-overlay-manual-src.tar.gz $(AUTOODIR)/auto-overlays.texinfo $(AUTOODIR)/auto-overlay-manual.texinfo




# implicit rules for making various formats

%.info: %.texinfo
	makeinfo $< -o $@

%.info.gz: %.info
	gzip -f $< -c > $@

%.txt: %.texinfo
	makeinfo --plaintext $< > $@

%.txt.gz: %.texinfo
	makeinfo --plaintext $< | gzip -c > $@

%.dvi: %.texinfo
	texi2dvi -c -o $@ $<

%.pdf: %.texinfo
	texi2dvi --pdf -c -o $@ $<

%.ps.gz: %.dvi
	dvips -f $< | gzip -c > $@

%-html: %.texinfo
	makeinfo --html $< -o $(dir $@)/html

%-html.tar.gz: %-html
	cd $(dir $@); pwd; tar -cvzf $(notdir $@) html
