
all: predictive-user-manual auto-overlay-manual #predictive-programmer-manual


DIR=~/programming/predictive/docs
WEB=~/www.dr-qubit.org/htdocs/predictive
USERDIR=predictive-user-manual
PROGDIR=predictive-programmer-manual
AUTOODIR=auto-overlay-manual


VPATH=$(USERDIR):$(PROGDIR):$(AUTOODIR)
GPATH=$(USERDIR):$(PROGDIR):$(AUTOODIR)


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



predictive-user-manual: predictive-user-manual.info.gz \
                        $(USERDIR)/html/index.html \
	                predictive-user-manual-html.tar.gz \
                        predictive-user-manual.txt.gz \
                        predictive-user-manual.ps.gz \
                        predictive-user-manual.pdf



predictive-programmer-manual: predictive-prog-manual.info.gz \
                              $(PROGDIR)/html/index.html \
	                      predictive-programmer-manual-html.tar.gz \
                              predictive-programmer-manual.txt.gz \
                              predictive-programmer-manual.ps.gz \
                              predictive-programmer-manual.pdf



auto-overlay-manual: auto-overlay-manual.info.gz \
                     $(AUTOODIR)/html/index.html \
	             auto-overlay-manual-html.tar.gz \
                     auto-overlay-manual.txt.gz \
                     auto-overlay-manual.ps.gz \
                     auto-overlay-manual.pdf \
                     auto-overlay-manual-src.tar.gz

auto-overlay-manual-src.tar.gz: auto-overlay-manual.texinfo auto-overlays.texinfo
	tar -cvzf $@ $(dir $<)/auto-overlays.texinfo \
	  $(dir $<)/auto-overlay-manual.texinfo

# have to override some implicit rules to add auto-overlays.texinfo dependency

auto-overlay-manual.info: auto-overlay-manual.texinfo auto-overlays.texinfo
	makeinfo $< -o $@

auto-overlay-manual.txt: auto-overlay-manual.texinfo auto-overlays.texinfo
	makeinfo --plaintext $< > $@

auto-overlay-manual.txt.gz: auto-overlay-manual.texinfo auto-overlays.texinfo
	makeinfo --plaintext $< | gzip -c > $@

auto-overlay-manual.dvi: auto-overlay-manual.texinfo auto-overlays.texinfo
	texi2dvi -c -o $@ $<

auto-overlay-manual.pdf: auto-overlay-manual.texinfo auto-overlays.texinfo
	texi2dvi --pdf -c -o $@ $<

auto-overlay-manual/html/index.html: auto-overlay-manual.texinfo \
                                     auto-overlays.texinfo
	echo foo $@ $<
	makeinfo --html $< -o $(dir $@)

auto-overlay-manual-html.tar.gz: auto-overlay-manual.texinfo \
                                 auto-overlays.texinfo \
                                 auto-overlay-manual/html/index.html
	cd $(dir $<); pwd; tar -cvzf $(notdir $@) html




# implicit rules for making various formats

%.info: %.texinfo
	makeinfo $< -o $@

# Note: to work correctly, this either requires the directory to be included
#       in the target or the target to already exist in the correct directory
#       so that directory search can find it
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

# Note: to work correctly, this either requires the directory to be included
#       in the target or the target to already exist in the correct directory
#       so that directory search can find it
%.ps.gz: %.dvi
	dvips -f $< | gzip -c > $@

%/html/index.html: %.texinfo
	makeinfo --html $< -o $(dir $@)

%-html.tar.gz: %.texinfo html/index.html
	echo bar $@ $<
	cd $(dir $<); pwd; tar -cvzf $(notdir $@) html
