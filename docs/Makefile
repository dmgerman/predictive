
all: predictive-user-manual auto-overlay-manual #predictive-programmer-manual


WEB=~/www.dr-qubit.org/htdocs/predictive
USERDIR=predictive-user-manual
PROGDIR=predictive-programmer-manual
AUTOODIR=auto-overlay-manual


VPATH=$(USERDIR):$(PROGDIR):$(AUTOODIR)
#GPATH=$(USERDIR):$(PROGDIR):$(AUTOODIR)



clean:
	rm -f $(USERDIR)/predictive-user-manual.info.gz
	rm -f $(USERDIR)/predictive-user-manual.info
	rm -rf $(USERDIR)/html/
	rm -f $(USERDIR)/predictive-user-manual-html.tar.gz
	rm -f $(USERDIR)/predictive-user-manual.txt
	rm -f $(USERDIR)/predictive-user-manual.txt.gz
	rm -f $(USERDIR)/predictive-user-manual.dvi
	rm -f $(USERDIR)/predictive-user-manual.ps.gz
	rm -f $(USERDIR)/predictive-user-manual.pdf
	rm -f $(USERDIR)/dir
	rm -f $(AUTOODIR)/auto-overlay-manual.info.gz
	rm -f $(AUTOODIR)/auto-overlay-manual.info
	rm -rf $(AUTOODIR)/html/
	rm -f $(AUTOODIR)/auto-overlay-manual-html.tar.gz
	rm -f $(AUTOODIR)/auto-overlay-manual.txt
	rm -f $(AUTOODIR)/auto-overlay-manual.txt.gz
	rm -f $(AUTOODIR)/auto-overlay-manual.dvi
	rm -f $(AUTOODIR)/auto-overlay-manual.ps.gz
	rm -f $(AUTOODIR)/auto-overlay-manual.pdf
	rm -f $(AUTOODIR)/dir
	rm -f $(AUTOODIR)/auto-overlay-manual-src.tar.gz



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



predictive-user-manual: $(USERDIR)/predictive-user-manual.info.gz \
	                $(USERDIR)/predictive-user-manual.info \
	                $(USERDIR)/predictive-user-manual-html.tar.gz \
                        $(USERDIR)/predictive-user-manual.txt.gz \
                        $(USERDIR)/predictive-user-manual.ps.gz \
                        $(USERDIR)/predictive-user-manual.pdf \
                        $(USERDIR)/dir

$(USERDIR)/predictive-user-manual-html.tar.gz: $(USERDIR)/html/index.html
	cd $(dir $@); pwd; tar -cvzf $(notdir $@) html



# predictive-programmer-manual: predictive-prog-manual.info.gz \
#                               $(PROGDIR)/html/index.html \
# 	                      predictive-programmer-manual-html.tar.gz \
#                               predictive-programmer-manual.txt.gz \
#                               predictive-programmer-manual.ps.gz \
#                               predictive-programmer-manual.pdf



auto-overlay-manual: $(AUTOODIR)/auto-overlay-manual.info.gz \
	             $(AUTOODIR)/auto-overlay-manual.info \
	             $(AUTOODIR)/auto-overlay-manual-html.tar.gz \
                     $(AUTOODIR)/auto-overlay-manual.txt.gz \
                     $(AUTOODIR)/auto-overlay-manual.ps.gz \
                     $(AUTOODIR)/auto-overlay-manual.pdf \
                     $(AUTOODIR)/dir

$(AUTOODIR)/auto-overlay-manual-html.tar.gz: $(AUTOODIR)/html/index.html
	cd $(dir $@); pwd; tar -cvzf $(notdir $@) html




# implicit rules for making various formats

%.info: %.texinfo
	makeinfo $< -o $@
# keep .info files for ELPA packaging
.PRECIOUS: %.info

# Note: to work correctly, this either requires the directory to be included
#       in the target or the target to already exist in the correct directory
#       so that directory search can find it
%.info.gz: %.info
	gzip -f $< -c > $@

# Note: need to rm dir file otherwise install-info just adds new entry to it
%/dir: %.info.gz
	rm -f $@
	install-info $< $(dir $<)dir

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

# %-html.tar.gz: %/html/index.html
# 	cd $(dir $<); pwd; tar -cvzf $(notdir $@) html
