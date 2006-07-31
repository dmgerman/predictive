
all: core dicts

clean:
	rm *.elc latex/*.elc ams-latex/*.elc html/*.elc f90/*.elc

EMACS = emacs



# list of core elisp files
core_files := $(shell ls *.el | sed 's:\.el:\.elc:g')

# list of libraries to load
#elisp_libs = heap.el tstree.el dict.el predictive.el auto-overlays.el auto-overlay-word.el auto-overlay-line.el auto-overlay-self.el auto-overlay-stack.el

# lists of dictionaries
latex_dicts := $(shell ls latex/dict-*.word-list | sed 's:\.word-list:\.elc:g')
ams_latex_dicts := $(shell ls ams-latex/dict-*.word-list | sed 's:\.word-list:\.elc:g')
html_dicts := $(shell ls html/dict-*.word-list | sed 's:\.word-list:\.elc:g')
f90_dicts := $(shell ls f90/dict-*.word-list | sed 's:\.word-list:\.elc:g')




# byte-compilation target
core: $(core_files)


# dictionary targets
dicts: latex_dicts ams_latex_dicts html_dicts f90_dicts

latex_dicts: $(latex_dicts) 

ams_latex_dicts: $(ams_latex_dicts) 

html_dicts: $(html_dicts) 

f90_dicts: $(f90_dicts)




# implicit rule for creating dictionaries
dict-%.elc: dict-%.word-list
	$(EMACS) --batch -L ./ --eval="(progn (require 'predictive) (predictive-create-dict '$(basename $(notdir $@)) \"$(basename $@)\" \"$<\") (dict-save-modified))"

# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) --batch -L ./ -f batch-byte-compile $<



test:
	echo $(dicts)
