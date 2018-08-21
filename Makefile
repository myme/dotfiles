all:

emacs:
	sudo apt install build-essential
	sudo apt build-dep emacs25
	(cd ~/apps/emacs && ./autogen.sh && ./configure && make)
	./install emacs

.PHONY: all emacs
