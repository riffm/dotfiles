install: install-vim install-bash install-git install-hg install-tmux install-mutt install-emacs

install-vim:
	rm -rf ~/.vim ~/.vimrc ~/.ctags
	ln -s `pwd`/vim ~/.vim
	ln -s ~/.vim/vimrc ~/.vimrc
	ln -s ~/.vim/ctags ~/.ctags

install-bash:
	rm -f ~/.profile
	ln -s `pwd`/bash/bashrc ~/.profile

install-git:
	rm -f ~/.gitconfig
	ln -s `pwd`/git/config ~/.gitconfig

install-hg:
	# make sure you have Dulwich installed for hg-git
	rm -f ~/.hgrc
	ln -s `pwd`/hg/hgrc ~/.hgrc

install-tmux:
	rm -f ~/.tmux.conf
	ln -s `pwd`/tmux.conf ~/.tmux.conf

install-mutt:
	rm -rf ~/.mutt
	ln -s `pwd`/mutt ~/.mutt
	rm -f ~/.urlview
	ln -s `pwd`/urlview ~/.urlview
	rm -f ~/.notmuch-config
	ln -s `pwd`/notmuch-config ~/.notmuch-config
	rm -f ~/.msmtprc
	ln -s `pwd`/msmtprc ~/.msmtprc
	rm -f ~/.muttrc
	ln -s `pwd`/muttrc ~/.muttrc
	rm -f ~/.offlineimaprc
	ln -s `pwd`/offlineimaprc ~/.offlineimaprc

install-emacs:
	rm -rf ~/.emacs.d
	ln -s `pwd`/.emacs.d ~/.emacs.d

install-gnus: install-emacs
	rm -rf ~/.gnus
	ln -s `pwd`/gnus/.gnus ~/.gnus
	rm -rf ~/.newsrc
	ln -s `pwd`/gnus/.newsrc ~/.newsrc
	rm -rf ~/.newsrc.eld
	ln -s `pwd`/gnus/.newsrc.eld ~/.newsrc.eld
