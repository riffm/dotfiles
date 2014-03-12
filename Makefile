install: install-vim install-bash install-git install-hg install-tmux install-mutt install-emacs

install-vim:
	rm -rf $(HOME)/.vim $(HOME)/.vimrc $(HOME)/.ctags
	ln -s $(PWD)/vim $(HOME)/.vim
	ln -s $(HOME)/.vim/vimrc $(HOME)/.vimrc
	ln -s $(HOME)/.vim/ctags $(HOME)/.ctags

install-bash:
	rm -f $(HOME)/.profile
	ln -s $(PWD)/bash/bashrc $(HOME)/.profile

install-zsh:
	rm -f $(HOME)/.zshrc
	ln -s $(PWD)/.zshrc $(HOME)/.zshrc

install-git:
	rm -f $(HOME)/.gitconfig
	ln -s $(PWD)/git/config $(HOME)/.gitconfig

install-hg:
	# make sure you have Dulwich installed for hg-git
	rm -f $(HOME)/.hgrc
	ln -s $(PWD)/hg/hgrc $(HOME)/.hgrc

install-tmux:
	rm -f $(HOME)/.tmux.conf
	ln -s $(PWD)/tmux.conf $(HOME)/.tmux.conf

install-mutt:
	rm -rf $(HOME)/.mutt
	ln -s $(PWD)/mutt $(HOME)/.mutt
	rm -f $(HOME)/.urlview
	ln -s $(PWD)/urlview $(HOME)/.urlview
	rm -f $(HOME)/.notmuch-config
	ln -s $(PWD)/notmuch-config $(HOME)/.notmuch-config
	rm -f $(HOME)/.msmtprc
	ln -s $(PWD)/msmtprc $(HOME)/.msmtprc
	rm -f $(HOME)/.muttrc
	ln -s $(PWD)/muttrc $(HOME)/.muttrc
	rm -f $(HOME)/.offlineimaprc
	ln -s $(PWD)/offlineimaprc $(HOME)/.offlineimaprc

install-emacs:
	rm -rf $(HOME)/.emacs.d
	ln -s $(PWD)/.emacs.d $(HOME)/.emacs.d

install-gnus: install-emacs
	rm -rf $(HOME)/.gnus
	ln -s $(PWD)/gnus/.gnus $(HOME)/.gnus
	rm -rf $(HOME)/.newsrc
	ln -s $(PWD)/gnus/.newsrc $(HOME)/.newsrc
	rm -rf $(HOME)/.newsrc.eld
	ln -s $(PWD)/gnus/.newsrc.eld $(HOME)/.newsrc.eld
