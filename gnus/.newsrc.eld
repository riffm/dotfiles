;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Fri, 11 Oct 2013 23:31:10 +0400")

(setq gnus-newsrc-alist '(("gmane.os.freebsd.advocacy" 3 ((1 . 5219)) ((seen (5020 . 5115) (5117 . 5120) (5123 . 5124) (5126 . 5150) (5152 . 5168) (5171 . 5178) (5183 . 5185) (5187 . 5219)))) ("nnmaildir+stcnet:stm" 3 nil nil "nnmaildir:stcnet") ("nnfolder+archive:sent.2013-10" 3 ((1 . 2)) ((seen (1 . 6))) (nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))) ("nnmaildir+stcnet:dpc" 3 ((1 . 14)) ((reply 2 (9 . 10))) "nnmaildir:stcnet") ("nnmaildir+stcnet:INBOX" 3 ((1 . 9)) ((reply 3)) "nnmaildir:stcnet") ("nnmaildir+stcnet:INBOX.Spam" 3 nil nil "nnmaildir:stcnet") ("nndraft:drafts" 1 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode)))) ("gmane.announce" 3 ((1 . 21181)) ((seen (21123 . 21181))))))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))

(setq gnus-killed-list '("gmane.os.freebsd.announce" "nnmaildir+stcnet:Trash" "gmane.annonces" "gmane.comp"))

(setq gnus-zombie-list '("gmane.org.wikimedia.wikimania.committee" "gwene.net.nickwinter.blog" "gmane.linux.debian.devel.wine" "gwene.com.tumblr.oldsoldiers" "gwene.com.branchable.git-annex.comments" "gwene.com.cheerupemokid" "gmane.comp.python.django.django-cms.devel" "gmane.comp.java.clojure.enlive" "gwene.org.btf-thyroid.format.feed.type"))

(setq gnus-topic-topology '(("Gnus" visible) (("misc" visible nil nil)) (("FreeBSD" visible nil nil)) (("Mail" visible nil nil) (("STM" visible nil nil)))))

(setq gnus-topic-alist '(("FreeBSD" "gmane.os.freebsd.advocacy") ("STM" "nnmaildir+stcnet:dpc" "nnmaildir+stcnet:stm") ("Mail" "nnfolder+archive:sent.2013-10" "nnmaildir+stcnet:INBOX" "nnmaildir+stcnet:INBOX.Spam" "nndraft:drafts") ("misc" "gmane.announce") ("Gnus")))
