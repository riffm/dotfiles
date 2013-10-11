;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Fri, 11 Oct 2013 13:36:33 +0400")

(setq gnus-newsrc-alist '(("nnmaildir+stcnet:stm" 3 nil nil "nnmaildir:stcnet") ("nnfolder+archive:sent.2013-10" 3 ((1 . 6)) ((seen (1 . 6))) (nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))) ("nnmaildir+stcnet:dpc" 3 ((1 . 3)) ((reply 3)) "nnmaildir:stcnet") ("nnmaildir+stcnet:INBOX" 3 ((1 . 8)) ((reply 6)) "nnmaildir:stcnet") ("nnmaildir+stcnet:INBOX.Spam" 3 nil nil "nnmaildir:stcnet") ("nndraft:drafts" 1 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode)))) ("gmane.announce" 3 ((1 . 21178)) ((seen (21123 . 21178))))))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))

(setq gnus-killed-list '("nnmaildir+stcnet:Trash" "gmane.annonces" "gmane.comp"))

(setq gnus-zombie-list '("gwene.com.cheerupemokid" "gmane.comp.python.django.django-cms.devel" "gmane.comp.java.clojure.enlive" "gwene.org.btf-thyroid.format.feed.type"))

(setq gnus-topic-topology '(("Gnus" visible) (("misc" visible nil nil)) (("Mail" visible nil nil) (("STM" invisible nil nil)))))

(setq gnus-topic-alist '(("STM" "nnmaildir+stcnet:dpc" "nnmaildir+stcnet:stm") ("Mail" "nnfolder+archive:sent.2013-10" "nnmaildir+stcnet:INBOX" "nnmaildir+stcnet:INBOX.Spam" "nndraft:drafts") ("misc" "gmane.announce") ("Gnus")))
