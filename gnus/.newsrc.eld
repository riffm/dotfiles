;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Tue, 15 Oct 2013 20:33:55 +0400")

(setq gnus-newsrc-alist '(("gmane.os.freebsd.advocacy" 3 ((1 . 5224)) ((seen (5020 . 5115) (5117 . 5120) (5123 . 5124) (5126 . 5150) (5152 . 5168) (5171 . 5178) (5183 . 5185) (5187 . 5224)))) ("nnmaildir+stcnet:Sent Messages" 3 ((1 . 2)) ((seen 2)) "nnmaildir:stcnet") ("nnmaildir+stcnet:stm" 3 nil nil "nnmaildir:stcnet") ("nnfolder+archive:sent.2013-10" 3 ((1 . 7)) ((seen (1 . 7))) (nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))) ("nnmaildir+stcnet:dpc" 3 ((1 . 36)) ((reply 2 (9 . 10) 16 23 27 35)) "nnmaildir:stcnet") ("nnmaildir+stcnet:INBOX" 3 nil nil "nnmaildir:stcnet") ("nnmaildir+stcnet:INBOX.Spam" 3 nil nil "nnmaildir:stcnet") ("nndraft:drafts" 1 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode)))) ("gmane.announce" 3 ((1 . 21192)) ((seen (21123 . 21189) 21192)))))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))

(setq gnus-killed-list '("gmane.os.freebsd.announce" "nnmaildir+stcnet:Trash" "gmane.annonces" "gmane.comp"))

(setq gnus-zombie-list '("gwene.be.ac.ulb.blog-recteur" "gmane.comp.gis.openstreetmap.region.sk" "gmane.comp.bitcoin.btcd.scm" "gwene.org.type-exit.adventures-wit" "gmane.org.user-groups.nuug.kart" "gwene.com.blogspot.products.placed" "gwene.com.wordpress.itu4u" "gwene.org.codefortulsa" "gwene.org.tulsawebdevs" "gwene.com.youtube.users.jack.shepard19rus" "gmane.recreation.radio.scanning.centexcomm" "gmane.comp.sysutils.elftoolchain.scm" "gwene.com.blogspot.buddys.party" "gwene.com.blogspot.bro-man-ce" "gwene.com.blogspot.awful.interiors" "gwene.com.blogspot.g-n-t" "gmane.science.simulation.sumo.user" "gmane.linux.debian.alioth.vdr.scm" "gwene.com.feedburner.militaryaerosp.rz.eo" "gwene.com.spacenews.articles" "gwene.com.vebc" "gwene.com.kiwiblitz" "gmane.org.wikimedia.wikimania.committee" "gwene.net.nickwinter.blog" "gmane.linux.debian.devel.wine" "gwene.com.tumblr.oldsoldiers" "gwene.com.branchable.git-annex.comments" "gwene.com.cheerupemokid" "gmane.comp.python.django.django-cms.devel" "gmane.comp.java.clojure.enlive" "gwene.org.btf-thyroid.format.feed.type"))

(setq gnus-topic-topology '(("Gnus" visible) (("misc" visible nil nil)) (("FreeBSD" visible nil nil)) (("Mail" visible nil nil) (("STM" visible nil nil)))))

(setq gnus-topic-alist '(("FreeBSD" "gmane.os.freebsd.advocacy") ("STM" "nnmaildir+stcnet:dpc" "nnmaildir+stcnet:stm") ("Mail" "nnfolder+archive:sent.2013-10" "nnmaildir+stcnet:INBOX" "nnmaildir+stcnet:INBOX.Spam" "nndraft:drafts" "nnmaildir+stcnet:Sent Messages") ("misc" "gmane.announce") ("Gnus")))
