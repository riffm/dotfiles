;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Wed, 16 Oct 2013 20:27:09 +0400")

(setq gnus-newsrc-alist '(("gmane.os.freebsd.advocacy" 3 ((1 . 5224)) ((seen (5020 . 5115) (5117 . 5120) (5123 . 5124) (5126 . 5150) (5152 . 5168) (5171 . 5178) (5183 . 5185) (5187 . 5224)))) ("nnmaildir+stcnet:Sent Messages" 3 ((1 . 2)) nil "nnmaildir:stcnet") ("nnmaildir+stcnet:stm" 3 nil nil "nnmaildir:stcnet") ("nnfolder+archive:sent.2013-10" 3 ((1 . 12)) ((seen (1 . 12))) (nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))) ("nnmaildir+stcnet:dpc" 3 ((1 . 30)) ((reply (3 . 5) (10 . 11) 15 19 26 30)) "nnmaildir:stcnet") ("nnmaildir+stcnet:INBOX" 3 nil nil "nnmaildir:stcnet") ("nnmaildir+stcnet:INBOX.Spam" 3 nil nil "nnmaildir:stcnet") ("nndraft:drafts" 1 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode)))) ("gmane.announce" 3 ((1 . 21197)) ((seen (21123 . 21181))))))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))

(setq gnus-killed-list '("gmane.os.freebsd.announce" "nnmaildir+stcnet:Trash" "gmane.annonces" "gmane.comp"))

(setq gnus-zombie-list '("gmane.linux.debian.alioth.vdr.scm" "gwene.com.blogspot.products.placed" "gwene.com.wordpress.itu4u" "gwene.org.codefortulsa" "gwene.com.youtube.the.young.turks" "gwene.be.ac.ulb.blog-recteur" "gwene.com.blog.mattforney" "gwene.com.feedburner.militaryaerosp.rz.eo" "gwene.org.tulsawebdevs" "gwene.com.spacenews.articles" "gwene.com.youtube.users.jack.shepard19rus" "gwene.com.blogspot.aaronsleazy" "gmane.science.geophysics.msnoise" "gmane.recreation.radio.scanning.centexcomm" "gwene.com.vebc" "gmane.comp.sysutils.elftoolchain.scm" "gwene.com.kiwiblitz" "gwene.org.bienscommuns.blog" "gwene.com.blogspot.buddys.party" "gwene.com.time.lightbox" "gwene.com.blog.staresatworld" "gwene.com.blogspot.bro-man-ce" "gmane.comp.bitcoin.btcd.scm" "gmane.comp.gis.openstreetmap.region.sk" "gwene.org.type-exit.adventures-wit" "gmane.comp.gcc.jit" "gwene.com.blogspot.awful.interiors" "gmane.org.user-groups.nuug.kart" "gwene.com.blogspot.g-n-t" "gmane.linux.stgt" "gwene.com.blog.illusionsanity" "gmane.science.simulation.sumo.user" "gmane.org.wikimedia.wikimania.committee" "gwene.net.nickwinter.blog" "gmane.linux.debian.devel.wine" "gwene.com.tumblr.oldsoldiers" "gwene.com.branchable.git-annex.comments" "gwene.com.cheerupemokid" "gmane.comp.python.django.django-cms.devel" "gmane.comp.java.clojure.enlive" "gwene.org.btf-thyroid.format.feed.type"))

(setq gnus-topic-topology '(("Gnus" visible) (("misc" visible nil nil)) (("FreeBSD" visible nil nil)) (("Mail" visible nil nil) (("STM" visible nil nil)))))

(setq gnus-topic-alist '(("FreeBSD" "gmane.os.freebsd.advocacy") ("STM" "nnmaildir+stcnet:dpc" "nnmaildir+stcnet:stm") ("Mail" "nnfolder+archive:sent.2013-10" "nnmaildir+stcnet:INBOX" "nnmaildir+stcnet:INBOX.Spam" "nndraft:drafts") ("misc" "gmane.announce") ("Gnus" "nnmaildir+stcnet:Sent Messages")))
