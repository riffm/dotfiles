
(set-default-coding-systems 'utf-8)

(load-theme 'tango-dark)

(if window-system
    (tool-bar-mode -1))

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq auto-save-list-file-prefix
      (concat temporary-file-directory "save-"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq show-trailing-whitespace t)
(autoload 'whitespace-mode
  "whitespace"
  "Toggle whitespace visualization."
  t)
(autoload 'whitespace-toggle-options
  "whitespace"
  "Toggle local `whitespace-mode' options."
  t)
(setq whitespace-style '(face trailing tabs empty tab-mark
                              lines-tail))
(add-hook 'prog-mode-hook #'(lambda () (whitespace-mode t)))

(setq tramp-default-method "ssh")

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(eval-after-load "flymake"
  '(add-to-list 'flymake-allowed-file-name-masks
                '("\\.py\\'" flymake-pyflakes-init)))

(require 'semantic/ia)
(defun activate-semantic-mode ()
  (setq semantic-default-submodes
        '(global-semanticdb-minor-mode
          global-semantic-highlight-func-mode
          global-semantic-idle-local-symbol-highlight-mode
          global-semantic-idle-scheduler-mode
          global-semantic-idle-completions-mode))
  (semantic-mode 1)
  (set-semantic-keys))

(defun set-semantic-keys ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "." 'semantic-complete-self-insert))

(defun setup-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'java-mode-hook 'activate-semantic-mode)

(setq gnutls-min-prime-bits nil)

(setq gnutls-log-level 1)

(defun add-curl-crt-bundle-to-gnutls-trustfiles ()
  (let ((f (cond
            ((eq system-type 'darwin) "/opt/local/share/curl/curl-ca-bundle.crt")
            ((eq system-type 'berkeley-unix) "/usr/local/share/certs/ca-root-nss.crt"))))
    (when (file-exists-p f)
      (add-to-list 'gnutls-trustfiles f)
      (setq starttls-use-gnutls t
            starttls-gnutls-program "gnutls-cli"
            starttls-extra-arguments (list "--starttls"
                                           (format "--x509cafile=%s" f))))))

(let ((f "InconsolataLGC-13"))
  (if (and (display-graphic-p)
           (not (null (x-list-fonts f))))
      (set-default-font f)))

(package-initialize)

(defvar emacs-pkgs
  '(ahg
    sml-mode
    scala-mode
    haskell-mode
    company))

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 emacs-pkgs)

(if (display-graphic-p)
    (progn
      (load-theme 'solarized-dark t)
      (setq solarized-distinct-fringe-background t
            solarized-high-contrast-mode-line t)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)))

(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq nrepl-hide-special-buffers t)

(require 'ahg)

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(defun jabber ()
  (interactive)
  (require 'gnutls)
  (load "~/secret.el.gpg")
  (add-curl-crt-bundle-to-gnutls-trustfiles)
  (setq jabber-account-list
        `(("riffm@rnd.stcnet.ru/emacs"
           (:network-server . "rnd.stcnet.ru")
           (:password . ,riffm-at-rnd-stcnet-ru-passwd)
           (:connection-type . starttls)
           (:port . 5222))
          ("riffm@jabber.ru/emacs"
           (:network-server . "jabber.ru")
           (:password . ,riffm-at-jabber-ru-passwd)
           (:connection-type . ssl)
           (:port . 5223))))
  (jabber-connect-all)
  (makunbound 'jabber-account-list)
  (clear-secrets))

(setq coffee-tab-width 2)

(when (executable-find "opam")
  (dolist (var (car (read-from-string
                     (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (split-string (getenv "PATH") path-separator))
  (push (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../share/emacs/site-lisp")
        load-path)
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
  (add-hook 'tuareg-mode-hook
            #'(lambda ()
                (define-key tuareg-mode-map
                  (kbd "C-M-i") 'utop-edit-complete))))

(require 'bbdb)
(setq
 bbdb-file "~/Dropbox/bbdb"
 bbdb-mua-auto-update-p 'query)
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)

(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      user-full-name "Tim Perevezentsev")

(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "riffm2005@gmail.com" from) "riffm2005")
               ((string-match "riffm@stmdev.ru" from) "stmdev")
               ((string-match "me@riffm.name" from) "riffm-name")
               ((string-match "riffm@rnd.stcnet.ru" from) "riffm-stcnet"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)

(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(setq org-log-done 'time)
(define-key mode-specific-map [?a] 'org-agenda)
(custom-set-variables
 '(org-agenda-files (quote ("~/Dropbox/tasks.org"
                            "~/Dropbox/tasks.org_archive")))
 '(org-default-notes-file "~/Dropbox/notes.org")
 '(org-agenda-show-all-dates t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("w" todo "WAITING" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-skip-deadline-if-done t)
      '(org-agenda-skip-scheduled-if-done t))

(custom-set-variables
 '(org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@)" "DELEGATED(l@)"
               "|" "DONE(d!)" "DEFERRED(f@)" "CANCELLED(x@)"))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)))

(custom-set-variables
 '(org-plantuml-jar-path "~/Dropbox/bin/plantuml.jar")
 '(org-confirm-babel-evaluate nil))

(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [(control ?x) ( meta ?r)] 'remember)
(custom-set-variables
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote ((116 "* TODO %?\n  %u" "~/Dropbox/tasks.org" "Tasks")
           (110 "* %u %?" "~/Dropbox/notes.org" "Notes"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))
