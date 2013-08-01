
(if window-system
    (tool-bar-mode -1))

(load-theme 'wombat)

(require 'package)
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
(setq whitespace-style '(face trailing tabs empty tab-mark))
(add-hook 'prog-mode-hook #'(lambda () (whitespace-mode t)))

(setq tramp-default-method "ssh")

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

;; get rid of this OS specific path
(defvar slime-root-path "/opt/local/share/emacs/site-lisp/slime")

(if (file-exists-p slime-root-path)
    (progn
      (add-to-list 'load-path slime-root-path)
      (setq slime-lisp-implementations
            `((sbcl ("/opt/local/bin/sbcl"))
              (ccl ("/opt/local/bin/ccl64"))
              (clisp ("/opt/local/bin/clisp"))))
      (require 'slime)
      (slime-setup)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
