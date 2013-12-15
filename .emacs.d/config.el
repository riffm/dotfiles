
(set-default-coding-systems 'utf-8)

(if window-system
    (tool-bar-mode -1))

(load-theme 'wombat)

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

(add-hook 'find-file-hook 'flymake-find-file-hook)

(eval-after-load "flymake"
  '(progn
     (local-set-key "\M-n" 'flymake-goto-next-error)
     (local-set-key "\M-p" 'flymake-goto-prev-error)))

(setf flymake-no-changes-timeout 20
      flymake-start-syntax-check-on-newline nil)

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

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

(require 'ahg)

(setq coffee-tab-width 2)

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
