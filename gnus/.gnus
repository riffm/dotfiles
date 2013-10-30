(setq gnus-select-method '(nntp "news.gmane.org"))

(setq gnus-secondary-select-methods
      '((nnmaildir "stcnet"
                   (directory "~/Mail/riffm-at-rnd.stcnet.ru")
                   (get-new-mail t)
                   (nnir-search-engine notmuch))))

(setq nnmail-split-methods
      '(("STM" "^From:.*rnd\.stcnet\.ru.*")
        ("misc" "")))

(setq-default
 user-mail-address "riffm2005@gmail.com"
 mm-text-html-renderer 'gnus-w3m
 gnus-group-line-format "%M%S%5y/%R:%B%(%g%)\n"
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(autoload 'notmuch "notmuch" "notmuch mail" t)

(setq-default gnus-posting-styles
              '((".*" (address "riffm2005@gmail.com"))
                ("stcnet:.*" (address "riffm@rnd.stcnet.ru"))))

(setq gnus-message-archive-group
      '(("stcnet:.*" "nnmaildir+stcnet:Sent Messages")
        (".*" (concat "nnfolder+archive:sent."
                      (format-time-string "%Y-%m"))))
      gnus-gcc-mark-as-read t)
