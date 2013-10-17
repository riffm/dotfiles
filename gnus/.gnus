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
 mm-text-html-renderer 'gnus-w3m
 gnus-group-line-format "%M%S%5y/%R:%B%(%g%)\n"
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-get-new-news-hook 'offlineimap)

(autoload 'notmuch "notmuch" "notmuch mail" t)
