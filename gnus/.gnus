(setq gnus-select-method '(nntp "news.gmane.org"))

(setq gnus-secondary-select-methods
      '((nnmaildir "stcnet"
                   (directory "~/Mail/riffm-at-rnd.stcnet.ru")
                   (get-new-mail t)
                   (nnir-search-engine notmuch))))

(setq nnmail-split-methods
      '(("STM" "^From:.*rnd\.stcnet\.ru.*")
        ("misc" "")))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-get-new-news-hook 'offlineimap)
(setq mm-text-html-renderer 'gnus-w3m)

(autoload 'notmuch "notmuch" "notmuch mail" t)
