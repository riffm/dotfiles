;;; ensime-inspector.el --- type and package inspectors

;; Type Inspector UI

(defvar ensime-inspector-buffer-name "*Inspector*")

(defvar ensime-indent-level 0
  "In inspector UI, how much to indent.")

(defun ensime-inspector-buffer-p (buffer)
  "Is this an ensime inspector buffer?"
  (eq (get-buffer ensime-inspector-buffer-name) buffer))

(defun ensime-popup-buffer-p (buffer)
  "Is this an ensime popup buffer?"
  (with-current-buffer buffer
    ensime-is-popup-buffer))

(defun ensime-inspector-insert-linked-package-path (path &optional face)
  "For each component of the package path, insert a link to inspect
   that package."
  (let ((pieces (split-string path "\\."))
	(accum ""))
    (dolist (piece pieces)
      (setq accum (concat accum piece))
      (ensime-insert-action-link
       piece
       `(lambda (x)
	  (ensime-inspect-package-by-path ,accum))
       (or face font-lock-type-face))
      (insert ".")
      (setq accum (concat accum "."))
      )))


(defun ensime-inspector-insert-link-to-type-id (text type-id &optional is-obj)
  "A helper for type link insertion. See usage in
 ensime-inspector-insert-linked-type. If is-obj is
 non-nil, use an alternative color for the link."
  (ensime-insert-action-link
   text
   `(lambda (x)
      (ensime-type-inspector-show
       (ensime-rpc-inspect-type-by-id ,type-id)
       ))
   (if is-obj
       font-lock-constant-face
     font-lock-type-face)
   ))

(defun ensime-inspector-insert-linked-type
  (type &optional with-doc-link qualified)
  "Helper utility to output a link to a type.
 Should only be invoked by ensime-inspect-type-at-point"
  (if (ensime-type-is-arrow-p type)
      (ensime-inspector-insert-linked-arrow-type type with-doc-link qualified)

    (let* ((type-args (ensime-type-type-args type))
	   (last-type-arg (car (last type-args)))
	   (is-obj (ensime-type-is-object-p type)))

      (insert (make-string ensime-indent-level ?\s))

      (if qualified
	  (ensime-with-name-parts
	   (ensime-type-full-name type)
	   (path outer-type-name name)
	   (when path
	     (ensime-inspector-insert-linked-package-path path))
	   (if (and outer-type-name (integerp (ensime-outer-type-id type)))
	       (progn
		 (ensime-inspector-insert-link-to-type-id
		  outer-type-name (ensime-outer-type-id type))
		 (insert "$")
		 (ensime-inspector-insert-link-to-type-id
		  name (ensime-type-id type) is-obj))
	     (progn
	       (ensime-inspector-insert-link-to-type-id
		name (ensime-type-id type) is-obj))))

	;; Otherwise, insert short name..
	(ensime-inspector-insert-link-to-type-id
	 (ensime-type-name type) (ensime-type-id type) is-obj))

      (when type-args
	(let ((ensime-indent-level 0))
	  (insert "[")
	  (dolist (tpe type-args)
	    (ensime-inspector-insert-linked-type tpe nil nil)
	    (if (not (eq tpe last-type-arg))
		(insert ", ")))
	  (insert "]")))

      (when with-doc-link
        (insert " ")
        (ensime-insert-link "doc" (ensime-make-doc-url type))
	(let ((pos (plist-get type :pos)))
          (when (ensime-pos-valid-local-p pos)
            (insert " ")
            (ensime-insert-link "source" pos))))

      )))

(defun ensime-inspector-insert-linked-arrow-type
  (type  &optional with-doc-link qualified)
  "Helper utility to output a link to a type.
   Should only be invoked by ensime-inspect-type-at-point"
  (let*  ((param-sections (ensime-type-param-sections type))
	  (result-type (ensime-type-result-type type)))
    (dolist (sect param-sections)
      (let ((params (plist-get sect :params)))
	(insert "(")
	(let ((last-param (car (last params))))
	  (dolist (p params)
	    (let ((tpe (cadr p)))
	      (ensime-inspector-insert-linked-type tpe nil qualified)
	      (if (not (eq p last-param))
		  (insert ", "))))
	  (insert ") => "))))
    (ensime-inspector-insert-linked-type result-type nil qualified)
    ))


(defun ensime-inspector-insert-linked-member (owner-type m)
  "Helper utility to output a link to a type member.
   Should only be invoked by ensime-inspect-type-at-point"
  (let* ((type (ensime-member-type m))
	 (pos (ensime-member-pos m))
	 (member-name (ensime-member-name m))
	 (url-or-pos
          (if (ensime-pos-valid-local-p pos)
              pos
            (ensime-make-doc-url owner-type m))))

    (if (or (equal 'method (ensime-declared-as m))
	    (equal 'field (ensime-declared-as m)))
	(progn
	  (ensime-insert-link
	   (format "%s" member-name)
           url-or-pos
           nil
	   font-lock-function-name-face)
	  (tab-to-tab-stop)
	  (ensime-inspector-insert-linked-type type nil nil))

      ;; otherwise, assume it's a nested type
      (progn
	(ensime-insert-with-face
	 (ensime-declared-as-str m)
	 'font-lock-comment-face)
	(tab-to-tab-stop)
	(ensime-inspector-insert-linked-type type nil nil)
	))
    ))


(defun ensime-inspect-type-at-point-other-frame ()
  "See ensime-inspect-type-at-point, but in other frame."
  (interactive)
  (let ((ensime-popup-in-other-frame t))
    (ensime-inspect-type-at-point)))

(defun ensime-type-inspect-info-at-point ()
  "Helper to pull the inspect info for object at point."
  (let* ((imported-type-path (ensime-imported-type-path-at-point))
	 (imported-type (when imported-type-path
			  (ensime-rpc-get-type-by-name-at-point
			   imported-type-path))))
    (if imported-type
	;; if imported type under point
	(ensime-rpc-inspect-type-by-id
	 (ensime-type-id imported-type))
      ;; otherwise do normal type inspection
      (ensime-rpc-inspect-type-at-range))))

(defun ensime-inspect-java-type-at-point ()
  "Use the global index to search for type at point.
 Inspect the type selected by user."
  (let* ((sym (ensime-sym-at-point))
	 (name (plist-get sym :name))
	 (name-start (plist-get sym :start))
	 (name-end (plist-get sym :end))
	 (suggestions (ensime-rpc-import-suggestions-at-point (list name) 10)))
    (when suggestions
      (let* ((names (mapcar
		     (lambda (s)
		       (propertize (plist-get s :name)
				   'local-name
				   (plist-get s :local-name)))
		     (apply 'append suggestions)))
	     (selected-name
	      (popup-menu*
	       names :point (point))))
	(when selected-name
	  (ensime-inspect-by-path
	   (ensime-kill-txt-props selected-name))
	  )))))

(defun ensime-inspect-type-at-point ()
  "Display a list of all the members of the type under point, sorted by
   owner type."
  (interactive)
  (let ((pack-path (ensime-package-path-at-point)))

    (cond ((ensime-visiting-java-file-p)
	   (ensime-inspect-java-type-at-point))

	  (t ;; inspect package if package under point
	   (if pack-path (ensime-inspect-package-by-path pack-path)
	     ;; otherwise, inspect type
	     (let* ((inspect-info (ensime-type-inspect-info-at-point)))
	       (ensime-type-inspector-show inspect-info)))))))

(defun ensime-type-inspector-show (info &optional focus-on-member)
  "Display a list of all the members of the type under point, sorted by
   owner type."
  (if (null info)
      (message "Cannot inspect nil type.")
    (let* ((interfaces (plist-get info :interfaces))
	   (type (plist-get info :type))
	   (companion-id (plist-get info :companion-id))
	   (buffer-name ensime-inspector-buffer-name)
	   (ensime-indent-level 0)
	   (focus-point nil))
      (ensime-with-inspector-buffer
       (buffer-name info t)

       ;; We want two main columns. The first, 20 chars wide.
       (let ((tab-stop-list '(20)))
	 (setq wrap-prefix (make-string 21 ?\s))

	 ;; Display main type
	 (let* ((full-type-name (plist-get type :name)))
	   (ensime-insert-with-face (format "%s\n"
					    (ensime-declared-as-str type))
				    font-lock-comment-face)
	   (ensime-inspector-insert-linked-type type t t)
	   (insert "\n")

	   ;; Insert a link to the companion object or class, if extant
	   (when-let (id companion-id)
                     (ensime-inspector-insert-link-to-type-id
                      "(companion)" id
                      (not (ensime-type-is-object-p type))))

	   ;; Display each member, arranged by owner type
	   (dolist (interface interfaces)
	     (let* ((owner-type (plist-get interface :type))
		    (implicit (plist-get interface :via-view))
		    (members (plist-get owner-type :members)))
	       (ensime-insert-with-face
		(format "\n\n%s%s\n"
			(ensime-declared-as-str owner-type)
			(if implicit
			    (concat " (via implicit, " implicit ")") ""))
		font-lock-comment-face)
	       (ensime-inspector-insert-linked-type owner-type t t)
	       (insert "\n")
	       (insert "---------------------------\n")
	       (dolist (m members)
		 (when (and focus-on-member
			    (equal (ensime-member-name m)
				   focus-on-member))
		   (setq focus-point (point)))
		 (ensime-inspector-insert-linked-member owner-type m)
		 (insert "\n")
		 )
	       ))

	   (if (integerp focus-point)
	       (progn (goto-char focus-point)
		      (recenter-top-bottom))
	     (goto-char (point-min)))
	   ))
       ))))



;; Inspector

(defun ensime-path-completions (path predicate flag)
  "Return a list of valid completions of the given qualified path.
See: http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_280.html for the
interface we are implementing."
  ;; Note: when this function is invoked from completing-read, current
  ;; buffer will be mini-buffer. We need to setup connection manually.
  ;; See ensime-completing-read-path...
  (ensime-with-path-and-name
   path (pack name)
   (let* ((members (ensime-rpc-package-member-completions pack name))
	  (candidates (mapcar (lambda (ea)
				(let ((name (plist-get ea :name)))
				  (if (and pack (> (length pack) 0))
				      (concat pack "." name) name)))
			      members)))
     (cond
      ((null flag) (try-completion path candidates predicate))
      ((eq flag t) (all-completions path candidates predicate))
      ((eq 'lambda flag) (member candidates path))
      (t nil)
      ))))

(defun ensime-completing-read-path (prompt &optional initial)
  ;; Note: First thing we do is bind buffer connection so
  ;; completion function will have access.
  (let ((ensime-dispatching-connection
	 (ensime-current-connection)))
    (completing-read prompt #'ensime-path-completions
		     nil nil (or initial (ensime-package-containing-point)))))

(defun ensime-inspect-package-by-path (path)
  (ensime-package-inspector-show
   (ensime-rpc-inspect-package-by-path path)))

(defun ensime-inspect-by-path (&optional path focus-on-member)
  "Open the Inspector on the type or package denoted by path. If path is nil,
read a fully qualified path from the minibuffer."
  (interactive)
  (let* ((case-fold-search nil))
    (let ((p (or path
		 (ensime-completing-read-path
		  "Qualified type or package name: "))))
      (ensime-with-path-and-name
       p (pack name)
       (if (and name (integerp (string-match "^[a-z_0-9]+$" name)))
	   (ensime-inspect-package-by-path p)
	 (let ((type (ensime-rpc-get-type-by-name p)))
	   (if type
	       (let ((info (ensime-rpc-inspect-type-by-id
			    (ensime-type-id type))))
		 (ensime-type-inspector-show info focus-on-member))
	     (message "Could not locate type named '%s'." p))
	   ))))))


(defun ensime-package-path-at-point ()
  "Return the package path at point, or nil if point is not in a package path."
  (let* ((case-fold-search nil)
	 (re "\\(?:package\\|import\\)[ ]+\\(\\(?:[a-z][a-z0-9_]+\\.\\)+[a-z][a-z0-9]+\\)"))
    (save-excursion
      (catch 'return
	(let ((init-point (point))
	      (limit (point-at-eol)))
	  (goto-char (point-at-bol))
	  (while (search-forward-regexp re limit t)
	    (if (and (>= init-point (match-beginning 1))
		     (<= init-point (match-end 1)))
		(throw 'return
		       (ensime-kill-txt-props
			(match-string 1))))))))))


(defun ensime-package-containing-point ()
  "Return the package point is in."
  (save-excursion
    (when (search-backward-regexp
	   "^package \\(\\(?:[a-z0-9_]+\\.\\)*[a-z0-9_]+\\)"
	   (point-min) t)
      (let ((path (match-string 1)))
	(ensime-kill-txt-props path)))))

(defun ensime-imported-type-path-at-point ()
  "Return the qualified name of the type being imported at point."
  (when-let (sym (symbol-at-point))
            (let ((sym-name (ensime-kill-txt-props
                             (symbol-name sym))))
              (when (and (integerp (string-match "^[A-ZA-z_]+$" sym-name))
                         (save-excursion
                           (beginning-of-line)
                           (search-forward-regexp
                            (concat
                             "^\\s-*import \\(\\(?:[a-z0-9_]+\\.\\)*\\)"
                             "\\(?:[A-Z][A-z0-9_\\.]+\\|{[A-z0-9_\\., \n]+}\\)$")
                            (point-at-eol) t)))
                (let ((path (ensime-kill-txt-props (match-string 1))))
                  (concat path sym-name))))))

(defun ensime-inspect-package-at-point ()
  "If cursor is over a package path, inspect that path. Otherwise,
inspect the package of the current source file."
  (interactive)
  (let ((pack (or (ensime-package-path-at-point)
		  (ensime-package-containing-point))))
    (if pack
	(ensime-inspect-by-path pack)
      (message "No package declaration found."))))


(defun ensime-inspect-project-package ()
  "Inspect the package declared as the project package in the config file."
  (interactive)
  (let* ((config (ensime-config))
	 (given (plist-get config :project-package)))
    (ensime-inspect-by-path given)))

(defun ensime-inspector-insert-package (pack)
  "Helper to insert a hyper-linked package name."
  (let ((name (ensime-package-full-name pack))
	(members (ensime-package-members pack)))
    (insert (make-string ensime-indent-level ?\s))
    (ensime-inspector-insert-linked-package-path name font-lock-variable-name-face)
    (insert "\n")
    (let ((ensime-indent-level (+ ensime-indent-level 5)))
      (dolist (ea members)
	(when (not (ensime-package-p ea))
	  (ensime-inspector-insert-linked-type ea t nil)
	  (ensime-insert-with-face
	   (format " %s" (ensime-declared-as-str ea))
	   font-lock-comment-face)
	  (insert "\n")))
      (dolist (ea members)
	(when (ensime-package-p ea)
	  (ensime-inspector-insert-package ea)
	  ))
      )))

(defun ensime-package-inspector-show (info)
  "Display a list of all the members of the provided package."
  (if (null info)
      (message "Cannot inspect nil package.")
    (let* ((buffer-name ensime-inspector-buffer-name)
	   (ensime-indent-level 0))
      (ensime-with-inspector-buffer
       (buffer-name info t)
       (ensime-inspector-insert-package info)
       (goto-char (point-min))
       ))))

(defvar ensime-inspector-history '()
  "Maintain a history of the info objects viewed in the inspector buffer.")

(defvar ensime-inspector-history-cursor 0
  "Where are we in the history?")

(defvar ensime-inspector-paging-in-progress nil
  "A dynamic variable to inform dynamic extant of user's intent.
   Are we moving in history, or inspecting a new info?")

(defun ensime-inspector-backward-page ()
  "Inspect the info object preceding current in history."
  (interactive)
  (setq ensime-inspector-history-cursor
	(min (- (length ensime-inspector-history) 1)
	     (+ ensime-inspector-history-cursor 1)))
  (ensime-inspector-goto-cursor))

(defun ensime-inspector-forward-page ()
  "Inspect the info object following current in history."
  (interactive)
  (setq ensime-inspector-history-cursor
	(max 0 (- ensime-inspector-history-cursor 1)))
  (ensime-inspector-goto-cursor))


(defun ensime-inspector-goto-cursor ()
  "Helper to jump to a specific point in history."
  (let ((info (nth ensime-inspector-history-cursor
		   ensime-inspector-history))
	(ensime-inspector-paging-in-progress t))

    (cond ((ensime-package-p info)
	   (ensime-package-inspector-show info))

	  ((ensime-type-inspection-p info)
	   (ensime-type-inspector-show info))

	  (t (error
	      (format "Cannot inspect unknown structure: %s"
		      info))))
    ))


(defvar ensime-popup-inspector-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map [mouse-1] 'push-button)
    (define-key map (kbd "q") 'ensime-popup-buffer-quit-function)
    (define-key map (kbd "M-n") 'forward-button)
    (define-key map (kbd "M-p") 'backward-button)
    (define-key map (kbd ".") 'ensime-inspector-forward-page)
    (define-key map (kbd ",") 'ensime-inspector-backward-page)
    map)
  "Type and package inspector key bindings.")

(provide 'ensime-inspector)

;; Local Variables:
;; no-byte-compile: t
;; End:
