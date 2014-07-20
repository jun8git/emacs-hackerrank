; contest: master; challenge: solve-me-first;
(setq debug-on-error t)
(require 'json)
(setq url-cookie-trusted-urls '("https://www\\.hackerrank\\.com/.*"))
(defvar flag t)
(defvar hackerrank_submission_url "https://www.hackerrank.com")

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(setq extList (make-hash-table :test 'equal))
(puthash "sh" "bash" extList)
(puthash "c" "c" extList)
(puthash "cpp" "cpp" extList)
(puthash "C" "cpp" extList)
(puthash "py" "python" extList)
(puthash "java" "java" extList)
(puthash "sc" "scala" extList)
(puthash "el" "cpp" extList)  ;;testing remove this

(defun getLanguage (file) (gethash (file-name-extension file) extList))

(defun hr-get-first-line (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (thing-at-point 'line))))

(defun hr-is-first-line-valid (str) (string-match "contest:[[:blank:]]*[-_[:alnum:]]+[[:blank:]]*;[[:blank:]]*challenge:[[:blank:]]*[-_[:alnum:]]+[[:blank:]]*;" str))

(defun hr-get-trimmed-first-line (str)
  (string-match "contest:[[:blank:]]*[-_[:alnum:]]+[[:blank:]]*;[[:blank:]]*challenge:[[:blank:]]*[-_[:alnum:]]+[[:blank:]]*;" str)
  (match-string-no-properties 0 str))

(defun hr-get-contest (str)
  (string-match "contest:[[:blank:]]*[-_[:alnum:]]+[[:blank:]]*" str)
  (setq tmp (match-string 0 str))
  (string-match "[[:blank:]]*[-_[:alnum:]]+[[:blank:]]*" tmp 8)
  (chomp (match-string 0 tmp)))

(defun hr-get-challenge (str)
  (string-match "challenge:[[:blank:]]*[-_[:alnum:]]+[[:blank:]]*" str)
  (setq tmp (match-string 0 str))
  (string-match "[[:blank:]]*[-_[:alnum:]]+[[:blank:]]*" tmp 10)
  (chomp (match-string 0 tmp)))

(defun hr-get-json (bufer)
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (json-read))
  )

(defun hr-get-callback (status)
  (setq buffer (current-buffer))
  (setq tmp (hr-get-json buffer))
  (setq model (cdr (assoc 'model tmp)))
  (pop-to-buffer (current-buffer))
  (erase-buffer)
  (insert (format "%s" (list (assoc 'compilemessage model) (assoc 'stdin model) (assoc 'stderr model) (assoc 'stdout model)))))

(defun hr-send-get (submission_id)
  ;; (setq submission_id (cdr (assoc 'id (cdr (assoc 'model (hr-get-json buffer))))))
  (setq url-request-method "GET")
  (sit-for 2)
  (url-retrieve (concat hackerrank_submission_url (number-to-string submission_id)) 'hr-get-callback))

(defun hr-post-callback (status)
  (setq buffer (current-buffer))
  (hr-send-get (cdr (assoc 'id (cdr (assoc 'model (hr-get-json buffer))))))) ;;check later why error

(defun hr-make-post-req (args)
  (setq url-request-method "POST")
  (setq url-request-data (mapconcat (lambda (arg)
                                      (concat (url-hexify-string (car arg))
                                              "="
                                              (url-hexify-string (cdr arg))))
                                    args
                                    "&"))
  (url-retrieve hackerrank_submission_url 'hr-post-callback))

(defun hr-main ()
  (setq hr-first-line (hr-get-first-line))
  (message (concat "Parsing " (hr-get-first-line)))
  (if (not (hr-is-first-line-valid hr-first-line))
      (message "Add comment on top your code in this format. contest: <contest-name>; challenge: <challenge-name>; ")
    (setq hr-trimmed-first-line (hr-get-trimmed-first-line hr-first-line))
    (setq hr-contest (hr-get-contest hr-trimmed-first-line))
    (setq hr-challenge (hr-get-challenge hr-trimmed-first-line))
    (setq hackerrank_submission_url
          (concat "https://www.hackerrank.com/rest/contests/" hr-contest "/challenges/" hr-challenge "/compile_tests/"))
    (hr-make-post-req (list (cons "code" (buffer-string)) (cons "language" (getLanguage buffer-file-name)) '("customtestcase" . "false")))
    ;;    (hr-make-post-req (list (cons "code" "someting") (cons "language" (getLanguage buffer-file-name)) '("customtestcase" . "false")))
    ))

(global-set-key (kbd "<f7>") '(lambda () (interactive) (hr-main)))
