;;; org-fu.el --- Org-mode tricks. -*- lexical-binding: t -*-
;; Expected files:
;; * gtd.org with level 1: Tasks, Projects
;;
;;   Projects with level 2: elisp, function-args, tiny.el, el-TeX,
;;   Worf, Scientific Articles
;;
;; * ent.org with level 1: Articles, Videos
;; * wiki/stack.org with level 1: Questions

;;* base directory
(require 'orca)
(require 'json)
(require 'org-protocol)
(require 'org-capture)

(defvar orfu-org-basedir "~/Dropbox/org")

(defun orfu-expand (name)
  (expand-file-name name orfu-org-basedir))

(defcustom orfu-github-project-name
  "https://github\\.com/abo-abo/\\([^/]+\\)"
  "Regex for Github repository projects."
  :type 'string)

;;* capture
;;** basic
;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
(setq org-capture-templates
      `(("t" "TODO" entry (file+headline ,(orfu-expand "gtd.org") "Tasks")
             "* TODO %?\nAdded: %T\n")
        ("L" "Link" entry #'orfu-handle-link
             "* TODO %(orfu-wash-link)\nAdded: %T\n%(orfu-link-hooks)\n")
        ("p" "Link" entry #'orfu-handle-link
             "* TODO %(orfu-wash-link)\nAdded: %T\n%i%(orfu-link-hooks)\n")))

(defun orfu-wash-link ()
  "Return a pretty-printed top of `org-stored-links'.
Try to remove superfluous information, like website title."
  (let ((link (caar org-stored-links))
        (title (cl-cadar org-stored-links)))
    (org-make-link-string
     link
     (replace-regexp-in-string " - Stack Overflow" "" title))))

(defvar orfu-link-hook nil)

(defun orfu-link-hooks ()
  (prog1
      (mapconcat #'funcall
                 orfu-link-hook
                 "\n")
    (setq orfu-link-hook nil)))

(setq orca-handler-list
      (delete-dups
       (append
        '((orca-handler-project)
          (orca-handler-current-buffer "\\* Tasks")
          (orfu-handle-link-youtube)
          (orfu-handle-link-github))
        orca-handler-list)))

(defun orfu-handle-link-scholar ()
  (let ((link (caar org-stored-links)))
    (when (string-match "^https://scholar.google.com/scholar.bib" link)
      (url-retrieve
       link
       (lambda (status)
         (let ((err (plist-get status :error)))
           (if err (error
                    "\"%s\" %s" link
                    (downcase (nth 2 (assq (nth 2 err) url-http-codes)))))
           (message (buffer-substring-no-properties
                     (point-min)
                     (point-max)))))
       nil nil t))))

(defun orfu-handle-link-github ()
  (let ((link (caar org-stored-links))
        (title (cl-cadar org-stored-links)))
    (when (string-match orfu-github-project-name link)
      (let ((project-name (match-string 1 link))
            (parts (split-string title "·")))
        (setf (cl-cadar org-stored-links)
              (concat (car parts)
                      (substring (cadr parts) 7)))
        (find-file (orfu-expand "wiki/github.org"))
        (goto-char (point-min))
        (re-search-forward (concat "^\\*+ +" project-name) nil t)))))

(defun orfu-handle-link ()
  (let ((link (orfu--youtube-link)))
    (when link
      (setq orfu-link-hook nil)
      (add-hook 'orfu-link-hook (lambda () (orfu--handle-link-youtube-2 link))))
    (orca-handle-link)))

(defun orfu-shell (cmd output-buffer)
  "Run CMD in OUTPUT-BUFFER."
  (save-window-excursion
    (with-current-buffer (shell output-buffer)
      (comint-clear-buffer)
      (insert cmd)
      (comint-send-input))))

(defun orfu--youtube-output-buffer ()
  (let ((max-id 0))
    (dolist (b (buffer-list))
      (when (string-match "\\`\\*youtube-dl \\([0-9]+\\)\\*\\'" (buffer-name b))
        (with-current-buffer b
          (if (looking-back comint-prompt-regexp)
              (kill-buffer)
            (setq max-id
                  (max (string-to-number (match-string 1 (buffer-name b)))
                       max-id))))))
    (format "*youtube-dl %d*" (1+ max-id))))

(defun orfu--youtube-link ()
  (let ((link (caar org-stored-links)))
    (when (and link (string-match "https://www.youtube.com/" link))
      (when (string-match "\\`\\(.*\\)&list=.*" link)
        (setq link (match-string 1 link)))
      (when (string-match "\\`\\(.*\\)&index=.*" link)
        (setq link (match-string 1 link)))
      link)))

(defmacro orfu-wait (expr &optional n-seconds)
  (setq n-seconds (or n-seconds 10))
  `(let ((tic (current-time))
         timeout)
     (while (not (or ,expr timeout))
       (when (> (time-to-seconds (time-subtract (current-time) tic)) ,n-seconds)
         (setq timeout t))
       (sit-for 0.1))
     (when timeout
       (error "Waited %d seconds for %S" ,n-seconds ',expr))))

(defun orfu--youtube-json (cmd)
  (dolist (file (directory-files default-directory t
                                 "\\.info\\.json\\'"))
    (delete-file file))
  (orfu-shell cmd (orfu--youtube-output-buffer))
  (let (description-json-file)
    (ignore-errors
      (orfu-wait
       (setq description-json-file
             (car (directory-files default-directory t
                                   "\\.info\\.json\\'")))
       5))
    (when description-json-file
      (prog1 (json-read-file description-json-file)
        (delete-file description-json-file)))))

(defvar orfu-youtube-file-format "youtube-%(uploader)s-%(title)s.%(ext)s")

(defun orfu-difference (set1 set2)
  (cl-set-difference set1 set2 :test #'equal))

(defun orfu--handle-link-youtube-1 (link &optional no-org)
  (setq link (replace-regexp-in-string "time_continue=[0-9]+&" "" link))
  (let* ((default-directory "~/Downloads/Videos")
         (cmd (format
               "setsid -w youtube-dl -f mp4 --write-info-json -o %S %s"
               orfu-youtube-file-format link))
         (json (orfu--youtube-json cmd)))
    (if (not json)
        (progn
          (goto-char (point-min))
          (re-search-forward "^\\*+ +Videos$")
          (org-capture-put
           :immediate-finish t
           :jump-to-captured t)
          t)
      (let* ((fname (cdr (assoc '_filename json)))
             (title (cdr (assoc 'title json)))
             (fname-part (concat fname ".part"))
             (channel (cdr (assoc 'uploader json)))
             fname-alt)
        (unless no-org
          (add-hook 'orfu-link-hook
                    `(lambda ()
                       ,(concat (org-make-link-string (expand-file-name fname) title)
                                (format "\nDuration: %d." (/ (cdr (assoc 'duration json)) 60)))))
          (goto-char (point-min))
          (unless (re-search-forward (concat "^\\*+ +" channel) nil t)
            (re-search-forward "^\\*+ +Misc$")
            (insert "\n** " channel))
          (org-capture-put
           :immediate-finish t
           :jump-to-captured t))
        (cond ((file-exists-p
                (setq fname-alt (replace-regexp-in-string "mp4$" "mkv" fname)))
               (orfu--start-vlc fname-alt fname-alt))
              ((file-exists-p
                (setq fname-alt (replace-regexp-in-string "mp4$" "webm" fname)))
               (orfu--start-vlc fname-alt fname-alt))
              (t
               (condition-case nil
                   (orfu--start-vlc fname fname-part)
                 (error
                  (progn
                    (orfu-shell
                     (replace-regexp-in-string "-f mp4 " "" cmd)
                     (orfu--youtube-output-buffer))
                    (orfu--start-vlc fname fname))))))
        t))))

(defun orfu--handle-link-youtube-2 (link)
  (setq link (replace-regexp-in-string "time_continue=[0-9]+&" "" link))
  (let ((dir "~/Downloads/Videos"))
    (make-directory dir t)
    (let* ((default-directory dir)
           (cmd (format
                 "setsid -w youtube-dl -f mp4 -o %S %s"
                 orfu-youtube-file-format link))
           (buf (orfu--youtube-output-buffer)))
      (orfu-shell cmd buf)
      (with-current-buffer buf
        (while (not (string-match "\\[download\\] \\(?:Destination: \\)?\\(.+\\.mp4\\)\\(?: has already been downloaded\\)?" (buffer-string)))
          (accept-process-output (get-buffer-process (current-buffer)) 0.5))
        (let* ((fname (match-string-no-properties 1 (buffer-string)))
               (channel (progn
                          (string-match "youtube-\\(.*\\)-\\(.*\\)\\.mp4$" fname)
                          (match-string 1 fname)))
               (title (match-string 2 fname))
               (fname-part (concat fname ".part"))
               fname-alt)
          (prog1 (org-make-link-string (expand-file-name fname dir) title)
            (cond ((file-exists-p
                    (setq fname-alt (replace-regexp-in-string "mp4$" "mkv" fname)))
                   (orfu--start-vlc fname-alt fname-alt))
                  ((file-exists-p
                    (setq fname-alt (replace-regexp-in-string "mp4$" "webm" fname)))
                   (orfu--start-vlc fname-alt fname-alt))
                  (t
                   (condition-case nil
                       (orfu--start-vlc fname fname-part)
                     (error
                      (progn
                        (orfu-shell (replace-regexp-in-string "-f mp4 " "" cmd) buf)
                        (orfu--start-vlc fname fname))))))))))))

(defcustom orfu-start-vlc-if-already-running t
  "When non-nil, start a new VLC."
  :type 'boolean)

(defun orfu--start-vlc (fname fname-part)
  (orfu-wait
   (or (file-exists-p fname) (file-exists-p fname-part)))
  (orfu-wait
   (or (file-exists-p fname)
       (and (file-exists-p fname-part)
            (> (read (counsel--command
                      "du" "-schb"
                      fname-part))
               (* 3 1024 1024)))))
  (when (or orfu-start-vlc-if-already-running
            (string= "" (shell-command-to-string "pidof vlc")))
    (orly-start
     "vlc"
     (if (file-exists-p fname-part)
         fname-part
       fname))))

(defun orfu-handle-link-youtube ()
  (let ((link (orfu--youtube-link)))
    (when link
      (find-file (orfu-expand "wiki/youtube.org"))
      (goto-char (point-min))
      (when (search-forward link nil t)
        (message "Link already captured"))
      (setq orfu-link-hook nil)
      (orfu--handle-link-youtube-1 link))))

;;** agenda
(defun orfu-tags-projects ()
  nil)

(setq org-agenda-custom-commands
      `(("n" "Agenda and all TODO's" ((agenda "")
                                      (alltodo "")))
        ("h" "Office and Outside and Home"
             ((agenda)
              (tags-todo "OFFICE")
              (tags-todo "OUTSIDE")
              ,@(orfu-tags-projects)
              (tags-todo "HOME")))
        ("r" "Articles"
             ((tags-todo "article")
              (tags-todo "MASTERINGEMACS")))
        ("o" "Office and Outside"
             ((agenda)
              (tags-todo "OFFICE")
              (tags-todo "OUTSIDE")
              ,@(orfu-tags-projects)))
        ("d" "Daily Action List"
             ((agenda ""
                      ((org-agenda-ndays 1)
                       (org-agenda-sorting-strategy
                        '((agenda time-up priority-down tag-up)))
                       (org-deadline-warning-days 0)))))
        ("P" "Project List"
             ((tags "PROJECT")))
        ("w" "Weekly"
             ((agenda ""
                      ((org-agenda-span 'week)
                       (org-agenda-skip-function 'orfu-skip-daily-tasks)
                       (org-deadline-warning-days 0)))))))

(defun orfu-skip-daily-tasks ()
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (headline (or (and (org-at-heading-p) (point))
                      (save-excursion (org-back-to-heading))))
        (rep-str (org-get-repeat)))
    (if (and rep-str (string-match "\\+[0-9]+d" rep-str))
        next-headline
      nil)))

;;;###autoload
(defun orfu-agenda-quick ()
  (interactive)
  (org-agenda nil "h"))

;;;###autoload
(defun orfu-agenda-office ()
  (interactive)
  (org-agenda nil "o"))

(defvar orfu-agenda-files-home nil)

(defun orfu-vacation-p ()
  "Requires a file \"wiki/vacation.org\".
With contents, for example:
    * 2017
    ** day off <2017-03-17 Fri>"
  (let ((vacation-file (orfu-expand "wiki/vacation.org")))
    (when (file-exists-p vacation-file)
      (let ((today (format-time-string "%Y-%m-%d")))
        (with-current-buffer (find-file-noselect
                              vacation-file)
          (save-excursion
            (goto-char (point-min))
            (re-search-forward (concat "<" today "[ a-zA-Z]*>") nil t)))))))

;;;###autoload
(defun orfu-agenda-day ()
  (interactive)
  (let* ((ct (decode-time (current-time)))
         (hour (nth 2 ct))
         (dow (nth 6 ct)))
    (if (and
         (member dow '(1 2 3 4 5))
         (>= hour 9)
         (< hour 17)
         (not (orfu-vacation-p)))
        (setq org-agenda-files
              (orfu-difference org-agenda-files orfu-agenda-files-home))
      (setq org-agenda-files
            (cl-union
             org-agenda-files orfu-agenda-files-home
             :test 'equal))))
  (org-agenda nil "d")
  (setq default-directory orfu-org-basedir))

;;;###autoload
(defun orfu-agenda-articles ()
  (interactive)
  (org-agenda nil "r"))

(provide 'org-fu)

;;; org-fu.el ends here
