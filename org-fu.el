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
(setq org-agenda-todo-ignore-with-date t)
(unless (assoc "t" org-capture-templates)
  (add-to-list 'org-capture-templates
               `("t" "TODO" entry (file+headline ,(orfu-expand "gtd.org") "Tasks")
                     "* TODO %?\nAdded: %U\n")))

(orca-wash-configure
 "https://www.podbean.com" (orca-wash-rep "Download - \\([^|]+*\\) | Podbean" "\\1"))
(orca-wash-configure
 "https://www.youtube.com" (orca-wash-rep " - YouTube" ""))

(setq orca-handler-list
      (delete-dups
       (append
        '((orca-handler-project)
          (orfu-handle-link-youtube)
          (orca-handler-current-buffer "\\* Tasks")
          (orfu-handle-link-github))
        orca-handler-list)))

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

(defun orfu-shell (cmd output-buffer)
  "Run CMD in OUTPUT-BUFFER."
  (save-window-excursion
    (with-current-buffer (shell output-buffer)
      (comint-clear-buffer)
      (insert cmd)
      (comint-send-input))))

(defun orfu-difference (set1 set2)
  (cl-set-difference set1 set2 :test #'equal))

(defun orfu--youtube-link ()
  (let ((link (caar org-stored-links)))
    (when (and link (string-match-p "https://www.youtube.com/watch" link))
      (when (string-match "\\`\\(.*\\)&list=.*" link)
        (setq link (match-string 1 link)))
      (when (string-match "\\`\\(.*\\)&index=.*" link)
        (setq link (match-string 1 link)))
      link)))

(defun orfu-handle-link-youtube ()
  (let ((link (orfu--youtube-link)))
    (when link
      (setq orca-link-hook nil)
      ;; (orfu--handle-link-youtube-1 link)
      )))

;;** agenda
(defun orfu-tags-projects ()
  nil)

(setq org-agenda-custom-commands
      `(("n" "Agenda and all TODO's"
             ((agenda "")
              (todo "PROG")
              (todo "TODO"
                    ((org-agenda-files '("~/Dropbox/org/projects/habit/habit.org"))
                     (org-agenda-todo-ignore-with-date nil)))
              (todo "NEXT")
              (todo "TODO")
              ;; (todo "LIST")
              ))
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

(defvar orfu-agenda-files-function nil
  "Function to produce a list of agenda files.")

;;;###autoload
(defun orfu-agenda-day ()
  (interactive)
  (when orfu-agenda-files-function
    (setq org-agenda-files (funcall orfu-agenda-files-function)))
  (org-agenda nil "n"))

;;;###autoload
(defun orfu-agenda-articles ()
  (interactive)
  (org-agenda nil "r"))

(provide 'org-fu)

;;; org-fu.el ends here
