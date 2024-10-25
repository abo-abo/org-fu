;;; org-fu.el --- Org-mode tricks. -*- lexical-binding: t -*-
;; Expected files:
;; - gtd.org with level 1: Tasks

(require 'orca)
(require 'org-capture)

(defun orfu-expand (name)
  (expand-file-name name org-directory))

;;* capture
;;** basic
;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
(setq org-agenda-todo-ignore-with-date t)
(unless (assoc "t" org-capture-templates)
  (add-to-list 'org-capture-templates
               `("t" "TODO" entry (file+headline ,(orfu-expand "gtd.org") "Tasks")
                 "* TODO %?\nAdded: %U\n")))

(setq orca-handler-list
      (delete-dups
       (append
        '((orca-handler-project)
          (orfu-handle-link-youtube)
          (orca-handler-current-buffer "\\* Tasks"))
        orca-handler-list)))

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
(setq org-agenda-custom-commands
      `(("n" "Agenda and all TODO's"
         ((agenda "")
          (todo "PROG")
          (todo "NEXT")
          (todo "TODO")
          ;; (todo "LIST")
          ))))

(defvar orfu-agenda-files-function nil
  "Function to produce a list of agenda files.")

;;;###autoload
(defun orfu-agenda-day ()
  (interactive)
  (when orfu-agenda-files-function
    (setq org-agenda-files (funcall orfu-agenda-files-function)))
  (org-agenda nil "n"))

(provide 'org-fu)

;;; org-fu.el ends here
