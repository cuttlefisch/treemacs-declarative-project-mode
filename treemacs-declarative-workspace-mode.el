;;; treemacs-declarative-workspace-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 14, 2023
;; Modified: January 14, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cuttlefisch/treemacs-declarative-workspace-mode
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;      The hash table representing the desired workspace state operates as
;;      anindependent workspace collection a separate from the
;;      treemacs-workspaces. The same project & workspace structs are used for
;;      compatibility & simplicity, but the only interaction with
;;      treemacs-workspaces is through the treemacs-find-file hook to prevent
;;      the displayed workspace contents from changing.
;;
;;  Description
;;
;;; Code:
                                        ;(defvar treemacs-declarative-workspaces->persist-workspaces t)
(require 'treemacs)

(defvar treemacs-declarative-workspaces--desired-state '()
  "Hashmap holding desired workspace/project state from decared worksapces.")
(defvar treemacs-declarative-workspaces--cache-file
  (expand-file-name "~/.emacs.d/.local/cache/treemacs-declared-worksapces.el")
  "Store persistant treemacs declared workspaces.")

(defun treemacs-declarative-workspaces--find-by-slot-value (slot value structs &optional struct-type)
  "Return the first struct in STRUCTS to have KEY of VALUE or nil."
  (let ((struct-type (or
                      struct-type
                      (type-of (car structs)))))
    (seq-find (lambda (struct)
                (string= (cl-struct-slot-value struct-type slot struct) value))
              structs)))

(defun treemacs-declarative-workspaces--workspace-by-name (name)
  "Return first workspace in desired state with 'name NAME or nil."
  (pp (format "finding workspace by name:\t%s" name))
  (let ((ws (treemacs-declarative-workspaces--find-by-slot-value
   'name
   name
   treemacs-declarative-workspaces--desired-state
   'treemacs-workspace)))
    (pp (format "%s" ws))
    ws))

(defun treemacs-declarative-workspaces--workspace-projects (workspace)
  (let ((projects (cl-struct-slot-value
   'treemacs-workspace
   'projects
   workspace)))
    (pp (format "projects:\n%s" projects))
        projects))

;; (defun treemacs-declarative-workspaces--append-project (workspace project)
;;   "Update WORKSPACE struct projects slot to PROJECTS."
;;     (setf (treemacs-declarative-workspaces--workspace-projects workspace)
;;           (append (treemacs-declarative-workspaces--workspace-projects workspace) project)))

(defun treemacs-declarative-workspaces--append-project (workspace project)
  "Append PROJECT to the `projects` slot of WORKSPACE struct and update the struct in treemacs-declarative-workspaces--desired-state"
  (let* ((index (cl-position workspace treemacs-declarative-workspaces--desired-state :test #'equal))
         (new-workspace (copy-sequence workspace))
         (new-projects (append (treemacs-declarative-workspaces--workspace-projects new-workspace) project)))
    (pp (format "\n\nAbout to set %s to \n%s\n\n" index new-projects))
    (setf (treemacs-workspace->projects new-workspace)  new-projects)
    (setf (nth index treemacs-declarative-workspaces--desired-state) new-workspace)))


(defun treemacs-declarative-workspace--assign-project (project-attrs workspace)
  "Add PROJECT to WORKSPACE in desired state."
  (interactive)
  (print "about to enter first let")
  (print (format "using workspace:\n%s" workspace))
  (let ((target-workspace (treemacs-declarative-workspaces--workspace-by-name workspace)))
    (print (format "working with:\t%s" target-workspace))
    (print (format "using these attrs with:\t%s" project-attrs))
    (cond
                                        ; Workspace already existed, add new sibling
     ((treemacs-workspace-p target-workspace)
      (let ((project (apply 'treemacs-project->create! project-attrs)))
        (print (format "about to append:\t%s" project))
        ;(print (format "to:\t%s" siblings))
        (treemacs-declarative-workspaces--append-project target-workspace project)))
     (t  ; Workspace didn't exist, create it along with new project
      (print (format "about to create workspace:\t%s" workspace))
      (setq treemacs-declarative-workspaces--desired-state
       (list (treemacs-workspace->create!
              :name workspace
              :projects (list (apply 'treemacs-project->create!
                                     project-attrs)))))))))

(defun treemacs-declarative-workspace--unassign-project (project workspace)
  "Add PROJECT to WORKSPACE in desired state."
  (interactive)
  (message "coming soon..."))

(defun treemacs-declarative-workspaces--treemacs-desired-state ()
  "Return value that should be assigned to treemacs-workspaces."
                                        ; basically converting hashtable to list of treemacs-workspace/treemacs-projects?
  (message "coming soon..."))

(defun treemacs-declarative-workspaces--override-workspaces ()
  "Set treemacs-workspaces to desired state."
  (setq treemacs--workspaces treemacs-declarative-workspaces--desired-state))

;;;###autoload
(define-minor-mode treemacs-declarative-workspaces-mode
  "When enabled, ensure treemacs workspace contents always match a
desired state."
  :init-value nil
  :global t
  :group 'treemacs
  :lighter "treemacs-declarative-workspaces-mode"
  (if treemacs-declarative-workspaces-mode
        ;; Code to run when the mode is turned on
        ;(add-hook treemacs-find-file-hook
        (add-hook 'treemacs-switch-workspace
                  #'treemacs-declarative-workspaces--override-workspaces)
      (remove-hook 'treemacs-switch-workspace
      ;(remove-hook treemacs-find-file-hook
                   #'treemacs-declarative-workspaces--override-workspaces)))

;; Optionally, you can bind the mode to a key
                                        ;(global-set-key (kbd "C-c m") 'your-mode)

(provide 'treemacs-declarative-workspace-mode)
;;; treemacs-declarative-workspace-mode.el ends here
