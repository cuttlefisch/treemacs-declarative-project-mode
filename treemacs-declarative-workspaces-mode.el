;;; treemacs-declarative-workspaces-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 14, 2023
;; Modified: January 17, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cuttlefisch/treemacs-declarative-workspaces-mode
;; Package-Requires: ((emacs "25.1") (treemacs "3.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;      The list of the desired workspace state operates as
;;      anindependent workspace collection a separate from the
;;      treemacs-workspaces. The same project & workspace structs are used for
;;      compatibility & simplicity, but the only interaction with
;;      treemacs-workspaces is through the treemacs-find-file hook to prevent
;;      the displayed workspace contents from changing.
;;
;;  Description
;;
;;; Code:
(require 'treemacs)
(require 'treemacs-workspaces)
(require 'yaml)
(require 'yaml-mode)

(defun treemacs-declarative-workspaces--minimal-desired-state (&optional default-name)
  (list (treemacs-workspace->create! list :name (or default-name "Default") :projects '())))

(defvar treemacs-declarative-workspaces--desired-state (treemacs-declarative-workspaces--minimal-desired-state)
  "List of desired workspace/project state from decared worksapces.")
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

(defun treemacs-declarative-workspaces--workspaces-by-name (name)
  "Return first workspace in desired state with 'name NAME or nil."
  (pp (format "finding workspace by name:\t%s" name))
  (let ((ws (treemacs-declarative-workspaces--find-by-slot-value
             'name
             name
             treemacs-declarative-workspaces--desired-state
             'treemacs-workspace)))
    (pp (format "%s" ws))
    ws))

(defun treemacs-declarative-workspaces--workspaces-projects (workspace)
  (let ((projects (cl-struct-slot-value
                   'treemacs-workspace
                   'projects
                   workspace)))
    (pp (format "projects:\n%s" projects))
    projects))

(defun treemacs-declarative-workspaces--append-project (workspace project)
  "Append PROJECT to the `projects` slot of WORKSPACE struct and update the struct in treemacs-declarative-workspaces--desired-state."
  (let* ((index (cl-position workspace treemacs-declarative-workspaces--desired-state :test #'equal))
         (new-workspace (copy-sequence workspace))
         (projects (treemacs-declarative-workspaces--workspaces-projects new-workspace))
         (new-projects (cl-pushnew project projects :test #'equal)))
    (print (format "\n\nAbout to set %s to \n%s\n\n" index new-projects))
    (setf (treemacs-workspace->projects new-workspace)  new-projects)
    (setf (nth index treemacs-declarative-workspaces--desired-state) new-workspace)))

(defun treemacs-declarative-workspaces--save-cache ()
  "Write current desired state to cache file."
  ;; TODO this looks pretty hacky
  (with-temp-file treemacs-declarative-workspaces--cache-file
    (let ((buf (current-buffer)))
      (insert "(setq treemacs-declarative-workspaces--desired-state (list " )
      (seq-map (lambda (workspace)
                 (prin1 workspace buf))
               treemacs-declarative-workspaces--desired-state)
      (insert "))" ))))

(defun treemacs-declarative-workspaces--read-cache ()
  "Write current desired state to cache file."
  (with-temp-buffer
    (insert-file-contents treemacs-declarative-workspaces--cache-file)
    (let ((cached-state (buffer-string)))
      (cond
       ((not (eq cached-state ""))
        ;; TODO this is obviously not secure, but we're in emacs
        (load treemacs-declarative-workspaces--cache-file))
       (t
        (treemacs-declarative-workspaces--minimal-desired-state))))))

(defun treemacs-declarative-workspaces--assign-project (project-attrs workspace)
  "Add PROJECT to WORKSPACE in desired state."
  (interactive)
  (print "about to enter first let")
  (print (format "using workspace:\n%s" workspace))
  (let ((target-workspace (treemacs-declarative-workspaces--workspaces-by-name workspace)))
    (print (format "working with:\t%s" target-workspace))
    (print (format "using these attrs with:\t%s" project-attrs))
    (cond
     ((treemacs-workspace-p target-workspace)
      (let ((project (apply 'treemacs-project->create! project-attrs)))
        (print (format "about to append:\t%s" project))
        (treemacs-declarative-workspaces--append-project target-workspace project)))
     (t  ; Workspace didn't exist, create it along with new project
      (print (format "about to create workspace:\t%s" workspace))
      (cl-pushnew (treemacs-workspace->create!
                   :name workspace
                   :projects (list (apply 'treemacs-project->create!
                                          project-attrs)))
                  treemacs-declarative-workspaces--desired-state
                  :test #'equal))))
  (when treemacs-declarative-workspaces-mode
    (treemacs-declarative-workspaces--override-workspaces))
  (treemacs-declarative-workspaces--save-cache))

(defun treemacs-declarative-workspaces--unassign-project (project workspace)
  "Add PROJECT to WORKSPACE in desired state."
  (interactive)
  (print (format"Removing project:\t%s" project))
  (let* ((workspace (treemacs-declarative-workspaces--workspaces-by-name workspace))
         (new-projects (cl-remove project
                                  (treemacs-workspace->projects workspace)
                                  :test #'string=
                                  :key (lambda (pj) (treemacs-project->name pj)))))
    (setf (treemacs-workspace->projects workspace) new-projects))
  (when treemacs-declarative-workspaces-mode
    (treemacs-declarative-workspaces--override-workspaces))
  (treemacs-declarative-workspaces--save-cache))

(defun treemacs-declarative-workspaces--remove-workspace (workspace)
  "Add PROJECT to WORKSPACE in desired state."
  (interactive)
  (setq treemacs-declarative-workspaces--desired-state
        (cl-remove (treemacs-declarative-workspaces--workspaces-by-name workspace)
                   treemacs-declarative-workspaces--desired-state
                   :test #'equal))
  (when treemacs-declarative-workspaces-mode
    (treemacs-declarative-workspaces--override-workspaces))
  (treemacs-declarative-workspaces--save-cache))

(defun treemacs-declarative-workspaces--assign-declared-project (project-resources)
  "Assign a project when it's declared."
  (message "assigning based on these resources:\n%s" project-resources)
 (when treemacs-declarative-workspaces-mode
  (when-let ((project-workspaces (gethash 'treemacs-workspaces project-resources))
             (project-file (gethash 'project-file project-resources)))
    (message "Looks like we're ready to start assigning workspaces")
    (seq-doseq (workspace project-workspaces)
      (let* ((project-name (or (gethash 'project-name
                                        project-resources)
                               workspace))
             (project-attrs (list
                             :name project-name
                             :path (file-name-directory project-file)
                             :path-status 'local-readable
                             :is-disabled? nil)))
        (message "about to actually assign")
        (treemacs-declarative-workspaces--assign-project project-attrs
                                                         workspace))))))

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
      (progn
        (treemacs-declarative-workspaces--read-cache)
        (advice-add 'treemacs-add-and-display-current-project
                    :around #'(lambda (&rest args) (treemacs--init)) )
        (add-hook 'declarative-project--apply-treemacs-workspaces-hook
                  #'treemacs-declarative-workspaces--assign-declared-project)
        (add-hook 'treemacs-switch-workspace-hook
                  #'treemacs-declarative-workspaces--override-workspaces))
    (progn
      (advice-remove 'treemacs-add-and-display-current-project #'(lambda (&rest args) (treemacs--init)) )
      (remove-hook 'declarative-project--apply-treemacs-workspaces-hook
                   #'treemacs-declarative-workspaces--assign-declared-project)
      (remove-hook 'treemacs-switch-workspace-hook
                   #'treemacs-declarative-workspaces--override-workspaces))))

(provide 'treemacs-declarative-workspaces-mode)
;;; treemacs-declarative-workspaces-mode.el ends here
