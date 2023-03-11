;;; treemacs-declarative-workspaces-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Hayden Stanko
;;
;; Author: Hayden Stanko <hayden@cuttle.codes>
;; Maintainer: Hayden Stanko <hayden@cuttle.codes>
;; Created: January 14, 2023
;; Modified: March 09, 2023
;; Version: 0.0.2
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cuttlefisch/treemacs-declarative-workspaces-mode
;; Package-Requires: ((emacs "25.1") (treemacs "2.10"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;      The list of the desired declared workspace state operates as
;;      an independent workspace collection separate from the
;;      treemacs-workspaces. The same project & workspace structs are used for
;;      compatibility & simplicity, but current treemacs integration is
;;      very basic.
;;
;;  Description
;;
;;; Code:
(require 'treemacs)
(require 'treemacs-workspaces)
(require 'declarative-project-mode)

(defun treemacs-declarative-workspaces--minimal-desired-state ()
  "Minimal contents for declared workspace, a workspace named 'Default'."
  (list (treemacs-workspace->create! :name "Default"
                                     :projects '())))

(defvar treemacs-declarative-workspaces--desired-state (treemacs-declarative-workspaces--minimal-desired-state)
  "List of desired workspaces and project contents from declared workspaces.")

(defvar treemacs-declarative-workspaces--cache-file
  (concat user-emacs-directory "treemacs-declared-workspaces.el")
  "Cache treemacs declared workspaces in Emacs's cache directory.")

(defvar treemacs-declarative-workspaces--autoprune t
  "If t remove invalid projects from declared workspace cache when read.
Prompt before removing if nil.")

(defvar treemacs-declarative-workspaces-mode nil
  "Var for treemacs-declarative-workspaces-mode.")

;; REVIEW: is this needed?
(defun treemacs-declarative-workspaces--find-by-slot-value (slot value structs &optional struct-type)
  "Return the first struct in STRUCTS to have SLOT of VALUE or nil."
  (let ((struct-type (or
                      struct-type
                      (type-of (car structs)))))
    (seq-find (lambda (struct)
                (string= (cl-struct-slot-value struct-type slot struct) value))
              structs)))

(defun treemacs-declarative-workspaces--workspace-memberp (project workspace)
  "Return PROJECT if member of given WORKSPACE."
        (member project (treemacs-workspace->projects workspace)))

(defun treemacs-declarative-workspaces--workspaces-by-name (name)
  "Return first workspace in desired state named NAME, or nil."
  (let* ((index (cl-position name
                             treemacs-declarative-workspaces--desired-state
                             :test #'equal
                             :key (lambda (ws) (treemacs-workspace->name ws)))))
    (when index
      (nth index treemacs-declarative-workspaces--desired-state))))

(defun treemacs-declarative-workspaces--save-cache ()
  "Write current desired state to cache file so it can be `load'ed."
  (with-temp-file treemacs-declarative-workspaces--cache-file
    (let ((buf (current-buffer)))
      (prin1 `(setq
               treemacs-declarative-workspaces--desired-state
               ',treemacs-declarative-workspaces--desired-state)
             buf))))

(defun treemacs-declarative-workspaces--read-cache ()
  "Write current desired state to cache file."
  (with-temp-buffer
    (unless (file-exists-p treemacs-declarative-workspaces--cache-file)
      (treemacs-declarative-workspaces--save-cache))
    (insert-file-contents treemacs-declarative-workspaces--cache-file)
    (let ((cached-state (buffer-string)))
      (cond
       ((not (eq cached-state ""))
        ;; TODO this is not secure, but we're in emacs
        (load treemacs-declarative-workspaces--cache-file))
       (t
        (treemacs-declarative-workspaces--minimal-desired-state))))
    (kill-buffer (current-buffer))))

(defun treemacs-declarative-workspaces--unassign-project (project workspace)
  "Add PROJECT to WORKSPACE in desired state."
  (interactive)
  (message "Removing project:\t%s" project)
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

(defun treemacs-declarative-workspaces--prune-invalid-projects ()
  "Remove invalid projects from cached declared workspaces."
  (dolist (workspace treemacs-declarative-workspaces--desired-state)
    (setf (treemacs-workspace->projects workspace)
          (seq-filter (lambda (project)
                        (file-exists-p (treemacs-project->path project)))
                      (treemacs-workspace->projects workspace)))))

(defun treemacs-declarative-workspaces--append-project (workspace project)
  "Append PROJECT to the `projects` slot of WORKSPACE struct and update desired state."
  (let* ((index (cl-position workspace treemacs-declarative-workspaces--desired-state :test #'equal))
         (new-workspace (copy-sequence workspace))
         (projects (treemacs-workspace->projects new-workspace))
         (new-projects (cl-pushnew project projects :test #'equal :key (lambda (pj) (treemacs-project->name pj)))))
    (setf (treemacs-workspace->projects new-workspace)  new-projects)
    (setf (nth index treemacs-declarative-workspaces--desired-state) new-workspace)))

(defun treemacs-declarative-workspaces--assign-project (project-attrs workspace)
  "Add new project with PROJECT-ATTRS to WORKSPACE in desired state."
  (interactive)
  (let ((target-workspace (treemacs-declarative-workspaces--workspaces-by-name workspace)))
    (cond
     ((treemacs-workspace-p target-workspace)
      (let ((project (apply 'treemacs-project->create! project-attrs)))
        (if (not (treemacs-declarative-workspaces--workspace-memberp project target-workspace))
                (treemacs-declarative-workspaces--append-project target-workspace project))))
     (t  ; Workspace didn't exist, create it along with new project
      (cl-pushnew (treemacs-workspace->create!
                   :name workspace
                   :projects (list (apply 'treemacs-project->create!
                                          project-attrs)))
                  treemacs-declarative-workspaces--desired-state
                  :test #'equal))))
  (when treemacs-declarative-workspaces-mode
    (treemacs-declarative-workspaces--override-workspaces))
  (treemacs-declarative-workspaces--save-cache))

(defun treemacs-declarative-workspaces--assign-declared-project (project-resources)
  "Assign a project with PROJECT-RESOURCES when it's declared."
  (when treemacs-declarative-workspaces-mode
    (when-let ((project-workspaces (declarative-project-workspaces project-resources))
               (root-directory (declarative-project-root-directory project-resources)))
      (seq-doseq (workspace project-workspaces)
        (let* ((project-name (or (declarative-project-name project-resources) workspace))
               (project-attrs (list
                               :name project-name
                               :path root-directory
                               :path-status 'local-readable
                               :is-disabled? nil)))
          (treemacs-declarative-workspaces--assign-project project-attrs workspace))))))

(defun treemacs-declarative-workspaces--override-workspaces ()
  "Set treemacs-workspaces to desired state."
  (setq treemacs--workspaces treemacs-declarative-workspaces--desired-state))


;;;###autoload
(define-minor-mode treemacs-declarative-workspaces-mode
  "Manage treemacs workspaces via distrubuted declarative files.

This package allows users to place `PROJECT.yaml' files anywhere across
the fileystem, and install them to treemacs workspaces via the
`declarative-project-mode' package. The desired state of workspaces
is then tracked via a central cache inside `user-emacs-directory'.
By default invalid projects are removed when the mode initializes,
set `treemacs-declarative-workspaces--autoprune' to nil to
prevent such behavior.

Also note that this package currently hijacks the treemacs project
workflow, and is still highly experimental."
  :init-value nil
  :global t
  :group 'treemacs
  :lighter "treemacs-declarative-workspaces-mode"
  (if treemacs-declarative-workspaces-mode
      (progn
        (treemacs-declarative-workspaces--read-cache)
        (when treemacs-declarative-workspaces--autoprune
          (treemacs-declarative-workspaces--prune-invalid-projects))
        (treemacs-declarative-workspaces--override-workspaces)
        (advice-add 'treemacs-add-and-display-current-project
                    :around #'(lambda (&rest _) (treemacs--init)) )
        (add-hook 'declarative-project--apply-treemacs-workspaces-hook
                  #'treemacs-declarative-workspaces--assign-declared-project)
        (add-hook 'treemacs-switch-workspace-hook
                  #'treemacs-declarative-workspaces--override-workspaces))
    (progn
      (treemacs-declarative-workspaces--save-cache)
      (advice-remove 'treemacs-add-and-display-current-project #'(lambda (&rest _) (treemacs--init)) )
      (remove-hook 'declarative-project--apply-treemacs-workspaces-hook
                   #'treemacs-declarative-workspaces--assign-declared-project)
      (remove-hook 'treemacs-switch-workspace-hook
                   #'treemacs-declarative-workspaces--override-workspaces))))

(provide 'treemacs-declarative-workspaces-mode)
;;; treemacs-declarative-workspaces-mode.el ends here
