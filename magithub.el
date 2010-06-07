;;; magithub.el --- Magit extensions for using GitHub

;; Copyright (c) 2010 Nathan Weizenbaum
;; Licensed under the same terms as Emacs.

;; Author: Nathan Weizenbaum
;; URL: http://github.com/nex3/magithub
;; Version: 0.1
;; Created: 2010-06-06
;; By: Nathan Weizenbaum
;; Keywords: git, github, magit
;; Package-Requires: ((magit "0.8") (json "1.2"))

;;; Commentary:
;; This package does two things.  First, it extends Magit's UI with
;; assorted GitHub-related functionality, similar to the github-gem
;; tool (http://github.com/defunkt/github-gem).  Second, Magithub uses
;; Magit's excellent Git library to build an Elisp library for
;; interfacing with GitHub's API.

(require 'magit)
(require 'url)

(defvar magithub-api-base "https://github.com/api/v2/json/"
  "The base URL for accessing the GitHub API.")

(defvar magithub-request-data nil
  "An assoc list of parameter names to values.

This is meant to be dynamicall bound around `magithub-retrieve-synchronously'.")

;;; Utilities

(defun magithub-make-query-string (params)
  "Return a query string constructed from PARAMS.
PARAMS is an assoc list of parameter names to values."
  (mapconcat
   (lambda (param)
     (concat (url-hexify-string (car param)) "="
             (url-hexify-string (cdr param))))
   params "&"))

(defun magithub-retrieve-synchronously (path)
  "Retrieve GitHub API PATH synchronously.

Like `url-retrieve-synchronously', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `magithub-request-data' is used instead of `url-request-data'."
  (let* ((auth (magithub-auth-info))
         (magithub-request-data (append `(("login" . ,(car auth))
                                          ("token" . ,(cdr auth)))
                                        magithub-request-data))
         (url-request-data (magithub-make-query-string magithub-request-data)))
    (url-retrieve-synchronously (concat magithub-api-base path
                                        (if (string= url-request-method "GET")
                                            (concat "?" url-request-data)
                                          "")))))


;;; Configuration
;; This API was taken from gist.el (http://github.com/defunkt/gist.el),
;; and renamed to avoid conflict.  The code also uses Magit rather
;; than relying on the Git executable directly.

(defun magithub-config (key)
  "Returns a GitHub specific value from the global Git config."
  (magit-git-string "config" "--global" (concat "github." key)))

(defun magithub-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (magit-git-string "config" "--global" (concat "github." key) value))

(defun magithub-auth-info ()
  "Returns the user's GitHub authorization information.
Searches for a GitHub username and token in the global git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)

  (let* ((user (magithub-config "user"))
         (token (magithub-config "token")))

    (when (not user)
      (setq user (read-string "GitHub username: "))
      (magithub-set-config "user" user))

    (when (not token)
      (setq token (read-string "GitHub API token: "))
      (magithub-set-config "token" token))

    (cons user token)))


(provide 'magithub)

;;; magithub.el ends here
