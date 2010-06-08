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
(eval-when-compile (require 'cl))


;;; Variables

(defvar magithub-api-base "https://github.com/api/v2/json/"
  "The base URL for accessing the GitHub API.")

(defvar magithub-request-data nil
  "An assoc list of parameter names to values.

This is meant to be dynamically bound around `magithub-retrieve'
and `magithub-retrieve-synchronously'.")

(defvar -magithub-users-cache nil
  "An assoc list of username prefixes to users matching those prefixes.

Each entry is of the form (PREFIX COMPLETE . USERS).  PREFIX is
the string prefix of all users; COMPLETE is whether or not all
users with that prefix are in USERS (since GitHub doesn't always
return all users); and USERS is an array of decoded JSON
responses from the GitHub API (plists).

Longer prefixes should appear earlier in the list.

This cache is only maintained within a single call to
`magithub-read-user'.")

(defconst magithub-max-users-for-search 30
  "The maximum number of users GitHub will return for a user search.")


;;; Utilities

(defun -magithub-remove-if (predicate seq)
  "Remove all items satisfying PREDICATE from SEQ.
Like `remove-if', but without the cl runtime dependency."
  (loop for el being the elements of seq
        if (not (funcall predicate el)) collect el into els
        finally return els))

(defun magithub-make-query-string (params)
  "Return a query string constructed from PARAMS.
PARAMS is an assoc list of parameter names to values.

Any parameters with a nil values are ignored."
  (replace-regexp-in-string
   "&+" "&"
   (mapconcat
    (lambda (param)
      (when (cdr param)
        (concat (url-hexify-string (car param)) "="
                (url-hexify-string (cdr param)))))
    params "&")))

(defun magithub-read-user (&optional prompt)
  "Read a GitHub username from the minibuffer with completion.
PROMPT is a string to prompt with, defaulting to \"GitHub user: \".

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns 30 (apparently random)
users, and also has no way to search for users whose names begin
with certain characters."
  (let ((-magithub-users-cache nil))
    (completing-read (or prompt "GitHub user: ") '-magithub-complete-user)))

(defun -magithub-complete-user (string predicate allp)
  "Try completing the given GitHub username.
STRING is the text already in the minibuffer, PREDICATE is a
predicate that the string must satisfy."
  (let ((usernames (mapcar (lambda (user) (plist-get user :name))
                           (-magithub-users-for-prefix string))))
    (if allp (all-completions string usernames predicate)
      (try-completion string usernames predicate))))

(defun -magithub-users-for-prefix (prefix)
  "Return all GitHub users whose names begin with PREFIX.

Users are returned as decoded JSON objects (plists) in an array.
Caches the results in `-magithub-users-cache', which should be
let-bound around a call to this function.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns 30 (apparently random)
users, and also has no way to search for users whose names begin
with certain characters."
  (flet ((with-prefix (users pfx)
           (lexical-let ((pfx pfx))
             (-magithub-remove-if
              (lambda (user) (not (string-prefix-p pfx (plist-get user :name) 'ignore-case)))
              users))))
    (let ((users (assoc* prefix -magithub-users-cache
                         :test (lambda (s1 s2) (string-prefix-p s2 s1 'ignore-case)))))
      (cond
       ;; We've cached the users for this prefix (or a prefix of it)
       ((and users (string= (car users) prefix)) (cddr users))
       ;; We've cached the users for a prefix of this prefix,
       ;; and we won't get more users from another GitHub call
       ((and users (cadr users)) (with-prefix (cddr users) prefix))
       ;; We need to run a GitHub call to get more users
       (t
        (let* ((url-request-method "GET")
               (users (plist-get
                       (magithub-retrieve-synchronously
                        (concat "user/search/" (url-hexify-string string)))
                       :users))
               (matching-users (with-prefix users prefix)))
          ;; If the length is less than the max, then this is all users
          ;; with this substring in their usernames,
          ;; so we don't need to do more GitHub searches
          (push (list* prefix (< (length users) magithub-max-users-for-search)
                       matching-users)
                -magithub-users-cache)
          matching-users))))))


;;; Bindings

(define-prefix-command 'magithub-prefix 'magithub-map)
(define-key magithub-map (kbd "C") 'magithub-create-from-local)
(define-key magit-mode-map (kbd "'") 'magithub-prefix)


;;; Requests

(defun magit-request-url (path)
  "Return the full GitHub URL for the resource PATH.

If `url-request-method' is GET, the returned URL will include
`url-request-data' as the query string."
  (concat magithub-api-base path
          (if (string= url-request-method "GET")
              (concat "?" url-request-data)
            "")))

(defmacro magithub-with-auth (&rest body)
  "Runs BODY with GitHub authorization info in `magithub-request-data'."
  (declare (indent 0))
  (let ((auth (gensym)))
    `(let* ((,auth (magithub-auth-info))
            (magithub-request-data (append (list
                                            (cons "login" (car ,auth))
                                            (cons "token" (cdr ,auth)))
                                           magithub-request-data)))
       ,@body)))

(defun magithub-handle-errors (status)
  "Handle any errors reported in a `url-retrieve' callback.
STATUS is the first argument passed to the callback.

If there is an error and GitHub returns an error message, that
message is printed with `error'.  Otherwise, the HTTP error is
signaled."
  (loop for (name val) on status by 'cddr
        do (when (eq name :error)
             (condition-case err
                 (let* ((json-object-type 'plist)
                        (data (json-read))
                        (err (plist-get data :error)))
                   (unless err (signal 'json-readtable-error nil))
                   (error "GitHub error: %s" err))
               (json-readtable-error (signal (car val) (cdr val)))))))

(defun magithub-retrieve (path callback &optional cbargs)
  "Retrieve GitHub API PATH asynchronously.
Call CALLBACK with CBARGS when finished.

Like `url-retrieve', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `magithub-request-data' is used instead of `url-request-data'.
* CALLBACK is passed a decoded JSON object (as a plist) rather
  than a list of statuses.  Basic error handling is done by `magithub-retrieve'."
  (magithub-with-auth
    (let ((url-request-data (magithub-make-query-string magithub-request-data)))
      (url-retrieve (magit-request-url path)
                    (lambda (status callback &rest cbargs)
                      (search-forward "\n\n" nil t) ; Move past headers
                      (magithub-handle-errors status)
                      (apply callback (let ((json-object-type 'plist)) (json-read)) cbargs))
                    (cons callback cbargs)))))

(defun magithub-retrieve-synchronously (path)
  "Retrieve GitHub API PATH synchronously.

Like `url-retrieve-synchronously', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `magithub-request-data' is used instead of `url-request-data'.
* Returns a decoded JSON object (as a plist) rather than a buffer
  containing the response."
  (magithub-with-auth
    (let ((url-request-data (magithub-make-query-string magithub-request-data)))
      (with-current-buffer (url-retrieve-synchronously (magit-request-url path))
        (goto-char (point-min))
        (search-forward "\n\n" nil t) ; Move past headers
        (let* ((data (let ((json-object-type 'plist)) (json-read)))
               (err (plist-get data :error)))
          (when err (error "GitHub error: %s" err))
          (kill-buffer)
          data)))))


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


;;; Creating Repos

(defun magithub-create-from-local (name &optional description homepage private)
  "Create a new GitHub repository for the current Git repository.
NAME is the name of the GitHub repository, DESCRIPTION describes
the repository, URL is the location of the homepage.  If PRIVATE
is non-nil, a private repo is created.

When called interactively, prompts for NAME, DESCRIPTION, and
HOMEPAGE.  NAME defaults to the name of the current Git
directory.  By default, creates a public repo; with a prefix arg,
creates a private repo."
  (interactive
   (list (read-string "Repository name: "
                      (file-name-nondirectory
                       (directory-file-name
                        (expand-file-name
                         (magit-get-top-dir default-directory)))))
         (read-string "Description: ")
         (read-string "Homepage: ")
         current-prefix-arg))

  (let ((url-request-method "POST")
        (magithub-request-data `(("name" . ,name)
                                 ("description" . ,description)
                                 ("homepage" . ,homepage)
                                 ("private" . ,(if private "0" "1")))))
    (magithub-retrieve "repos/create"
                       (lambda (data name)
                         (magit-git-string
                          "remote" "add" "origin"
                          (concat "git@github.com:" (magithub-config "user")
                                  "/" name ".git"))
                         (magit-set "origin" "branch" "master" "remote")
                         (magit-set "refs/heads/master" "branch" "master" "merge")
                         (magit-run-git-async "push" "-v" "origin" "master")
                         (message "GitHub repository created: %s"
                                  (plist-get (plist-get data :repository) :url)))
                       (list name))))


(provide 'magithub)

;;; magithub.el ends here
