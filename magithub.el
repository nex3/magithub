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

;;;###autoload
(eval-after-load 'magit
  (unless (featurep 'magithub)
    (require 'magithub)))


;;; Variables

(defvar magithub-api-base "https://github.com/api/v2/json/"
  "The base URL for accessing the GitHub API.")

(defvar magithub-github-url "https://github.com/"
  "The URL for the main GitHub site.

This is used for some calls that aren't supported by the official API.")

(defvar magithub-request-data nil
  "An assoc list of parameter names to values.

This is meant to be dynamically bound around `magithub-retrieve'
and `magithub-retrieve-synchronously'.")

(defvar magithub-parse-response t
  "Whether to parse responses from GitHub as JSON.
Used by `magithub-retrieve' and `magithub-retrieve-synchronously'.
This should only ever be `let'-bound, not set outright.")

(defvar magithub-users-history nil
  "A list of users selected via `magithub-read-user'.")

(defvar magithub-repos-history nil
  "A list of repos selected via `magithub-read-repo'.")

(defvar -magithub-users-cache nil
  "An assoc list of username prefixes to users matching those prefixes.

Each entry is of the form (PREFIX . USERS).  PREFIX is the string
prefix of all users, and USERS is an array of decoded JSON
responses from the GitHub API (plists).

This cache is only maintained within a single call to
`magithub-read-user'.")

(defvar -magithub-repos-cache nil
  "An assoc list from usernames to repos owned by those users.

Each entry is of the form (USERNAME . REPOS), where REPOS is an
array containing decoded JSON responses from the GitHub
API (plists).

This cache is only maintained within a single call to
`magithub-read-repo'.")


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

(defun magithub-parse-repo (repo)
  "Parse a REPO string of the form \"username/repo\".
Return (USERNAME . REPO), or raise an error if the format is
incorrect."
  (condition-case err
      (destructuring-bind (username repo) (split-string repo "/")
        (cons username repo))
    (wrong-number-of-arguments (error "Invalid GitHub repository %s" repo))))


;;; Reading Input

(defun magithub-read-user (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub username from the minibuffer with completion.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub user: \".  HIST defaults to
'magithub-users-history.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users, and also has no way to search for users whose names
begin with certain characters."
  (setq hist (or hist 'magithub-users-history))
  (let ((-magithub-users-cache nil))
    (completing-read (or prompt "GitHub user: ") '-magithub-complete-user predicate
                     require-match initial-input hist def inherit-input-method)))

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
GitHub's user search API only returns an apparently random subset
of users, and also has no way to search for users whose names
begin with certain characters."
  (if (string= string "")
      (if allp (all-completions "" '() predicate)
        (try-completion "" '() predicate))
    (flet ((with-prefix (users pfx)
             (lexical-let ((pfx pfx))
               (-magithub-remove-if
                (lambda (user) (not (string-prefix-p pfx (plist-get user :name) 'ignore-case)))
                users))))
      (let ((users (assoc prefix -magithub-users-cache)))
        (if users (cddr users) ;; We've cached the users for this prefix
          ;; We need to run a GitHub call to get more users
          (let* ((url-request-method "GET")
                 (users (magithub-user-search string))
                 (matching-users (with-prefix users prefix)))
            ;; If the length is less than the max, then this is all users
            ;; with this substring in their usernames,
            ;; so we don't need to do more GitHub searches
            (push (cons prefix matching-users) -magithub-users-cache)
            matching-users))))))

(defun magithub-read-repo-for-user (user &optional prompt predicate require-match
                                         initial-input hist def inherit-input-method)
  "Read a GitHub repository from the minibuffer with completion.
USER is the owner of the repository.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo: <user>/\"."
  (let ((-magithub-repos-cache nil))
    (lexical-let ((user user))
      (completing-read (or prompt (concat "GitHub repo: " user "/"))
                       (lambda (&rest args)
                         (apply '-magithub-complete-repo-for-user user args))
                       predicate require-match initial-input hist def
                       inherit-input-method))))

(defun -magithub-complete-repo-for-user (user string predicate allp)
  "Try completing the given GitHub repository.
USER is the owner of the repository
STRING is the text already in the minibuffer, PREDICATE is a
predicate that the string must satisfy."
  (let ((repos (mapcar (lambda (repo) (plist-get repo :name))
                       (-magithub-repos-for-user user))))
    (if allp (all-completions string repos predicate)
      (try-completion string repos predicate))))

(defun -magithub-repos-for-user (user)
  "Returns a list of all repos owned by USER.
The repos are decoded JSON objects (plists).

This is like `magithub-repos-for-user' except that it uses
`-magithub-repos-cache' and returns a list rather than an array."
  (let ((repos (cdr (assoc user -magithub-repos-cache))))
    (unless repos
      (setq repos (append (magithub-repos-for-user user) nil)) ;; Convert to list
      (push (cons user repos) -magithub-repos-cache))
    repos))

(defun magithub-read-repo (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub user-repository pair with completion.
Return (USERNAME . REPO), or nil if the user enters no input.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo (user/repo): \".  HIST defaults to
'magithub-repos-history.  If REQUIRE-MATCH is non-nil and the
user enters no input, raises an error.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users, and also has no way to search for users whose names
begin with certain characters."
  (setq hist (or hist 'magithub-repos-history))
  (let ((-magithub-users-cache nil)
        (-magithub-repos-cache nil))
    (let ((result (completing-read (or prompt "GitHub repo (user/repo): ")
                                   '-magithub-complete-repo predicate require-match
                                   initial-input hist def inherit-input-method)))
      (if (string= result "")
          (when require-match (error "No repository given"))
        (magithub-parse-repo result)))))

(defun -magithub-complete-repo (string predicate allp)
  "Try completing the given GitHub user/repository pair.
STRING is the text already in the minibuffer, PREDICATE is a
predicate that the string must satisfy."
  (destructuring-bind (username . rest) (split-string string "/")
    (if (not rest) ;; Need to complete username before we start completing repo
        (let ((usernames (mapcar (lambda (user) (concat (plist-get user :name) "/"))
                                 (-magithub-users-for-prefix username))))
          (if allp (all-completions username usernames predicate)
            (try-completion username usernames predicate)))
      (let ((repos (mapcar (lambda (repo) (concat username "/" (plist-get repo :name)))
                           (-magithub-repos-for-user username))))
        (if allp (all-completions string repos predicate)
          (try-completion string repos predicate))))))


;;; Bindings

(define-prefix-command 'magithub-prefix 'magithub-map)
(define-key magithub-map (kbd "C") 'magithub-create-from-local)
(define-key magithub-map (kbd "c") 'magithub-clone)
(define-key magithub-map (kbd "f") 'magithub-fork-current)
(define-key magit-mode-map (kbd "'") 'magithub-prefix)


;;; Requests

(defun magit-request-url (path)
  "Return the full GitHub URL for the resource PATH.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

If `url-request-method' is GET, the returned URL will include
`url-request-data' as the query string."
  (concat magithub-api-base
          (if (stringp path) path (mapconcat 'url-hexify-string path "/"))
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
             (if (not magithub-handle-errors)
                 (signal (var val) (cdr val))
               (condition-case err
                   (let* ((json-object-type 'plist)
                          (data (json-read))
                          (err (plist-get data :error)))
                     (unless err (signal 'json-readtable-error nil))
                     (error "GitHub error: %s" err))
                 (json-readtable-error (signal (car val) (cdr val))))))))

(defun magithub-retrieve (path callback &optional cbargs)
  "Retrieve GitHub API PATH asynchronously.
Call CALLBACK with CBARGS when finished.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

Like `url-retrieve', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `magithub-request-data' is used instead of `url-request-data'.
* CALLBACK is passed a decoded JSON object (as a plist) rather
  than a list of statuses.  Basic error handling is done by `magithub-retrieve'.

If `magithub-parse-response' is nil, CALLBACK is just passed nil
rather than the JSON response object."
  (magithub-with-auth
    (let ((url-request-data (magithub-make-query-string magithub-request-data)))
      (lexical-let ((callback callback) (magithub-parse-response magithub-parse-response))
        (url-retrieve (magit-request-url path)
                      (lambda (status &rest cbargs)
                        (when magithub-parse-response
                          (search-forward "\n\n" nil t)) ; Move past headers
                        (magithub-handle-errors status)
                        (apply callback
                               (when magithub-parse-response
                                 (let ((json-object-type 'plist)) (json-read)))
                               cbargs))
                      cbargs)))))

(defun magithub-retrieve-synchronously (path)
  "Retrieve GitHub API PATH synchronously.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

Like `url-retrieve-synchronously', except for the following:
* PATH is an API resource path, not a full URL.
* GitHub authorization is automatically enabled.
* `magithub-request-data' is used instead of `url-request-data'.
* Return a decoded JSON object (as a plist) rather than a buffer
  containing the response unless `magithub-parse-response' is nil."
  (magithub-with-auth
    (let ((url-request-data (magithub-make-query-string magithub-request-data)))
      (with-current-buffer (url-retrieve-synchronously (magit-request-url path))
        (goto-char (point-min))
        (if (not magithub-parse-response) current-buffer
          (search-forward "\n\n" nil t) ; Move past headers
          (let* ((data (let ((json-object-type 'plist)) (json-read)))
                 (err (plist-get data :error)))
            (when err (error "GitHub error: %s" err))
            (kill-buffer)
            data))))))


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


;;; GitHub Information

(defun magithub-repos-for-user (user)
  "Return an array of all repos owned by USER.
The repos are decoded JSON objects (plists)."
  (let ((url-request-method "GET"))
    (plist-get
     (magithub-retrieve-synchronously
      (list "repos" "show" user))
     :repositories)))

(defun magithub-user-search (user)
  "Run a GitHub user search for USER.
Return an array of all matching users.

WARNING: WARNING: This function currently doesn't work fully,
since GitHub's user search API only returns an apparently random
subset of users."
  (let ((url-request-method "GET"))
    (plist-get
     (magithub-retrieve-synchronously
      (list "user" "search" string))
     :users)))


;;; Local Repo Information

(defun magithub-repo-info ()
  "Return information about this GitHub repo.
This is of the form (USERNAME REPONAME SSH).  USERNAME is the
owner of the repo, REPONAME is the name of the repo, and SSH
is non-nil if it's checked out via SSH.

Error out if this isn't a GitHub repo."
  (or
   (block nil
     (let ((url (magit-get "remote" "origin" "url")))
       (unless url (return))
       (when (string-match "\\(?:git\\|http\\)://github\\.com/\\(.*?\\)/\\(.*\\)\.git" url)
         (return (list (match-string 1 url) (match-string 2 url) nil)))
       (when (string-match "git@github\\.com:\\(.*?\\)/\\(.*\\)\\.git" url)
         (return (list (match-string 1 url) (match-string 2 url) t)))
       (return)))
   (error "Not in a GitHub repo")))

(defun magithub-repo-owner ()
  "Return the name of the owner of this GitHub repo.

Error out if this isn't a GitHub repo."
  (car (magithub-repo-info)))

(defun magithub-repo-name ()
  "Return the name of this GitHub repo.

Error out if this isn't a GitHub repo."
  (cadr (magithub-repo-info)))

(defun magithub-repo-ssh-p ()
  "Return non-nil if this GitHub repo is checked out via SSH.

Error out if this isn't a GitHub repo."
  (caddr (magithub-repo-info)))


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

(defun magithub-clone (username repo dir)
  "Clone GitHub repo USERNAME/REPO into directory DIR.
Once the repo is cloned, switch to a `magit-status' buffer for it.

Interactively, prompts for the repo name and directory."
  (interactive
   (destructuring-bind (username . repo) (magithub-read-repo "Clone repo (user/repo): ")
     (list username repo (read-directory-name "Parent directory: "))))
  ;; The trailing slash is necessary for Magit to be able to figure out
  ;; that this is actually a directory, not a file
  (let ((dir (concat (directory-file-name (expand-file-name dir)) "/" repo "/")))
    (magit-run-git "clone" (concat "http://github.com/" username "/" repo ".git") dir)
    (magit-status dir)))


;;; Forking Repos

(defun magithub-fork-current ()
  "Fork the current repository in place."
  (interactive)
  (destructuring-bind (owner repo _) (magithub-repo-info)
    (let ((url-request-method "GET"))
      (magithub-retrieve (list "repos" "fork" owner repo)
                         (lambda (obj owner repo)
                           (magit-with-refresh
                             (magit-set (concat "git@github.com:" owner "/" repo ".git")
                                        "remote" "origin" "url"))
                           (message "Forked %s/%s" owner repo))
                         (list owner repo)))))

(defun magithub-send-pull-request (text recipients)
  "Send a pull request with text TEXT to RECIPIENTS.
RECIPIENTS should be a list of usernames."
  (let ((url-request-method "POST")
        (magithub-request-data (cons (cons "message[body]" text)
                                     (mapcar (lambda (recipient)
                                               (cons "message[to][]" recipient))
                                             recipients)))
        (magithub-api-base magithub-github-url)
        (url-max-redirections 0) ;; GitHub will try to redirect, but we don't care
        magithub-parse-response)
    (magithub-retrieve (list (magithub-repo-owner) (magithub-repo-name)
                             "pull_request" (magit-name-rev "HEAD^"))
                       (lambda (_) (message "Your pull request was sent.")))))


(provide 'magithub)

;;; magithub.el ends here
