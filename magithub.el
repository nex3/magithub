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
(require 'json)
(eval-when-compile (require 'cl))


;;; Variables

(defvar magithub-api-base "https://github.com/api/v2/json/"
  "The base URL for accessing the GitHub API.")

(defvar magithub-github-url "https://github.com/"
  "The URL for the main GitHub site.

This is used for some calls that aren't supported by the official API.")

(defvar magithub-use-ssl nil
  "If non-nil, access GitHub via HTTPS.
This is more secure, but slower.")

(defvar magithub-gist-url "http://gist.github.com/"
  "The URL for the Gist site.")

(defvar magithub-view-gist t
  "Whether or not to open new Gists in the browser.")

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

(defvar -magithub-repo-obj-cache (make-hash-table :test 'equal)
  "A hash from (USERNAME . REPONAME) to decoded JSON repo objects (plists).
This caches the result of `magithub-repo-obj' and
`magithub-cached-repo-obj'.")


;;; Utilities

(defun -magithub-remove-if (predicate seq)
  "Remove all items satisfying PREDICATE from SEQ.
Like `remove-if', but without the cl runtime dependency."
  (loop for el being the elements of seq
        if (not (funcall predicate el)) collect el into els
        finally return els))

(defun -magithub-cache-function (fn)
  "Return a lambda that will run FN but cache its return values.
The cache is a very naive assoc from arguments to returns.
The cache will only last as long as the lambda does.

FN may call -magithub-use-cache, which will use a pre-cached
value if available or recursively call FN if not."
  (lexical-let ((fn fn) cache cache-fn)
    (setq cache-fn
          (lambda (&rest args)
            (let ((cached (assoc args cache)))
              (if cached (cdr cached)
                (flet ((-magithub-use-cache (&rest args) (apply cache-fn args)))
                  (let ((val (apply fn args)))
                    (push (cons args val) cache)
                    val))))))))

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

(defun -magithub-lazy-completion-callback (fn &optional noarg)
  "Converts a simple string-listing FN into a lazy-loading completion callback.
FN should take a string (the contents of the minibuffer) and
return a list of strings (the candidates for completion).  This
method takes care of any caching and makes sure FN isn't called
until completion needs to happen.

If NOARG is non-nil, don't pass a string to FN."
  (lexical-let ((fn (-magithub-cache-function fn)) (noarg noarg))
    (lambda (string predicate allp)
      (let ((strs (if noarg (funcall fn) (funcall fn string))))
        (if allp (all-completions string strs predicate)
          (try-completion string strs predicate))))))

(defun magithub-read-user (&optional prompt predicate require-match initial-input
                                     hist def inherit-input-method)
  "Read a GitHub username from the minibuffer with completion.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub user: \".  HIST defaults to
'magithub-users-history.

WARNING: This function currently doesn't work fully, since
GitHub's user search API only returns an apparently random subset
of users."
  (setq hist (or hist 'magithub-users-history))
  (completing-read (or prompt "GitHub user: ")
                   (-magithub-lazy-completion-callback
                    (lambda (s)
                      (mapcar (lambda (user) (plist-get user :name))
                              (magithub-user-search s))))
                   predicate require-match initial-input hist def inherit-input-method))

(defun magithub-read-repo-for-user (user &optional prompt predicate require-match
                                         initial-input hist def inherit-input-method)
  "Read a GitHub repository from the minibuffer with completion.
USER is the owner of the repository.

PROMPT, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD work as in `completing-read'.  PROMPT
defaults to \"GitHub repo: <user>/\"."
  (lexical-let ((user user))
    (completing-read (or prompt (concat "GitHub repo: " user "/"))
                     (-magithub-lazy-completion-callback
                      (lambda ()
                        (mapcar (lambda (repo) (plist-get repo :name))
                                (magithub-repos-for-user user)))
                      'noarg)
                     predicate require-match initial-input hist def
                     inherit-input-method)))

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
  (let ((result (completing-read
                 (or prompt "GitHub repo (user/repo): ")
                 (-magithub-lazy-completion-callback '-magithub-repo-completions)
                 predicate require-match initial-input hist def inherit-input-method)))
    (if (string= result "")
        (when require-match (error "No repository given"))
      (magithub-parse-repo result))))

(defun -magithub-repo-completions (string)
  "Try completing the given GitHub user/repository pair.
STRING is the text already in the minibuffer, PREDICATE is a
predicate that the string must satisfy."
  (destructuring-bind (username . rest) (split-string string "/")
    (if (not rest) ;; Need to complete username before we start completing repo
        (mapcar (lambda (user) (concat (plist-get user :name) "/"))
                (magithub-user-search username))
      (if (not (string= (car rest) ""))
          (-magithub-use-cache (concat username "/"))
        (mapcar (lambda (repo) (concat username "/" (plist-get repo :name)))
                (magithub-repos-for-user username))))))

(defun magithub-read-pull-request-recipients ()
  "Read a list of recipients for a GitHub pull request."
  (let ((collabs (magithub-repo-parent-collaborators))
        (network (magithub-repo-network)))
    (-magithub-remove-if
     (lambda (s) (string= s ""))
     (completing-read-multiple
      "Send pull request to: "
      (mapcar (lambda (repo) (plist-get repo :owner)) (magithub-repo-network))
      nil nil (concat (mapconcat 'identity collabs crm-separator)
                      (if (= (length collabs) (length network)) "" crm-separator))))))

(defun magithub-read-untracked-fork ()
  "Read the name of a fork of this repo that we aren't yet tracking.
This will accept either a username or a username/repo pair,
and return (USERNAME . REPONAME)."
  (let ((fork
         (completing-read
          "Track fork (user or user/repo): "
          (-magithub-lazy-completion-callback
           (lambda ()
             (mapcar (lambda (repo) (concat (plist-get repo :owner) "/"
                                       (plist-get repo :name)))
                     (magithub-untracked-forks)))
           'noarg)
          nil nil nil 'magithub-repos-history)))
    (cond
     ((string= fork "") (error "No fork given"))
     ((string-match "/" fork) (magithub-parse-repo fork))
     (t (cons fork (magithub-repo-name))))))


;;; Bindings

(define-prefix-command 'magithub-prefix 'magithub-map)
(define-key magithub-map (kbd "C") 'magithub-create-from-local)
(define-key magithub-map (kbd "c") 'magithub-clone)
(define-key magithub-map (kbd "f") 'magithub-fork-current)
(define-key magithub-map (kbd "p") 'magithub-pull-request)
(define-key magithub-map (kbd "t") 'magithub-track)
(define-key magithub-map (kbd "g") 'magithub-gist-repo)
(define-key magit-mode-map (kbd "'") 'magithub-prefix)


;;; Requests

(defun magit-request-url (path)
  "Return the full GitHub URL for the resource PATH.

PATH can either be a string or a list of strings.
In the latter case, they're URL-escaped and joined with \"/\".

If `url-request-method' is GET, the returned URL will include
`url-request-data' as the query string."
  (let ((url
         (concat magithub-api-base
                 (if (stringp path) path (mapconcat 'url-hexify-string path "/"))
                 (if (string= url-request-method "GET")
                     (concat "?" url-request-data)
                   ""))))
    (if magithub-use-ssl url
      (replace-regexp-in-string "^https" "http" url))))

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
                               (if (not magithub-parse-response)
                                   (current-buffer)
                                 (let* ((json-object-type 'plist)
                                        (obj (json-read)))
                                   (kill-buffer)
                                   obj))
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
        (if (not magithub-parse-response) (current-buffer)
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
  (if (string= user "") []
    (let ((url-request-method "GET"))
      (plist-get
       (magithub-retrieve-synchronously
        (list "user" "search" string))
       :users))))

(defun magithub-repo-obj (&optional username repo)
  "Return an object representing the repo USERNAME/REPO.
Defaults to the current repo.

The returned object is a decoded JSON object (plist)."
  (setq username (or username (magithub-repo-owner)))
  (setq repo (or repo (magithub-repo-name)))
  (remhash (cons username repo) -magithub-repo-obj-cache)
  (magithub-cached-repo-obj username repo))

(defun magithub-cached-repo-obj (&optional username repo)
  "Return a (possibly cached) object representing the repo USERNAME/REPO.
Defaults to the current repo.

The returned object is a decoded JSON object (plist).

This differs from `magithub-repo-obj' in that it returns a cached
copy of the repo object if one exists.  This is useful for
properties such as :parent and :fork that are highly unlikely to
change."
  (setq username (or username (magithub-repo-owner)))
  (setq repo (or repo (magithub-repo-name)))
  (let ((cached (gethash (cons username repo) -magithub-repo-obj-cache)))
    (or cached
        (let* ((url-request-method "GET")
               (obj (plist-get
                     (magithub-retrieve-synchronously
                      (list "repos" "show" username repo))
                     :repository)))
          (puthash (cons username repo) obj -magithub-repo-obj-cache)
          obj))))

(defun magithub-repo-collaborators (&optional username repo)
  "Return an array of names of collaborators on USERNAME/REPO.
Defaults to the current repo."
  (setq username (or username (magithub-repo-owner)))
  (setq repo (or repo (magithub-repo-name)))
  (let ((url-request-method "GET"))
    (plist-get
     (magithub-retrieve-synchronously
      (list "repos" "show" username repo "collaborators"))
     :collaborators)))

(defun magithub-repo-network (&optional username repo)
  "Return an array of forks and/or parents of USERNAME/REPO.
Defaults to the current repo.

Each fork is a decoded JSON object (plist)."
  (setq username (or username (magithub-repo-owner)))
  (setq repo (or repo (magithub-repo-name)))
  (let ((url-request-method "GET"))
    (plist-get
     (magithub-retrieve-synchronously
      (list "repos" "show" username repo "network"))
     :network)))

(defun magithub-repo-parent-collaborators (&optional username repo)
  "Return an array of names of collaborators on the parent of USERNAME/REPO.
These are the default recipients of a pull request for this repo.
Defaults to the current repo.

If this repo has no parents, return the collaborators for it instead."
  (let ((parent (plist-get (magithub-cached-repo-obj username repo) :parent)))
    (if (not parent) (magithub-repo-collaborators username repo)
      (destructuring-bind (parent-owner . parent-repo) (magithub-parse-repo parent)
        (magithub-repo-collaborators parent-owner parent-repo)))))

(defun magithub-untracked-forks ()
  "Return a list of forks of this repo that aren't being tracked as remotes.
Returned repos are decoded JSON objects (plists)."
  (lexical-let ((remotes (magit-git-lines "remote")))
    (delq "origin" remotes)
    (push (magithub-repo-owner) remotes)
    (-magithub-remove-if
     (lambda (repo) (member-ignore-case (plist-get repo :owner) remotes))
     (magithub-repo-network))))

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


;;; Network

(defun magithub-track (username &optional repo fetch)
  "Track USERNAME/REPO as a remote.
If FETCH is non-nil, fetch that remote.

Interactively, prompts for the username and repo.  With a prefix
arg, fetches the remote."
  (interactive
   (destructuring-bind (username . repo) (magithub-read-untracked-fork)
     (list username repo current-prefix-arg)))
  (magit-run-git "remote" "add" username
                 (format "http://github.com/%s/%s.git" username repo))
  (when fetch (magit-run-git-async "remote" "update" username))
  (message "Tracking %s/%s%s" username repo
           (if fetch ", fetching..." "")))


;;; Creating Repos

(defun magithub-gist-repo (&optional private)
  "Upload the current repo as a Gist.
If PRIVATE is non-nil or with a prefix arg, the Gist is private.

Copies the URL of the Gist into the kill ring.  If
`magithub-view-gist' is non-nil (the default), opens the gist in
the browser with `browse-url'."
  (interactive "P")
  (let ((url-max-redirections 0)
        (url-request-method "POST")
        (magithub-api-base magithub-gist-url)
        (magithub-request-data
         `(,@(if private '(("private" . "1")))
           ("file_ext[gistfile1]" . ".dummy")
           ("file_name[gistfile1]" . "dummy")
           ("file_contents[gistfile1]" .
            "Dummy Gist created by Magithub. To be replaced with a real repo.")))
        magithub-parse-response)
    (let (url)
      (with-current-buffer (magithub-retrieve-synchronously "gists")
        (goto-char (point-min))
        (re-search-forward "^Location: \\(.*\\)$")
        (setq url (match-string 1))
        (kill-buffer))
      (kill-new url)
      (let ((ssh-url (replace-regexp-in-string
                      "^http://gist\\.github\\.com/"
                      "git@gist.github.com:" url)))
        (magit-run-git "remote" "add" "origin" ssh-url)
        (magit-set "origin" "branch" "master" "remote")
        (magit-set "refs/heads/master" "branch" "master" "merge")
        (magit-run-git-async "push" "-v" "-f" "origin" "master")
        (when magithub-view-gist (browse-url url))
        (message "Gist created: %s" url)))))

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
    (let ((url-request-method "POST"))
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
                             "pull_request" (magit-name-rev "HEAD"))
                       (lambda (_)
                         (kill-buffer)
                         (message "Your pull request was sent.")))))

(defun magithub-pull-request (recipients)
  "Compose a pull request and send it to RECIPIENTS.
RECIPIENTS should be a list of usernames.

Interactively, reads RECIPIENTS via `magithub-read-pull-request-recipients'.
For non-interactive pull requests, see `magithub-send-pull-request'."
  (interactive (list (magithub-read-pull-request-recipients)))
  (with-magithub-message-mode
    (magit-log-edit-set-field
     'recipients (mapconcat 'identity recipients crm-separator)))
  (magithub-pop-to-message "send pull request"))


;;; Message Mode

(defvar magithub-message-mode-hook nil "Hook run by `magithub-message-mode'.")

(defvar magithub-message-confirm-cancellation magit-log-edit-confirm-cancellation
  "If non-nil, confirm when cancelling the editing of a `magithub-message-mode' buffer.")

(defconst magithub-message-buffer-name "*magithub-edit-message*"
  "Buffer name for composing messages.")

(defconst magithub-message-header-end "-- End of Magithub header --\n")

(defvar magithub-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magithub-message-send)
    (define-key map (kbd "C-c C-k") 'magithub-message-cancel)
    (define-key map (kbd "C-c C-]") 'magithub-message-cancel)
    map)
  "The keymap for `magithub-message-mode'.")

(defvar magithub-pre-message-window-configuration nil)

(macrolet
    ((define-it (parent-mode)
       `(define-derived-mode magithub-message-mode ,parent-mode "Magithub Message Edit"
          "A mode for editing pull requests and other GitHub messages."
          (run-mode-hooks 'magithub-message-mode-hook))))
  (if (featurep 'markdown-mode) (define-it markdown-mode)
    (define-it text-mode)))

(defmacro with-magithub-message-mode (&rest body)
  "Runs BODY with Magit's log-edit functions usable with Magithub's message mode."
  (declare (indent 0))
  `(let ((magit-log-edit-buffer-name magithub-message-buffer-name)
         (magit-log-header-end magithub-message-header-end)
         (magit-log-edit-confirm-cancellation
          magithub-message-confirm-cancellation)
         (magit-pre-log-edit-window-configuration
          magithub-pre-message-window-configuration))
     (unwind-protect (progn ,@body)
       (setq magithub-pre-message-window-configuration
             magit-pre-log-edit-window-configuration))))

(defun magithub-pop-to-message (operation)
  "Open up a `magithub-message-mode' buffer and switch to it.
OPERATION is the name of what will happen when C-c C-c is used,
printed as a message when the buffer is opened."
  (let ((dir default-directory)
	(buf (get-buffer-create magithub-message-buffer-name)))
    (setq magithub-pre-message-window-configuration
	  (current-window-configuration))
    (pop-to-buffer buf)
    (setq default-directory dir)
    (magithub-message-mode)
    (message "Type C-c C-c to %s (C-c C-k to cancel)." operation)))

(defun magithub-message-send ()
  "Finish writing the message and send it."
  (interactive)
  (let ((recipients (with-magithub-message-mode
                      (magit-log-edit-get-field 'recipients))))
    (with-magithub-message-mode (magit-log-edit-set-fields nil))
    (magithub-send-pull-request
     (buffer-string) (split-string recipients crm-separator))
    (let (magithub-message-confirm-cancellation)
      (magithub-message-cancel))))

(defun magithub-message-cancel ()
  "Abort and erase message being composed."
  (interactive)
  (with-magithub-message-mode (magit-log-edit-cancel-log-message)))


;;; Hooks into Magit

(defadvice magit-init (after magithub-init-too (dir) activate)
  (when (y-or-n-p "Create GitHub repo? ")
    (let ((default-directory dir))
      (call-interactively 'magithub-create-from-local))))

(provide 'magithub)

;;;###autoload
(eval-after-load 'magit
  (unless (featurep 'magithub)
    (require 'magithub)))

;;; magithub.el ends here
