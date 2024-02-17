;;; mdx.el --- Mangadex follows to Org converter     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Visuwesh

;; Author: Visuwesh <visuweshm@gmail.com>
;; Keywords: misc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package fetches the provided user's MangaDex followed list and
;; converts it to an org-mode file for easier curation, local search,
;; among other things.  The non-existent search within the followed
;; list in the web interface is what prompted this package.  This
;; package also fetches the cover image and attaches it in the
;; directory "mdx-data" where the org file resides.  For
;; authorisation, the package uses the "client" API for which API
;; secret is needed.  To get this secret, request client secret in the
;; "API" section under your account settings.  For more details, see
;; https://api.mangadex.org/docs/02-authentication/personal-clients/.

;; The package contains a single entry `mdx' which updates the entires
;; in, or creates the org file.
;; Customise the relevant user options in the custom group 'mdx' by
;; `M-x customize-group RET mdx RET'.  This includes auth settings,
;; and the location of the org file.

;;; Code:
(require 'url)
(require 'org)

;; Ref: https://api.mangadex.org/docs/redoc.html
(defgroup mdx nil
  "Mangadex to org converter."
  :group 'external
  :prefix "mdx-")

(defcustom mdx-filename (expand-file-name "~/doc/org/mdx.org")
  "Absolute filename of the org file to use."
  :type '(file))

(defvar mdx-base-uri "https://api.mangadex.org"
  "The base URI for MangaDex API.")

(defun mdx-api (target)
  "Return the full URL for the target TARGET."
  (concat mdx-base-uri
          (unless (string-prefix-p "/" target) "/")
          target))

(defvar url-http-end-of-headers)
(defun mdx-json-parse-request ()
  "Parse the JSON returned by the request in current-buffer."
  (save-restriction
    (goto-char url-http-end-of-headers)
    (narrow-to-region (point) (point-max))
    (json-parse-buffer :array-type 'list
                       :null-object nil)))

(defun mdx-gethash (ht &rest keys)
  "Traverse the nested hashtable HT as per KEYS."
  (while keys
    (setq ht (gethash (car keys) ht)
          keys (cdr keys)))
  ht)

;; Ref. https://api.mangadex.org/docs/02-authentication/personal-clients/
(defcustom mdx-client-secret-function #'ignore
  "Function with no argument that returns the client secret."
  :type '(function))

(defcustom mdx-client-id-function #'ignore
  "Function with no argument that returns the client id."
  :type '(function))

(defcustom mdx-password-function #'ignore
  "Function with no argument that returns the user password."
  :type '(function))

(defcustom mdx-username ""
  "Username of the mangadex account."
  :type '(string))

(defvar mdx--cached-tokens nil)

(defun mdx-get-tokens (&optional force)
  "Get the tokens for the mangadex user.
If FORCE is non-nil, then force refetching instead of returning
the cached tokens."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (url-encode-url
          (format (concat "grant_type=password&"
                          "username=%s&"
                          "password=%s&"
                          "client_id=%s&"
                          "client_secret=%s")
                  mdx-username
                  (funcall mdx-password-function)
                  (funcall mdx-client-id-function)
                  (funcall mdx-client-secret-function)))))
    (if (or force (null mdx--cached-tokens))
        (with-current-buffer (url-retrieve-synchronously
                              "https://auth.mangadex.org/realms/mangadex/protocol/openid-connect/token")
          (setq mdx--cached-tokens (mdx-json-parse-request)))
      mdx--cached-tokens)))

;; (defun mdx-get-followed-manga ()
;;   "Get all manga followed by user.
;; The secrets are retrieved and renewed if necessary by this
;; function."
;;   (let* ((secret-ht (mdx-get-tokens))
;;          (url-request-method "GET")
;;          (url-request-extra-headers
;;           `(("Authorization" . ,(concat "Bearer " (mdx-gethash secret-ht "access_token")))))
;;          (donep nil)
;;          (limit 100)
;;          (offset 0)
;;          total ht list)
;;     (while (not donep)
;;       (with-current-buffer (url-retrieve-synchronously
;;                             (mdx-api (format "/user/follows/manga?limit=%d&offset=%d"
;;                                                 limit offset)))
;;         (setq ht (mdx-json-parse-request))
;;         (if (equal (gethash "result" ht) "error")
;;             (setq secret-ht (mdx-get-tokens 'force))
;;           (unless total (setq total (gethash "total" ht)))
;;           (setq list (append list (gethash "data" ht))
;;                 offset (+ offset (length (gethash "data" ht)))
;;                 donep (null (gethash "data" ht))))))
;;     ;; Force a re-fetch.
;;     (mdx-get-manga-reading-status "" 'force)
;;     list))

(defvar mdx--manga-reading-statuses nil)

(defun mdx-get-manga-reading-status (id &optional force)
  "Return the reading status of manga with id ID.
If FORCE is non-nil, then force refetching of reading status of
all manga."
  (when (or force (null mdx--manga-reading-statuses))
    (let* ((secret-ht (mdx-get-tokens))
           (url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " (mdx-gethash secret-ht "access_token"))))))
      (with-current-buffer (url-retrieve-synchronously
                            (mdx-api (concat "/manga/status")))
        (let ((ht (mdx-json-parse-request)))
          (if (equal (gethash "result" ht) "error")
              (progn (mdx-get-tokens t)
                     (mdx-get-manga-reading-status id))
            (setq mdx--manga-reading-statuses (gethash "statuses" ht)))))))
  (gethash id mdx--manga-reading-statuses))

;; Whilst using /user/follows/manga should be the obvious solution, it
;; does not actually return all manga.  Even mangadex.org website does
;; the fetch this way.  Incredibly stupid, I know but what can I do?
(defun mdx-get-followed-manga ()
  "Get all manga followed by user.
The secrets are retrieved and renewed if necessary by this
function."
  (let ((status (progn
                  (mdx-get-manga-reading-status "" 'force)
                  mdx--manga-reading-statuses))
        (limit 0)
        (request-post (concat "includes[]=cover_art"
                              "&includes[]=artist"
                              "&includes[]=author"
                              "&contentRating[]=safe"
                              "&contentRating[]=suggestive"
                              "&contentRating[]=erotica"
                              "&contentRating[]=pornographic"
                              "&limit=100"))
        request-str list)
    (maphash
     (lambda (id _)
       (when (= limit 100)
         (with-current-buffer (url-retrieve-synchronously
                               (mdx-api (concat "/manga?"
                                                   request-str
                                                   request-post)))
           (setq list (append list (gethash "data" (mdx-json-parse-request))))
           (setq limit 0 request-str nil)))
       (setq limit (1+ limit)
             request-str (concat "ids[]=" id "&" request-str)))
     status)
    ;; Limit might not be divisibly by 100.
    (when request-str
      (with-current-buffer (url-retrieve-synchronously
                            (mdx-api (concat "/manga?" request-str request-post)))
        (setq list (append list (gethash "data" (mdx-json-parse-request))))))
    list))

(defun mdx-get-manga (id)
  "Return the hashtable for the manga with id ID."
  (let ((url-request-method "GET"))
    (with-current-buffer (url-retrieve-synchronously
                          (mdx-api (concat "/manga/" id
                                              "?includes[]=cover_art"
                                              "&includes[]=artist"
                                              "&includes[]=author")))
      (mdx-gethash (mdx-json-parse-request) "data"))))

(defun mdx-get-titles (ht)
  "Return the titles for the manga from request data HT.
The first element is the title, and the rest are alternate
titles.  Only English titles are included."
  (let* ((title (mdx-gethash ht "attributes" "title"))
         (alt (mdx-gethash ht "attributes" "altTitles"))
         (title-keys (hash-table-keys title))
         ret)
    (if (member "en" title-keys)
        (setq ret (list (gethash "en" title)))
      (if (member "ja" title-keys)
          (setq ret (list (gethash "ja" title)))
        (setq ret (list (gethash (car title-keys) title)))))
    (dolist (a alt)
      (when (member "en" (hash-table-keys a))
        (push (gethash "en" a) ret)))
    (setq ret (nreverse ret))))

(defun mdx-get-description (ht)
  "Return the description of the manga HT.
Returns nil if there are no English description."
  (mdx-gethash ht "attributes" "description" "en"))

(defun mdx-get-demographic (ht)
  "Return the demographic of the manga HT.
Returns the string \"unknown\" if this is unknown."
  (let ((d (mdx-gethash ht "attributes" "publicationDemographic")))
    (or d "unknown")))

(defun mdx--get-artist-or-author-field (ht type field)
  "Return the field FIELD for artist or author of the manga HT.
TYPE is the type of author/artist to fetch."
  (seq-keep
   (lambda (r)
     (when (equal (gethash "type" r) type)
       (let ((ht (if (gethash "attributes" r)
                     (gethash "attributes" r)
                   (let ((url-request-method "GET")
                         (id (gethash "id" r)))
                     (with-current-buffer (url-retrieve-synchronously
                                           (mdx-api (concat "/author/" id)))
                       (let ((a (mdx-json-parse-request)))
                         (mdx-gethash a "data" "attributes")))))))
         (gethash field ht))))
   (mdx-gethash ht "relationships")))

(defun mdx-get-artists (ht)
  "Return the artists of the manga HT."
  (mdx--get-artist-or-author-field ht "artist" "name"))

(defun mdx-get-authors (ht)
  "Return the authors of the manga HT."
  (mdx--get-artist-or-author-field ht "author" "name"))

(defun mdx-get-pixiv (ht)
  "Return pixiv URL of the arists and authors of the manga HT."
  (append (mdx--get-artist-or-author-field ht "author" "pixiv")
          (mdx--get-artist-or-author-field ht "artist" "pixiv")))

(defun mdx-get-manga-id (ht)
  "Return the manga id for the manga HT."
  (mdx-gethash ht "id"))

(defun mdx-get-cover-url (ht)
  "Get the cover URL for the manga HT."
  (let ((cover (seq-find (lambda (r)
                           (equal "cover_art" (gethash "type" r)))
                         (mdx-gethash ht "relationships"))))
    (if (gethash "fileName" cover)
        (gethash "fileName" cover)
      (mdx-fetch-cover-url ht))))

(defun mdx-fetch-cover-url (ht)
  "Fetch the cover URL for the manga HT."
  (let* ((cover-id (gethash "id"
                            (seq-find (lambda (r)
                                        (equal "cover_art" (gethash "type" r)))
                                      (mdx-gethash ht "relationships"))))
         (manga-id (mdx-get-manga-id ht))
         (cjson (let ((url-request-method "GET"))
                  (with-current-buffer (url-retrieve-synchronously
                                        (mdx-api (concat "/cover/" cover-id)))
                    (mdx-json-parse-request)))))
    (format "https://uploads.mangadex.org/covers/%s/%s"
            manga-id
            (mdx-gethash cjson "data" "attributes" "fileName"))))

(defun mdx-fetch-cover-art (ht directory)
  "Fetch the covert art for HT and save it in DIRECTORY.
The filename shall be the ID of the manga HT.
Returns the absolute filename used for the cover art if
successfully fetched."
  (let* ((url (mdx-get-cover-url ht))
         (manga-id (mdx-get-manga-id ht))
         (file (expand-file-name
                (concat manga-id "." (file-name-extension url))
                directory)))
    (and (url-copy-file url file :ok-if-exists)
         file)))

;; Did I mention that "Boys' Love" and "Girls' Love" makes me puke?
;; Bring back Yuri and Yaoi.
(defcustom mdx-tag-translate
  '(("Boys' Love" . "Yaoi")
    ("Girls' Love" . "Yuri")
    ("Slice of Life" . "SOL"))
  "An alist of tag names used by mangadex and their replacement."
  :type '(alist :key-type string :value-type string))

(defvar mdx-tag-replacements
  '(("'" . "")
    (" " . "-"))
  "An alist of string replacement for tag names.")

(defun mdx--normalise-tag-name (tag)
  "Normalise the tag name TAG."
  (setq tag (downcase (alist-get tag mdx-tag-translate tag nil #'equal)))
  (pcase-dolist (`(,from . ,to) mdx-tag-replacements)
    (setq tag (string-replace from to tag)))
  tag)

(defun mdx-get-tags (ht)
  "Return the list of tags for the manga HT.
This also includes the demographic."
  (let ((demographic (mdx-get-demographic ht))
        (tags (mdx-gethash ht "attributes" "tags"))
        res)
    (dolist (tag tags)
      (let ((name (mdx--normalise-tag-name
                   (mdx-gethash tag "attributes" "name" "en"))))
        (push name res)))
    (unless (equal demographic "unknown")
      (push demographic res))
    res))

(defvar mdx-prop-func-alist
  '((Title . mdx-get-titles)
    (Artist . mdx-get-artists)
    (Author . mdx-get-authors)
    (Pixiv . mdx-get-pixiv)
    (Demographic . mdx-get-demographic)
    (Tag . mdx-get-tags)
    (ID . mdx-get-manga-id))
  "An alist that maps the property to the fetcher function.")

(defun mdx--insert-properties (prop values)
  "Insert properties PROP with values VALUES."
  (when values
    (if (atom values)                   ; Thank you Drew!
        (insert ":" (symbol-name prop) ":" "\t" values "\n")
      (when (car values)
        (let* ((prop (symbol-name prop))
               (prop+ (concat prop "+")))
          (insert ":" prop ":" "\t" (car values) "\n")
          (dolist (v (cdr values))
            (insert ":" prop+ ":" "\t" v "\n")))))))

;; TODO: No. of chapters read.  https://api.mangadex.org/docs/redoc.html#tag/ReadMarker/operation/get-manga-chapter-readmarkers
(defun mdx-init-org-buffer ()
  "Write the necessary Org stuff for the mdx org buffer."
  (unless (file-directory-p "mdx-data/")
    (make-directory "mdx-data/"))
  (insert "# -*- org-attach-directory: \"mdx-data/\"; -*-\n"
          "#+TODO: TODO SOMEDAY | DONE DROPPED ONHOLD\n"))

(defcustom mdx-reading-status-TODO-alist
  '(("reading" . "TODO")
    ("re-reading" . "TODO")
    ("on_hold" . "ONHOLD")
    ("dropped" . "DROPPED")
    ("plan_to_read" . "SOMEDAY")
    ("completed" . "DONE")
    (nil . ""))
  "An alist of manga reading status and org TODO keyword.
The key value nil means no reading status set."
  :type '(alist :key-type (choice
                           (const :tag "No reading status" nil)
                           (string :tag "Reading status"))
                :value-type string))

(defun mdx-write-org-heading (ht)
  "Write Org heading for the manga HT."
  (let* ((title (car (mdx-get-titles ht)))
         (description (mdx-get-description ht))
         (id (mdx-get-manga-id ht))
         (status (assoc-default (mdx-get-manga-reading-status id)
                                mdx-reading-status-TODO-alist)))
    (message "Writing `%s'..." title)
    (insert "* " status " " title "\n")
    (insert ":PROPERTIES:" "\n")
    (pcase-dolist (`(,prop . ,func) mdx-prop-func-alist)
      (mdx--insert-properties prop (funcall func ht)))
    (mdx--insert-properties 'DIR "mdx-data/")
    (insert ":END:" "\n")
    (insert "\n")
    (insert "[[https://mangadex.org/title/"
            id
            "][Link]]"
            " "
            "[[elisp:(mdx-update-org-heading)][Update]]"
            " "
            "[[attachment:<<<COVERHERE>>>][Cover]]"
            "\n")
    (when description
      (insert "\n")
      (let ((point (point)))
        (insert description)
        (save-restriction
          (narrow-to-region point (point-max))
          ;; (vz/nov-repunctuate-sentences)
          (fill-region (point-min) (point-max))
          (let ((point (point)))
            (when (< (skip-chars-backward "\n[:space:]") 0)
              (delete-region (point) point)))))
      (insert "\n\n"))
    (let ((cover (car (file-expand-wildcards (concat "mdx-data/" id ".*")))))
      (unless cover
        (message "Fetching album art for %s" title)
        (setq cover (mdx-fetch-cover-art ht (expand-file-name "mdx-data/"))))
      (save-excursion
        (and (search-backward "<<<COVERHERE>>>")
             (replace-match (file-name-nondirectory cover) t t))))))

(defun mdx-update-file ()
  (with-current-buffer (find-file-noselect mdx-filename)
    (goto-char (point-min))
    (with-buffer-unmodified-if-unchanged
      (let ((ids (org-map-entries (lambda () (cons (org-id-get) (point-marker)))))
            (manga (mdx-get-followed-manga)))
        (dolist (m manga)
          (let ((marker (assoc-default (mdx-get-manga-id m) ids)))
            (if marker
                (progn
                  (goto-char marker)
                  (mdx-update-org-heading m))
              (goto-char (point-max))
              (insert "\n")
              (mdx-write-org-heading m))))
        (dolist (i ids)
          (set-marker (cdr i) nil))))
    (pop-to-buffer (current-buffer))))

(defun mdx ()
  "Create or open & update MangaDex Org file."
  (interactive)
  (if (file-exists-p mdx-filename)
      (mdx-update-file)
    (with-current-buffer (get-buffer-create "test2")
      (let ((default-directory (file-name-directory mdx-filename)))
        (erase-buffer)
        (mdx-init-org-buffer)
        (org-mode)
        (let ((manga (mdx-get-followed-manga)))
          (dolist (m manga)
            (mdx-write-org-heading m)))
        (write-region (point-min) (point-max) mdx-filename)))
    (pop-to-buffer (find-file-noselect mdx-filename))))

(defcustom mdx-update-props
  nil ;; '(Demographic Tag Pixiv)
  "List of tags to update when fetching each time."
  :type '(repeat symbol))

;; TODO: Update no. of chapters read.
(defun mdx-update-org-heading (&optional ht)
  "Update the current heading's details.
If optional HT is non-nil, then use that hash table rather than
refetching."
  (save-restriction
    (org-narrow-to-subtree)
    (let* ((manga-id (org-entry-get (point) "ID"))
           (manga (or ht (mdx-get-manga manga-id)))
           ;; If HT is supplied, then we can assume that reading
           ;; status is also up-to-date.
           (status (mdx-get-manga-reading-status manga-id (null ht))))
      (goto-char (point-min))
      (when (search-forward "* ")
        (when (looking-at org-todo-regexp)
          (replace-match ""))
        (insert (assoc-default status mdx-reading-status-TODO-alist)))
      (dolist (p mdx-update-props)
        (let ((values (funcall (cdr (assq p mdx-prop-func-alist))
                               manga)))
          (goto-char (point-min))
          (when (or values (car values))
            (save-excursion
              (while (re-search-forward (format ":%s\\+?:" p) nil 'noerror)
                ;; 1+ is needed to delete the newline.
                (delete-region (pos-bol) (1+ (pos-eol)))))
            (search-forward ":END:")
            (forward-line 0)
            (mdx--insert-properties p values)))))))

(provide 'mdx)
;;; mdx.el ends here
