;;; greader.el --- Google Reader
;;;$Id: greader.el,v 1.74 2006/10/13 01:37:30 raman Exp $
;;; $Author: raman $
;;; Description:  Google Reader
;;; Keywords: Google   Atom API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; greader| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Reader|
;;; $Date: 2006/10/13 01:37:30 $ |
;;;  $Revision: 1.74 $ |
;;; Location undetermined
;;; License: GPL
;;;

;;}}}
;;{{{ Copyright:

;;; Copyright (c) 2006 and later, Google Inc.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without modification,
;;; are permitted provided that the following conditions are met:

;;;     * Redistributions of source code must retain the above copyright notice,
;;;       this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright notice,
;;;       this list of conditions and the following disclaimer in the documentation
;;;       and/or other materials provided with the distribution.
;;;     * The name of the author may not be used to endorse or promote products
;;;       derived from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;{{{  introduction

;;; Google Reader
;;; Entry Points:

;;; greader-reading-list
;;; greader-subscribe-feed
;;; greader-find-feed

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)
(require 'g-auth)
(require 'browse-url)

;;}}}
;;{{{ customizations

(defgroup greader nil
  "Google Reader"
  :group 'g)
(defcustom greader-user-email nil
  "Mail address that identifies reader user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'greader)

(defcustom greader-user-password nil
  "Password for authenticating to reader account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'greader)

;;}}}
;;{{{ constants:

(defconst greader-service-name "reader"
  "Service name for accessing Google Reader.")

(defsubst greader-p (service)
  "Check if this is Reader."
  (declare (special greader-service-name))
  (string-equal service greader-service-name))

(defvar greader-base-url
  "http://www.google.com/reader/"
  "Base URL for Google Reader  API.")

(defvar greader-token-url
  (concat greader-base-url
          "api/0/token ")
  "URL for retrieving Google Reader token.")

;;}}}
;;{{{  Reader Authenticate

(defsubst make-greader-auth ()
  "Make a new greader auth handle."
  (declare (special greader-service-name
                    greader-user-email greader-user-password))
  (make-g-auth :service greader-service-name
               :email greader-user-email
               :password greader-user-password
               :post-auth-action 'greader-post-authenticate-function))

(defvar greader-auth-handle
  (make-greader-auth)
  "Greader auth handle.
Holds user's email address, password, and the auth token received
from the server.")

(defun greader-post-authenticate-function (auth-handle)
  "Run Googlre Reader post-auth steps."
  (declare (special g-curl-program g-curl-common-options
                    greader-token-url))
  (unless (greader-p (g-auth-service auth-handle))
    (error "This auth handle is not for Google Reader."))
  (setf (g-auth-token auth-handle)
        (g-get-result
         (format
          "%s %s --cookie SID='%s' %s 2>/dev/null"
          g-curl-program g-curl-common-options
          (g-cookie "SID" auth-handle) greader-token-url))))

;;}}}
;;{{{ Generators:

(defvar greader-feed-url-pattern
  (concat greader-base-url
          "public/atom/feed/%s?n=%s")
  "URL pattern to generate URLs to streams from external feeds.")

(defsubst greader-feed-url (feed-url)
  "Return URL to stream for specified feed."
  (declare (special greader-feed-url-pattern
                    greader-number-of-articles))
  (format greader-feed-url-pattern
          (g-url-encode feed-url)
          greader-number-of-articles))

(defvar greader-state-url-pattern
  (concat greader-base-url
          "atom/user/-/state/com.google/%s?n=%s&start=%s")
  "URL pattern to generate URLs to `state' streams.")

(defcustom greader-number-of-articles 50
  "Number of articles to retrieve."
  :type 'integer
  :group 'greader)

(defsubst greader-state-url (state)
  "Return URL to stream for specified state."
  (declare (special greader-state-url-pattern
                    greader-number-of-articles))
  (format greader-state-url-pattern state
          greader-number-of-articles
          0))

(defvar greader-state-alist
  '(("reading-list" . "reading-list")
    ("read" . "read")
    ("kept-unread" . "kept-unread")
    ("starred" . "starred"))
  "Association list of state names.")

(defcustom greader-default-state "reading-list"
  "State of default stream we pull."
  :type
  (let ((type (mapcar
               #'(lambda (c)
                   (list 'item (car c)))
               greader-state-alist)))
    (push "Reading State" type)
    (push :tag type)
    (push 'choice type)
    type)
  :group 'greader)

(defcustom greader-atom-base
  "http://www.google.com/reader/public/atom/user/"
  "Base URI to use in reading list."
  :type 'string
  :group 'greader)

(defvar greader-label-url-pattern
  (concat greader-base-url
          "atom/user/0/label/%s?n=%s")
  "URL pattern to generate URLs to `label' streams.")

(defsubst greader-label-url (label)
  "Return URL to stream for specified label."
  (declare (special greader-label-url-pattern
                    greader-number-of-articles))
  (format greader-label-url-pattern label
          greader-number-of-articles))

(defvar greader-prefs-url
  (concat greader-base-url
          "api/0/preference/list?output=json")
  "URL  to generate URLs to `prefs' streams.")

(defvar greader-find-feed-url
  (concat greader-base-url
          "api/0/feed-finder?q=%s")
  "URL  for  finding feeds.")

(defvar greader-edit-url-pattern
  "'http://www.google.com/reader/api/0/%s/edit'"
  "URL  pattern for  edit URLs.")

(defvar greader-edit-alist
  '(("subscription" . "subscription")
    ("tag" . "tag"))
  "Types of edit actions we know of.")

(defsubst greader-edit-url (type)
  "Return URL to stream for specified edit type."
  (declare (special greader-edit-url-pattern))
  (format greader-edit-url-pattern type))

;;}}}
;;{{{ getters:

(defun greader-read-state (prompt)
  "Return state name read from minibuffer."
  (declare (special greader-state-alist))
  (completing-read prompt
                   greader-state-alist
                   nil
                   'require-match))
;;;###autoload
(defun greader-reading-list (&optional state)
  "Ensure our cookies are live, and get the reading list.
Optional interactive prefix `state' prompts for state to retrieve

e.g., starred."
  (interactive "P")
  (declare (special greader-auth-handle
                    g-curl-program g-curl-common-options
                    greader-default-state g-atom-view-xsl))
  (when  (and state (interactive-p))
    (setq state (greader-read-state "State:")))
  (g-auth-ensure-token greader-auth-handle)
  (g-display-result
   (format
    "%s %s --cookie SID='%s' %s 2>/dev/null"
    g-curl-program g-curl-common-options
    (g-cookie "SID" greader-auth-handle)
    (greader-state-url (or state greader-default-state)))
   g-atom-view-xsl))

(defun greader-read-preference (prompt)
  "Return pref name read from minibuffer."
  (declare (special greader-prefs-alist))
  (completing-read prompt
                   greader-prefs-alist
                   nil
                   'require-match))

(defvar greader-preferences nil
  "User Preferences retrieved from server.")

;;;###autoload
(defun greader-preferences ()
  "Ensure our cookies are live, and get all preferences for this
user."
  (interactive)
  (declare (special greader-auth-handle  greader-prefs-url
                    greader-preferences
                    g-curl-program g-curl-common-options))
  (g-auth-ensure-token greader-auth-handle)
  (let ((preferences nil)
        (raw-preferences
         (g-json-get 'prefs
                     (g-json-get-result
                      (format
                       "%s %s --cookie SID='%s' %s 2>/dev/null"
                       g-curl-program g-curl-common-options
                       (g-cookie "SID" greader-auth-handle)
                       greader-prefs-url)))))
    (loop for v across raw-preferences
          do
          (push
           (cons
            (cdr (assoc 'id v))
            (cdr (assoc 'value v)))
           preferences))
    (setq greader-preferences preferences)))

(defvar greader-subscribed-feed-list-url
  (concat greader-base-url
          "api/0/subscription/list?output=json")
  "URL for retrieving list of subscribed feeds.")

(defvar greader-unread-count-url
  (concat greader-base-url
          "api/0/unread-count?all=true&output=json")
  "URL for retrieving unread counts for subscribed  feeds.")

(defvar greader-subscription-opml-url
  (concat greader-base-url
          "subscriptions/export")
  "URL for retrieving list of subscribed feeds.")

(defvar greader-tag-list-url
  (concat greader-base-url
          "api/0/tag/list?output=json")
  "URL for retrieving list of tags.")

(defun greader-view-json-subscriptions (subscriptions counts)
  "View Greader Subscription list."
  (declare (special greader-atom-base))
  (g-using-scratch
   (insert
    (format
     "<html><head>
<title>%s Subscription List</title>
</head>\n"
     (g-auth-username greader-auth-handle)))
   (insert
    (format "<body><h1>Subscription List For %s</h1>\n<ol>"
            (g-auth-username greader-auth-handle)))
   (loop for s across subscriptions
         do
         (let* ((id (g-json-get 'id s))
                    (count (greader-get-unread-count-by-id id counts)))
           (insert
            (format
             "<li><a href=\"%s\">%s (%s: %s)</a></li>\n"
             (let ((url (substring id 5)))
               (cond
                ((string-match "^http" url) url)
                (t (concat greader-atom-base url))))
             (g-json-get 'title s)
             count
             (cond
              ((string-match "rss" id) "R")
              ((string-match "atom" id) "A")
              (t ""))))))
   (insert "</ol></body></html>\n")
   (browse-url-of-region (point-min) (point-max))))

(defun greader-view-json-results (query results)
  "View Greader results list."
  (declare (special greader-atom-base))
  (let ((items (g-json-get 'items results)))
    (g-using-scratch
         (insert
    (format
     "<html><head>
<title> Results Matching %s</title>
</head>\n"
     query))
   (insert
    (format "<body><h1>Results Matching  %s</h1>\n<ol>"
            query))
   (loop for item across items
         do
         (insert
          (format "<h2><a href=\"%s\">%s</a></h2>\n"
         (g-json-get 'title item)
(g-json-get 'title item)))
(insert
 (format "<div>%s</div>\n"
         (g-json-get 'content
                     (g-json-get 'content item))))))))


(defun greader-unread-count ()
  "Retrieve unread counts for subscribed feeds."
  (declare (special greader-auth-handle
                    g-curl-program g-curl-common-options
                    greader-unread-count-url))
  (let ((counts
         (g-json-get 'unreadcounts
                     (g-json-get-result
                      (format
                       "%s %s --cookie SID='%s' '%s' 2>/dev/null"
                       g-curl-program g-curl-common-options
                       (g-cookie "SID" greader-auth-handle)
                       greader-unread-count-url)))))
    counts))

(defsubst greader-get-unread-count-by-id (id counts)
  "Given a Feed Id, get the unread count from the cache in counts."
  (let ((c
         (find-if
          #'(lambda (a)
              (equal (g-json-get 'id a) id))
          counts)))
    (g-json-get 'count  c)))

(defsubst greader-get-timestamp-by-id (id counts)
  "Given a Feed Id, get the timestamp from the cache in counts."
  (let ((c
         (find-if
          #'(lambda (a)
              (equal (g-json-get 'id a) id))
          counts)))
    (g-json-get 'newestItemTimestampUsec c)))

;;;###autoload
(defun greader-feed-list (&optional nosort)
  "Retrieve list of subscribed feeds.
Feeds are sorted by timestamp of newly arrived articles.
Optional interactive prefix arg `nosort' turns off sorting."
  (interactive "P")
  (declare (special greader-auth-handle
                    g-curl-program g-curl-common-options
                    greader-subscribed-feed-list-url))
  (g-auth-ensure-token greader-auth-handle)
  (let ((counts (greader-unread-count))
        (subscriptions
         (g-json-get 'subscriptions
                     (g-json-get-result
                      (format
                       "%s %s --cookie SID='%s' %s 2>/dev/null"
                       g-curl-program g-curl-common-options
                       (g-cookie "SID" greader-auth-handle)
                       greader-subscribed-feed-list-url)))))
    (unless nosort 
    (setq subscriptions
          (sort* subscriptions
                 #'(lambda (a b)
                     (let ((a-id (g-json-get 'id a))
                           (b-id (g-json-get 'id b)))
                       (>
                        (greader-get-unread-count-by-id a-id counts)
                        (greader-get-unread-count-by-id b-id counts)))))))
    (greader-view-json-subscriptions subscriptions counts)))

;;;###autoload

(defun greader-opml ()
  "Retrieve OPML representation of our subscription list."
  (interactive)
  (declare (special greader-auth-handle greader-subscription-opml-url
                    g-curl-program g-curl-common-options))
  (g-auth-ensure-token greader-auth-handle)
  (shell-command
   (format
    "%s %s --cookie SID='%s' %s 2>/dev/null"
    g-curl-program g-curl-common-options
    (g-cookie "SID" greader-auth-handle)
    greader-subscription-opml-url)))

(defun greader-view-json-tags (tags)
  "View Greader tag list."
  (g-using-scratch
   (erase-buffer)
   (insert
    (format
     "<html><head><title>%s Tag List</title></head>\n"
     (g-auth-username greader-auth-handle)))
   (insert
    (format "<body><h1>Tag List For %s</h1>\n<ol>"
            (g-auth-username greader-auth-handle)))
   (loop for tag across tags
         do
         (let ((id (g-json-get 'id tag)))
           (insert
            (format
             "<li><a href=\"%satom/%s\">%s</a> Shared: %s</li>\n"
             greader-base-url
             id
             (car (last (split-string id "/")))
             (g-json-get 'shared tag)))))
   (insert "</ol></body></html>\n")
   (browse-url-of-region (point-min) (point-max))))

(defun greader-tag-list ()
  "Retrieve list of tags for user."
  (interactive)
  (declare (special greader-auth-handle
                    g-curl-program g-curl-common-options
                    greader-tag-list-url))
  (g-auth-ensure-token greader-auth-handle)
  (let ((tags
         (g-json-get 'tags
                     (g-json-get-result
                      (format
                       "%s %s --cookie SID='%s' %s 2>/dev/null"
                       g-curl-program g-curl-common-options
                       (g-cookie "SID" greader-auth-handle)
                       greader-tag-list-url)))))
    (greader-view-json-tags tags)))

(defun greader-tagged-reading-list (tag-url)
  "Fetch feeds in specified tag."
  (interactive "sTag URL: ")
  (declare (special greader-auth-handle
                    g-curl-program g-curl-common-options
                    g-atom-view-xsl))
  (g-auth-ensure-token greader-auth-handle)
  (g-display-result
   (format
    "%s %s --cookie SID='%s' %s 2>/dev/null"
    g-curl-program g-curl-common-options
    (g-cookie "SID" greader-auth-handle)
    tag-url)
   g-atom-view-xsl))

;;}}}
;;{{{ posters:
;;;###autoload
(defun greader-subscribe-feed (feed-url )
  "Subscribe to specified feed."
  (interactive "sURL:")
  (greader-update-subscription feed-url 'subscribe))
;;;###autoload
(defun greader-unsubscribe-feed (feed-url )
  "UnSubscribe from specified feed."
  (interactive "sURL:")

  (greader-update-subscription feed-url 'unsubscribe))
;;;###autoload
(defun greader-title-feed (feed-url )
  "Title  specified feed."
  (interactive
   (list
    (read-from-minibuffer "Feed URL: "
                          (funcall g-url-under-point))))
  (greader-update-subscription feed-url 'title))

;;;###autoload
(defun greader-tag-feed (feed-url )
  "Tag  specified feed."
  (interactive
   (list
    (read-from-minibuffer"Feed URL: "
                         (funcall g-url-under-point))))
  (greader-update-subscription feed-url 'add-tags))
;;;###autoload
(defun greader-untag-feed (feed-url )
  "Remove Tag from specified feed."
  (interactive
   (list
    (read-from-minibuffer "Feed URL: "
                          (funcall g-url-under-point))))
  (greader-update-subscription feed-url 'remove-tags))

(defun greader-update-subscription (feed-url action )
  "Perform specified subscribe, unsubscribe, or edit action."
  (declare (special g-curl-program g-curl-common-options
                    greader-auth-handle))
  (g-auth-ensure-token greader-auth-handle)
  (g-using-scratch
   (let ((cl  nil))
     (insert
      (format "T=%s&ac=%s&s=feed%%2F%s&%s%s"
              (g-auth-token greader-auth-handle)
              (ecase action
                ('title "edit")
                ('subscribe "subscribe")
                ('unsubscribe "unsubscribe")
                ('add-tags "edit")
                ('remove-tags "edit"))
              (g-url-encode feed-url)
              (ecase action
                ('title "t=")
                ('subscribe "")
                ('unsubscribe "")
                ('add-tags "a=user/0/label/")
                ('remove-tags "r=user/0/label/"))
              (if (memq action '(add-tags remove-tags title))
                  (read-from-minibuffer
                   (ecase action
                     ('title "Title:")
                     ('add-tags "Add Tag:")
                     ('remove-tags "Remove Tag:")))
                "")))
     (setq cl (format "-H Content-length:%s" (g-buffer-bytes)))
     (shell-command-on-region
      (point-min) (point-max)
      (format
       "%s %s --cookie SID='%s' %s -X POST --data-binary @- %s 2>/dev/null"
       g-curl-program g-curl-common-options
       (g-cookie "SID" greader-auth-handle)
       cl                               ; content-length header
       (greader-edit-url "subscription"))
      (current-buffer) 'replace))
   (goto-char (point-min))
   (cond
    ((looking-at "OK")
     (message "%s %s" action feed-url))
    (t (error "Error %s: %s"
              action (buffer-string))))))

(defsubst greader-read-item-url (prompt)
  "Smart reader for fetching item urls."
  (let ((guess (and (fboundp g-url-under-point)
                    (funcall g-url-under-point))))
    (or guess (read-from-minibuffer prompt))))
;;;###autoload
(defun greader-add-label (item-url label)
  "Add label to this item."
  (interactive
   (list
    (greader-read-item-url "Item URL: ")
    (read-from-minibuffer "Label: ")))
  (declare (special g-curl-program g-curl-common-options
                    greader-auth-handle))
  (g-auth-ensure-token greader-auth-handle)
  (g-using-scratch
   (insert
    (format
     (concat
      "s=feed%%2F%s"
      "&a=user%%2F-%%2Flabel/%s"
      "&T=%s")
     item-url label (g-auth-token greader-auth-handle)))
   (shell-command-on-region
    (point-min) (point-max)
    (format
     "%s %s --cookie SID='%s' -X POST --data-binary @- %s 2>/dev/null"
     g-curl-program g-curl-common-options
     (g-cookie "SID" greader-auth-handle)
     (greader-edit-url "tag"))
    (current-buffer) 'replace)
   (goto-char (point-min))
   (cond
    ((looking-at "OK")
     (message "Added label %s" label))
    (t (error "Error labeling: %s"
              (buffer-string))))))
;;;###autoload
(defun greader-star (item-url)
  "Star this item."
  (interactive
   (list
    (greader-read-item-url "Item URL:")))
  (declare (special g-curl-program g-curl-common-options
                    greader-auth-handle))
  (g-auth-ensure-token greader-auth-handle)
  (g-using-scratch
   (insert
    (format
     (concat
      "s=feed%%2F%s"
      "&a=user%%2F-%%2Fstate%%2Fcom.google%%2Fstarred"
      "&T=%s")
     item-url (g-auth-token greader-auth-handle)))
   (shell-command-on-region
    (point-min) (point-max)
    (format
     "%s %s --cookie SID='%s' -X POST --data-binary @- %s 2>/dev/null"
     g-curl-program g-curl-common-options
     (g-cookie "SID" greader-auth-handle)
     (greader-edit-url "tag"))
    (current-buffer) 'replace)
   (goto-char (point-min))
   (cond
    ((looking-at "OK")
     (message "Starred it"))
    (t (error "Error labeling: %s"
              (buffer-string))))))

;;}}}
;;{{{ finders:
;;;###autoload
(defun greader-find-feeds (query)
  "Find feeds matching query."
  (interactive "sFind Feeds Matching: ")
  (declare (special greader-find-feed-url
                    g-atom-view-xsl))
  (let ((url (format greader-find-feed-url
                     (g-url-encode query))))
    (g-display-result
     (format
      "%s %s %s 2>/dev/null"
      g-curl-program g-curl-common-options
      url)
     g-atom-view-xsl)))

;;}}}
;;{{{ Searching:

(defvar greader-search-url
  (concat greader-base-url
          "api/0/search/items/ids?output=json&num=100&q=%s")
  "URL template for GReader search.")

(defvar greader-contents-rest-url
  "http://www.google.com/reader/api/0/stream/items/contents"
  "REST endpoint for getting content.")
;;;###autoload
(defun greader-search (query)
  "GReader search."
  (interactive "sQuery:")
  (declare (special greader-auth-handle
                    g-curl-program g-curl-common-options
                    greader-contents-rest-url
                    greader-search-url g-atom-view-xsl))
  (g-auth-ensure-token greader-auth-handle)
  (let ((results 
         (g-json-get 'results
                     (g-json-get-result
                      (format
                       "%s %s --cookie SID='%s' '%s' 2>/dev/null"
                       g-curl-program g-curl-common-options
                       (g-cookie "SID" greader-auth-handle)
                       (format greader-search-url
                               (g-url-encode query))))))
        (docids nil))
    (setq docids 
          (loop for i across results
                collect (cdr (assq 'id i))))
    (g-using-scratch
     (let ((cl  nil))
       (insert 
        (format "T=%s" (g-auth-token greader-auth-handle)))
       (loop for i in docids 
             do
             (insert (format "&i=%s" i)))
       (setq cl (format "-H Content-length:%s" (g-buffer-bytes)))
       (shell-command-on-region
        (point-min) (point-max)
        (format
         "%s %s --cookie SID='%s' %s -X POST --data-binary @- %s 2>/dev/null"
         g-curl-program g-curl-common-options
         (g-cookie "SID" greader-auth-handle)
         cl                             ; content-length header
         greader-contents-rest-url)
        (current-buffer) 'replace))
     (greader-view-json-results
      query
      (json-read-from-string
       (buffer-string))))))

;;}}}
;;{{{ Sign out:
;;;###autoload
(defun greader-sign-out()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special greader-auth-handle
                    greader-user-email greader-user-password))
  (message "Signing out %s from Reader"
           (g-auth-email greader-auth-handle))
  (cond
   (greader-auth-handle
    (setq greader-user-email nil
          greader-user-password nil)
    (setq greader-auth-handle (make-greader-auth)))
   (t (message "You've not used Reader in this emacs
session."))))

;;;###autoload
(defun greader-sign-in()
  "Resets client so you can start with a different userid."
  (interactive)
  (declare (special greader-auth-handle greader-user-email ))
  (setq greader-user-email
        (read-from-minibuffer "User Email:"))
  (setq greader-auth-handle (make-greader-auth))
  (g-authenticate greader-auth-handle))

;;}}}

(provide 'greader)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
