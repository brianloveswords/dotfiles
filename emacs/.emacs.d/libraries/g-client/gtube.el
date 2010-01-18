;;; g-tube.el --- YouTube Google  Client
;;;$Id:$
;;; $Author: raman $
;;; Description:  YouTube API Client
;;; Keywords: Google    API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gcal| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Reader|
;;; $Date: 2006/09/28 17:47:44 $ |
;;;  $Revision: 1.30 $ |
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

;;; http://www.youtube.com/dev_intro
;;; http://www.youtube.com/dev_docs

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)
(require 'g-auth)
(require 'browse-url)

;;}}}
;;{{{ Customizations

(defgroup gtube nil
  "Google Youtube"
  :group 'g)
(defcustom gtube-user-name nil
  "YouTube user name."
  :type '(choice
          (const :tag "none" nil)
          (string   ""))
  :group 'gtube)

(defcustom gtube-user-email nil
  "YouTube user email address."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gtube)

(defcustom gtube-user-password nil
  "Password for authenticating to YouTube account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gtube)

(defcustom gtube-developer-id nil
  "YouTube API  Developer Id"
  :type '(radio (const :tag "Prompt for Id" nil)
                (string :tag "Save Id in .emacs"))
  :group 'gtube)

(defcustom gtube-view
  (expand-file-name "ut.xsl" g-directory)
  "XSL transform used to view YouTube responses."
  :group 'gtube)

;;}}}
;;{{{ Constants

;;; not yet used:

(defconst gtube-service-name "skel"
  "Service name for accessing Google tube.")

(defsubst gtube-p (service)
  "Check if this is Calendar."
  (declare (special gtube-service-name))
  (string-equal service gtube-service-name))

(defconst gtube-rest-end-point
  "http://www.youtube.com/api2_rest"
  "REST end-point for YouTube APIs.")

;;}}}
;;{{{ Construct REST end-point:

(defsubst gtube-rest-resource (method-name &optional arguments)
  "Return a GTube REST URI.
Calls method `method-name' with specified arguments.
Arguments is a list of name/value pairs."
  (declare (special gtube-rest-end-point
                    gtube-developer-id))
  (let ((base (format "%s?method=%s&dev_id=%s"
                      gtube-rest-end-point
                      method-name gtube-developer-id )))
    (if arguments
        (concat base
                "&"
                (mapconcat
                 #'(lambda (pair)
                     (format "%s=%s"
                             (car pair)
                             (g-url-encode (cadr pair))))
                 arguments
                 "&"))
      base)))

;;; helper: get XML
(defvar gtube-xml-buffer "*GTube Results*"
  "Buffer used to hold XML responses.")

(defsubst gtube-display (resource)
  "Retrieve and display YouTube response."
  (declare (special gtube-view
                    g-curl-program))
  (when (featurep 'emacspeak)
    (add-hook 'emacspeak-web-post-process-hook
	      #'(lambda ()
		  (declare (special emacspeak-we-url-executor))
		  (setq emacspeak-we-url-executor
			'emacspeak-m-player-youtube-player))))
  (g-display-result
   (format "%s --silent '%s' %s"
           g-curl-program resource
           (g-curl-debug))
   gtube-view))
      
;;}}}
;;{{{ get info:

;;;###autoload
(defun gtube-user-profile (&optional user)
  "Retrieve user profile."
  (interactive
   (list
    (read-from-minibuffer "username: "
                          gtube-user-name)))
  (declare (special gtube-user-name))
  (or user (setq user gtube-user-name))
  (gtube-display
   (gtube-rest-resource "youtube.users.get_profile"
                        `(("user" ,user)))))

;;;###autoload
(defun gtube-user-favorites (&optional user)
  "Retrieve user favorites."
  (interactive
   (list
    (read-from-minibuffer "username: "
                          gtube-user-name)))
  (declare (special gtube-user-name))
  (or user (setq user gtube-user-name))
  (gtube-display
   (gtube-rest-resource "youtube.users.list_favorite_videos"
                        `(("user" ,user)))))

;;;###autoload
(defun gtube-user-friends (&optional user)
  "Retrieve user profile."
  (interactive
   (list
    (read-from-minibuffer "username: "
                          gtube-user-name)))
  (declare (special gtube-user-name))
  (or user (setq user gtube-user-name))
  (gtube-display
   (gtube-rest-resource "youtube.users.list_friends"
                        `(("user" ,user)))))

;;;###autoload
(defun gtube-video-details (video-id)
  "Display details of specified video."
  (interactive
   (list
    (read-from-minibuffer "Video: "
                          (word-at-point))))
  (gtube-display
   (gtube-rest-resource  "youtube.videos.get_details"
                         `(("video_id" ,video-id)))))

;;;###autoload
(defun gtube-video-by-tag (tag &optional page count)
  "Retrieve content having specified tag.
optional args page and count specify position in result-set and
  number of results to retrieve."
  (interactive
   (list
    (read-from-minibuffer "Tag: "
                          (word-at-point))
    (read-from-minibuffer "Page: "
                          "1")
    (read-from-minibuffer "Count: "
                          "25")))
  (or page (setq page "1"))
  (or count (setq count "10"))
  (gtube-display
   (gtube-rest-resource  "youtube.videos.list_by_tag"
                         `(("tag" ,tag)
                           ("page" ,page)
                           ("per_page" ,count)))))

;;;###autoload
(defun gtube-video-by-category-and-tag (category tag &optional page count)
  "Retrieve content from specified category having specified tag.
optional args page and count specify position in result-set and
  number of results to retrieve."
  (interactive
   (list
    (read-from-minibuffer "Category: "
                          (word-at-point))
    (read-from-minibuffer "Tag: "
                          (word-at-point))
    (read-from-minibuffer "Page: "
                          "1")
    (read-from-minibuffer "Count: "
                          "25")))
  (or page (setq page "1"))
  (or count (setq count "10"))
  (gtube-display
   (gtube-rest-resource  "youtube.videos.list_by_tag"
                         `(("tag" ,tag)
                           ("category_id" ,category)
                           ("page" ,page)
                           ("per_page" ,count)))))

;;;###autoload
(defun gtube-video-playlist (playlist-id &optional page count)
  "Retrieve content in specified playlist.
optional args page and count specify position in result-set and
  number of results to retrieve."
  (interactive
   (list
    (read-from-minibuffer "PlayList: "
                          (word-at-point))
    (read-from-minibuffer "Page: "
                          "1")
    (read-from-minibuffer "Count: "
                          "25")))
  (or page (setq page "1"))
  (or count (setq count "10"))
  (gtube-display
   (gtube-rest-resource  "youtube.videos.list_by_playlist"
                         `(("id" ,playlist-id)
                           ("page" ,page)
                           ("per_page" ,count)))))

;;;###autoload
(defun gtube-video-popular (time-range )
  "Retrieve popular content for specified time-range.
  Time-range is one of day, week, month, or all."
  (interactive "sTime Range: ")
  (gtube-display
   (gtube-rest-resource  "youtube.videos.list_popular"
                         `(("time_range" ,time-range)))))

;;;###autoload
(defun gtube-video-by-user (user &optional page count)
  "Retrieve content from specified user.
optional args page and count specify position in result-set and
  number of results to retrieve."
  (interactive
   (list
    (read-from-minibuffer "User: "
                          (word-at-point))
    (read-from-minibuffer "Page: "
                          "1")
    (read-from-minibuffer "Count: "
                          "25")))
  (or page (setq page "1"))
  (or count (setq count "10"))
  (gtube-display
   (gtube-rest-resource  "youtube.videos.list_by_user"
                         `(("user" ,user)
                           ("page" ,page)
                           ("per_page" ,count)))))

;;;###autoload
(defun gtube-video-featured ( )
  "Retrieved featured video list."
  (interactive)
  (gtube-display
   (gtube-rest-resource  "youtube.videos.list_featured")))

;;}}}
;;{{{ tube Authenticate

;;; not yet used:

(defsubst make-gtube-auth ()
  "Make a new gtube auth handle."
  (declare (special gtube-service-name
                    gtube-user-email gtube-user-password))
  (make-g-auth :service gtube-service-name
               :email gtube-user-email
               :password gtube-user-password))

(defvar gtube-auth-handle (make-gtube-auth)
  "G auth handle used for signing into calendar.")

(defun gtube-authenticate ()
  "Authenticate into Google Calendar."
  (declare (special gtube-auth-handle))
  (g-authenticate gtube-auth-handle))

;;}}}

(provide 'gtube)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
