;;;Auto generated

;;;### (autoloads (gcal-sign-in gcal-sign-out gcal-emacs-calendar-setup
;;;;;;  gcal-show-event gcal-view gcal-calendar-agenda-days gcal-delete-event
;;;;;;  gcal-quickadd-event gcal-add-event gcal-user-email gcal-default-user-email)
;;;;;;  "gcal" "gcal.el" (18363 4993))
;;; Generated autoloads from gcal.el

(defvar gcal-default-user-email nil "\
Default user id for Calendar.")

(custom-autoload (quote gcal-default-user-email) "gcal" t)

(defvar gcal-user-email nil "\
Mail address that identifies calendar user.")

(custom-autoload (quote gcal-user-email) "gcal" t)

(autoload (quote gcal-add-event) "gcal" "\
Add a calendar event.

\(fn)" t nil)

(autoload (quote gcal-quickadd-event) "gcal" "\
Add a calendar event.
Specify the event in plain English.

\(fn EVENT-DESC)" t nil)

(autoload (quote gcal-delete-event) "gcal" "\
Delete a calendar event.

\(fn EVENT-URI)" t nil)

(defvar gcal-calendar-agenda-days 5 "\
Number of days for which we show an agenda by default.")

(custom-autoload (quote gcal-calendar-agenda-days) "gcal" t)

(autoload (quote gcal-view) "gcal" "\
Retrieve and display resource after authenticating.

\(fn RESOURCE)" t nil)

(autoload (quote gcal-show-event) "gcal" "\
Show event at URL.

\(fn URL)" t nil)

(define-prefix-command (quote gcal-calendar-prefix-map))

(autoload (quote gcal-emacs-calendar-setup) "gcal" "\
Setup GCal keybindings in Emacs calendar.

\(fn)" nil nil)

(autoload (quote gcal-sign-out) "gcal" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload (quote gcal-sign-in) "gcal" "\
Sign in, useful when changing to a different user profile.

\(fn)" t nil)

;;;***

;;;### (autoloads (gphoto-edit-entry gphoto-sign-in gphoto-sign-out
;;;;;;  gphoto-comment-or-tag gphoto-directory-add-photos gphoto-photo-add
;;;;;;  gphoto-album-create gphoto-user-tagsearch gphoto-user-search
;;;;;;  gphoto-recent gphoto-community-search gphoto-download gphoto-view
;;;;;;  gphoto-tags gphoto-albums gphoto-feeds) "gphoto" "gphoto.el"
;;;;;;  (18363 4993))
;;; Generated autoloads from gphoto.el

(autoload (quote gphoto-feeds) "gphoto" "\
Retrieve and display feed of albums or tags after authenticating.

\(fn KIND)" t nil)

(autoload (quote gphoto-albums) "gphoto" "\
Display feed of albums.

\(fn)" t nil)

(autoload (quote gphoto-tags) "gphoto" "\
View feed of tags.

\(fn)" t nil)

(autoload (quote gphoto-view) "gphoto" "\
Retrieve and display resource after authenticating.

\(fn RESOURCE)" t nil)

(autoload (quote gphoto-download) "gphoto" "\
Download resource after authenticating.

\(fn RESOURCE)" t nil)

(autoload (quote gphoto-community-search) "gphoto" "\
Search all public photos.

\(fn QUERY)" t nil)

(autoload (quote gphoto-recent) "gphoto" "\
Retrieve feed of recently uploaded photos or comments.

\(fn USER KIND)" t nil)

(autoload (quote gphoto-user-search) "gphoto" "\
Retrieve feed o recently uploaded comments for  specified user.

\(fn USER QUERY)" t nil)

(autoload (quote gphoto-user-tagsearch) "gphoto" "\
Retrieve feed o matches comments for  specified user.

\(fn USER TAG)" t nil)

(autoload (quote gphoto-album-create) "gphoto" "\
Create a new GPhoto album.

\(fn)" t nil)

(autoload (quote gphoto-photo-add) "gphoto" "\
Add a photo to an existing album.

\(fn ALBUM-NAME PHOTO)" t nil)

(autoload (quote gphoto-directory-add-photos) "gphoto" "\
Add all jpeg files in a directory to specified album.

\(fn DIRECTORY ALBUM-NAME)" t nil)

(autoload (quote gphoto-comment-or-tag) "gphoto" "\
Add comments or tags  to an existing photo.

\(fn TYPE RESOURCE)" t nil)

(autoload (quote gphoto-sign-out) "gphoto" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload (quote gphoto-sign-in) "gphoto" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload (quote gphoto-edit-entry) "gphoto" "\
Retrieve metadata for entry and prepare it for editting.
The retrieved entry is placed in a buffer ready for editing.
`url' is the URL of the entry.

\(fn URL)" t nil)

;;;***

;;;### (autoloads (greader-sign-in greader-sign-out greader-search
;;;;;;  greader-find-feeds greader-star greader-add-label greader-untag-feed
;;;;;;  greader-tag-feed greader-title-feed greader-unsubscribe-feed
;;;;;;  greader-subscribe-feed greader-opml greader-feed-list greader-preferences
;;;;;;  greader-reading-list) "greader" "greader.el" (18363 4993))
;;; Generated autoloads from greader.el

(autoload (quote greader-reading-list) "greader" "\
Ensure our cookies are live, and get the reading list.
Optional interactive prefix `state' prompts for state to retrieve

e.g., starred.

\(fn &optional STATE)" t nil)

(autoload (quote greader-preferences) "greader" "\
Ensure our cookies are live, and get all preferences for this
user.

\(fn)" t nil)

(autoload (quote greader-feed-list) "greader" "\
Retrieve list of subscribed feeds.
Feeds are sorted by timestamp of newly arrived articles.
Optional interactive prefix arg `nosort' turns off sorting.

\(fn &optional NOSORT)" t nil)

(autoload (quote greader-opml) "greader" "\
Retrieve OPML representation of our subscription list.

\(fn)" t nil)

(autoload (quote greader-subscribe-feed) "greader" "\
Subscribe to specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-unsubscribe-feed) "greader" "\
UnSubscribe from specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-title-feed) "greader" "\
Title  specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-tag-feed) "greader" "\
Tag  specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-untag-feed) "greader" "\
Remove Tag from specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-add-label) "greader" "\
Add label to this item.

\(fn ITEM-URL LABEL)" t nil)

(autoload (quote greader-star) "greader" "\
Star this item.

\(fn ITEM-URL)" t nil)

(autoload (quote greader-find-feeds) "greader" "\
Find feeds matching query.

\(fn QUERY)" t nil)

(autoload (quote greader-search) "greader" "\
GReader search.

\(fn QUERY)" t nil)

(autoload (quote greader-sign-out) "greader" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload (quote greader-sign-in) "greader" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

;;;***

;;;### (autoloads (gsheet-sign-in gsheet-sign-out gsheet-sheets gsheet-fetch)
;;;;;;  "gsheet" "gsheet.el" (18363 4993))
;;; Generated autoloads from gsheet.el

(autoload (quote gsheet-fetch) "gsheet" "\
Fetch specified sheet.

\(fn SHEET-URL)" t nil)

(autoload (quote gsheet-sheets) "gsheet" "\
Retrieve and display feed of feeds after authenticating.

\(fn)" t nil)

(autoload (quote gsheet-sign-out) "gsheet" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload (quote gsheet-sign-in) "gsheet" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

;;;***

;;;### (autoloads (gskeleton-sign-in gskeleton-sign-out) "gskeleton"
;;;;;;  "gskeleton.el" (18363 4993))
;;; Generated autoloads from gskeleton.el

(autoload (quote gskeleton-sign-out) "gskeleton" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload (quote gskeleton-sign-in) "gskeleton" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

;;;***

;;;### (autoloads (gtube-video-featured gtube-video-by-user gtube-video-popular
;;;;;;  gtube-video-playlist gtube-video-by-category-and-tag gtube-video-by-tag
;;;;;;  gtube-video-details gtube-user-friends gtube-user-favorites
;;;;;;  gtube-user-profile) "gtube" "gtube.el" (18363 4993))
;;; Generated autoloads from gtube.el

(autoload (quote gtube-user-profile) "gtube" "\
Retrieve user profile.

\(fn &optional USER)" t nil)

(autoload (quote gtube-user-favorites) "gtube" "\
Retrieve user favorites.

\(fn &optional USER)" t nil)

(autoload (quote gtube-user-friends) "gtube" "\
Retrieve user profile.

\(fn &optional USER)" t nil)

(autoload (quote gtube-video-details) "gtube" "\
Display details of specified video.

\(fn VIDEO-ID)" t nil)

(autoload (quote gtube-video-by-tag) "gtube" "\
Retrieve content having specified tag.
optional args page and count specify position in result-set and
  number of results to retrieve.

\(fn TAG &optional PAGE COUNT)" t nil)

(autoload (quote gtube-video-by-category-and-tag) "gtube" "\
Retrieve content from specified category having specified tag.
optional args page and count specify position in result-set and
  number of results to retrieve.

\(fn CATEGORY TAG &optional PAGE COUNT)" t nil)

(autoload (quote gtube-video-playlist) "gtube" "\
Retrieve content in specified playlist.
optional args page and count specify position in result-set and
  number of results to retrieve.

\(fn PLAYLIST-ID &optional PAGE COUNT)" t nil)

(autoload (quote gtube-video-popular) "gtube" "\
Retrieve popular content for specified time-range.
  Time-range is one of day, week, month, or all.

\(fn TIME-RANGE)" t nil)

(autoload (quote gtube-video-by-user) "gtube" "\
Retrieve content from specified user.
optional args page and count specify position in result-set and
  number of results to retrieve.

\(fn USER &optional PAGE COUNT)" t nil)

(autoload (quote gtube-video-featured) "gtube" "\
Retrieved featured video list.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("g-app.el" "g-auth.el" "g-autogen.el"
;;;;;;  "g-load-path.el" "g-utils.el" "g.el" "gnotebook.el" "indent-files.el"
;;;;;;  "json.el") (18363 5002 517430))

;;;***

;;;### (autoloads (gblogger-sign-in gblogger-sign-out gblogger-add-label
;;;;;;  gblogger-delete-entry gblogger-new-entry gblogger-edit-entry
;;;;;;  gblogger-atom-display gblogger-blog) "gblogger" "gblogger.el"
;;;;;;  (18363 4993))
;;; Generated autoloads from gblogger.el

(autoload (quote gblogger-blog) "gblogger" "\
Retrieve and display feed of feeds after authenticating.

\(fn)" t nil)

(autoload (quote gblogger-atom-display) "gblogger" "\
Retrieve and display specified feed after authenticating.

\(fn FEED-URL)" t nil)

(autoload (quote gblogger-edit-entry) "gblogger" "\
Retrieve entry and prepare it for editting.
The retrieved entry is placed in a buffer ready for editing.
`url' is the URL of the entry.

\(fn URL)" t nil)

(autoload (quote gblogger-new-entry) "gblogger" "\
Create a new Blog post.

\(fn URL)" t nil)

(autoload (quote gblogger-delete-entry) "gblogger" "\
Delete item at specified edit URL.

\(fn EDIT-URL)" t nil)

(autoload (quote gblogger-add-label) "gblogger" "\
Adds labels to gblogger entry being editted.

\(fn LABEL)" t nil)

(autoload (quote gblogger-sign-out) "gblogger" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

(autoload (quote gblogger-sign-in) "gblogger" "\
Resets client so you can start with a different userid.

\(fn)" t nil)

;;;***
