;;; indie-org.sh.el --- Code for publishing indie-org.sh   -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Michael Herstine

;; Author: Michael Herstine <sp1ff@pobox.com>
;; Keywords: local, outlines, wp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Code for publishing indie-org.sh.

;;; Code:

(require 'indie-org)
(require 'indie-org-rss)
(require 'indie-org-h-feed)

;; This needs to be defined at file scope, otherwise `load-file-name'
;; resolves to nil.
(defconst project-dir
  (let ((x (file-name-directory load-file-name)))
    (file-name-directory (substring x 0 (- (length x) 1))))
  "Project directory for indie-org.sh.
Presumed to be the parent of the directory containing this file).
Includes a trailing '/'.")

(defconst publication-state-file
  (concat (file-name-as-directory project-dir) ".state")
  "File to which publication state is serialized.")

(defconst webmentions-io-token-file
  (concat (file-name-as-directory project-dir) ".webmention-io-token"))

(defconst telegraph-io-token-file
  (concat (file-name-as-directory project-dir) ".telegraph-io-token"))

(defun iosh/relpath (level)
  "Return a relative path LEVEL deep (e.g. 2 => ../../)."
  (let ((p (mapconcat 'identity (make-list level "..") "/")))
    (if (eq 0 (length p)) "./" (concat p "/"))))

(defun iosh/extra-head (level)
  "Return the org-html 'extra-head' for a directory at level LEVEL.

This is HTML that goes at the end of the <head> of the
document.  I use this to pull in the style sheet that themes the
site as well as setup webmention links."
  (concat
   (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%sres/style.css\">\n" (iosh/relpath level))
   "<link rel=\"webmention\" href=\"https://webmention.io/indie-org.sh/webmention\"/>\n"
   "<link rel=\"pingback\" href=\"https://webmention.io/indie-org.sh/xmlrpc\"/>\n"))

(defun iosh/postamble (info)
  "Return the HTML defining the page footer for both posts & top-level pages.
INFO is the communications channel describing the currentpage."
  (let* ((title (org-export-data (plist-get info :title) info))
         (webmentions-received (plist-get info :iosh/webmentions-received))
         (posse-responses
          (indie-org-pub-posse-responses-at-pub info (plist-get info :iosh/posse-responses)))
         (wms
          (sort
           (indie-org-pub-webmentions-received-at-pub info webmentions-received)
           (lambda (lhs rhs)
             (<
              (float-time (indie-org-received-wm-time-received lhs))
              (float-time (indie-org-received-wm-time-received rhs)))))))

    (if posse-responses
        (concat
         "<br>\n<div class=\"iosh-syndications\">\n"
         "View on "
         (string-join
           (mapcar
            (lambda (rsp)
              (concat
               "<a class=\"u-syndication\" rel=\"syndication\" href=\""
               (indie-org-posse-response-url rsp)
               "\">"
               (indie-org-posse-target-to-string
                (indie-org-posse-response-sort rsp))
               "</a>"))
            posse-responses)
           ",")
          "\n</div>\n"))

    (concat
     "<div class=\"iosh-wm-div\">\n"
     (apply
      #'concat
      (mapcar
       (lambda (wm)
         (let* ((author (indie-org-webmentions-received-wm-author wm))
                (author-name (indie-org-webmentions--received-wm-author-name author))
                (author-url (indie-org-webmentions--received-wm-author-url author))
                (name
                 (if (and author-name (not (eq 0 (length author-name))))
                     author-name
                   "someone"))
                (verb
                 (indie-org-webmentions-wm-sort-to-verb
                  (indie-org-webmentions-received-wm-sort wm)))
                (url (indie-org-webmentions-received-wm-url wm))
                (recvd
                 (format-time-string
                  "%A, %B %e %Y, %H:%M"
                  (indie-org-webmentions-received-wm-time-received wm))))
           (format
            "<div>%s %s this post %s</div>"
            (if (and author-url (not (eq 0 (length author-url))))
                (format "<a href=\"%s\">%s</a>" author-url name)
              name)
            (if url
                (format "<a href=\"%s\">%s</a>" url verb)
              verb)
            recvd)))
       wms))
     "</div>\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              h-feed                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iosh/partial-h-feed-headline (headline contents info)
  "Transcode HEADLINE element into h-feed format.
CONTENTS is the headline contents.  INFO is a plist used as a
communication channel."
  ;; Implementation largely copied from ox-rss.el.
  (if (> (org-export-get-relative-level headline info) 1)
      (org-export-data-with-backend headline 'indie-org info)
    (let* ((title
            (indie-org-plain-text
		         (or (org-element-property :P_NAME headline)
			           (replace-regexp-in-string
			            org-bracket-link-regexp
			            (lambda (m) (or (match-string 3 m)
					                        (match-string 1 m)))
			            (org-element-property :raw-value headline)))
             info))
           (permalink (concat "posts/" (org-element-property :U_URL headline)))
           (dsc (org-element-property :P_SUMMARY headline))
           (pubdate (org-element-property :PUBDATE headline))
           (updated (org-element-property :UPDATED headline))
           (latest (or updated pubdate)))
      ;; `updated' is possibly nil
      (format
       (concat
        "<li class=\"h-entry hentry iosh-h-feed-li\">"
        "<span class=\"p-name\" style=\"display:none\">%s</span>"
        "<a href=\"%s\" class=\"u-url u-uid p-summary\">%s</a> "
        (if updated
            (format
             "updated <time class=\"dt-updated\">%s</time>, \
originally published "
             updated)
          "published ")
        (format
         "<time class=\"dt-published\">%s</time>"
         pubdate)
        "</li>")
       title
       permalink
       (string-trim dsc)))))

(defun iosh/partial-h-feed-section (section contents info)
  "Transcode SECTION element into RSS format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

(defun iosh/partial-h-feed-template (contents info)
  "Transcode CONTENTS to a partial h-feed.
INFO is a plist used as a communication channel."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
        (hfeed-name (plist-get info :hfeed-name))
        (hfeed-summary (plist-get info :description)))
    (concat
     (format "<!-- autogenerated %s -->\n" timestamp)
     "<ul class=\"h-feed hfeed iosh-div-h-feed\">\n"
     (if hfeed-name
         (format "<div class=\"p-name\" style=\"display:none\">%s</div>\n" hfeed-name))
     (if hfeed-summary
         (format "<p class=\"p-summary\" style=\"display:none\">%s</p>\n" hfeed-summary))
     (string-trim contents)
     "</ul>")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         templates for top-level pages                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iosh/page-html (contents info)
  "HTML template for top-level pages.
CONTENTS have already been transcoded.
INFO is a plist used as a communications channel.

This is a stripped-down version of `org-html-template' (with all
the HTML5 stuff ripped out)."
  (concat
   ;; Skip the xml line & just go straight to the doctype
   ;; <https://stackoverflow.com/questions/37592045/is-an-xml-declaration-necessary-in-an-html-document>
   (org-html-doctype info)
   "\n<html>\n<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   ;; Preamble
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Document title
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
                       (plist-get info :title)))
           (subtitle (plist-get info :subtitle)))
       (when title
         (format
          "<h1 class=\"title\">%s%s</h1>\n"
          (org-export-data title info)
          (if subtitle
              (format
               (concat "\n" (org-html-close-tag "br" nil info) "\n"
                       "<span class=\"subtitle\">%s</span>\n")
               (org-export-data subtitle info))
            "")))))

   contents

   ;; h-feed (home page only)
   (let* ((output-directory
           (file-name-as-directory
            (plist-get info :indie-org/publishing-root)))
          (output-file (plist-get info :output-file))
          (published-page-key
           (substring output-file (length output-directory))))
     (if (string-equal published-page-key "index.html")
         (concat "<div>" (plist-get info :h-feed) "</div>")
       ""))

   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Closing document.
   "</body>\n</html>"))

(defun iosh/page-postamble (info)
  "Return the HTML defining the page footer for top-level pages.
INFO is the communications channel describing the currentpage."
  (let* ((title (org-export-data (plist-get info :title) info)))
    (concat
     (if (string-equal title "indie-org.sh")
         "<div style=display:none class=\"h-card\">
<a class=\"u-url u-uid\" href=\"https://www.indie-org.sh\">
<img class=\"u-photo\" src=\"/img/avatar.png\" alt=\"\"/>
<span class=\"p-name\">Michael</span>
</a>
<p class=\"p-note\">Putting Org Mode on the Indieweb.</p>
<p>You can e-mail me at <a class=\"u-email\" href=\"mailto:indie-org@pobox.com\">indie-org@pobox.com</a></p>
<ul>
<li><a href=\"https://twitter.com/indieorgsh\" rel=\"me\">twitter.com/indieorgsh</a>
<li><a href=\"https://emacs.ch/@indieorg\" rel=\"me\">Mastodon</a>
<li><a href=\"/indie-org.sh-public.pgp\" rel=\"pgpkey\">PGP key</a>
</ul>
</div>"
       "")
     "Theme based on <a href=\"https://gitlab.com/OlMon/org-themes/-/tree/master/src/retro_dark\">Retro Dark</a>"
     (iosh/postamble info))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         templates for posts                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iosh//translate-post-type (post-type)
  "Translate POST-TYPE from a list of strings to a datastructure.

:post-type will come back as a (possibly empty) list of strings.
This method will interpret it to one of the following:

  - nil :: :post-type was not specified ({index,h-feed,rss}.org)
  - article :: a self-contained, long-form piece of writing
  - note :: a brief note, presumably POSSE'd
  - (reply . URL) :: a reply to a Tweet or Toot at URL
  - (like . URL) :: a like of a Tweet or Toot at URL
  - (repost . URL) :: a repost to a Tweet or Toot at URL

This method does no validation of other parameters; for instance,
it's illegal to specify POSSE targets for a reply (since it's
implicit), but this method makes no effort to enforce that."

  ;; `post-type' _can_ be nil-- if this is one of the site-map pages
  ;; ({index,rss,h-feed}.org). More than one, however, is an error.
  (unless (or (not post-type) (eq 1 (length post-type)))
    (error "Too many post-types specified"))

  (if (not post-type)
      nil
    (let ((post-type (car post-type)))
      (cond
       ((string= post-type "article") 'article)
       ((string= post-type "note") 'note)
       ((string-prefix-p "reply:" post-type)  (cons 'reply (substring post-type 6)))
       ((string-prefix-p "like:" post-type)   (cons 'like (substring post-type 5)))
       ((string-prefix-p "repost:" post-type) (cons 'repost (substring post-type 7)))
       (t (error "Unknown post-type %s" post-type))))))

(defun iosh/post-html (contents info)
  "HTML template for posts.
CONTENTS have already been transcoded.
INFO is a plist used as a communications channel.

Return the complete document string after HTML conversion for
`indie-org.sh` posts.

This is a stripped-down version of `org-html-template' (with all
the HTML5 stuff ripped out)."
  (let* ((page-key (indie-org-page-key-at-pub info))
         (filename (plist-get info :input-file))
         ;; 👇 `post-type' will be either a keyword or a cons cell
         (post-type
          (iosh//translate-post-type
           (org-publish-find-property filename :post-type info 'iosh/post-html)))
         ;; 👇  `toot-text' may be nil, depending on whether the post defined any
         (toot-text (plist-get info :toot-text))
         ;; 👇 `posse-targets' will be a list of POSSE targets (`twitter', `mastodon', &c)
         (posse-targets
          (mapcar
           (lambda (x) (indie-org-posse-string-to-target x))
           (org-publish-find-property filename :posse info 'indie-org)))
         (posse-requests (plist-get info :iosh/posse-requests))
         (with-title (plist-get info :with-title))
         (description (plist-get info :description)))

    ;; Let's validate some preconditions, first:
    (if post-type
        (cond
         ((eq post-type 'article)
          ;; We have an article.  Ok to POSSE.  "toot-text" only valid if POSSE
          ;; is present and contains one of mastodon or twitter. You must give
          ;; a title, whether it's displayed or not.
          (if (and toot-text (not (or (memq :twitter posse-targets) (memq :mastodon posse-targets))))
              (error "You specified tweet content without POSSE'ing to either Mastodon or Twitter"))
          (unless (plist-get info :title)
            (error "An article must have a title")))
         ((eq post-type 'note)
          ;; We have a note.  Ok to POSSE.  "toot-text" only valid if POSSE is
          ;; present and contains one of twitter or mastodon.
          (if (and toot-text (not (or (memq :mastodon posse-targets) (memq :twitter posse-targets))))
              (error "You specified tweet content without POSSE'ing to either Mastodon or Twitter")))
         ((memq (car post-type) '(reply like repost))
          ;; We have a note replying to something on Mastodon or Twitter.  Not
          ;; Ok to specify POSSE (it's implicit).  "toot-text" always valid.
          (if posse-targets
              (error "You cannot specify POSSE targets for a reply, like or repost"))
          (setq
           posse-targets
           ;; If the target is a tweet-- it's easy. Mastodon is a little tougher.
           (if (string-prefix-p "https://twitter.com" (cdr post-type)) '(:twitter) '(:mastodon))))
         (t (error "Unknown or missing post-type: %s" post-type))))1

    (let ((document
           (concat
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;                                head                                  ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;; Skip the xml line & just go straight to the doctype
            ;; <https://stackoverflow.com/questions/37592045/is-an-xml-declaration-necessary-in-an-html-document>
            (org-html-doctype info)
            "\n<html>\n<head>\n"
            (org-html--build-meta-info info)
            (org-html--build-head info)
            (org-html--build-mathjax-config info)
            "</head>\n"

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;                                body                                  ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            "<body>\n"
            ;; Preamble
            (org-html--build-pre/postamble 'preamble info)
            ;; Document contents.
            (let ((div (assq 'content (plist-get info :html-divs))))
              (format "<%s id=\"%s\" class=\"%s\">\n"
                      (nth 1 div)
                      (nth 2 div)
                      (plist-get info :html-content-class)))

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;              title/subtitle/description handling              ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;; We have three related page attributes:
            ;;            
            ;; - title :: p-name, #+TITLE, :title, display in the document is
            ;;     controlled by org-export-with-title/:with-title
            ;; - subtitle :: no mf2 property, #+SUBTITLE, :subtitle, display in
            ;;     the document is controlled by
            ;;     org-export-with-title/:with-title, introduced by the 'html
            ;;     backend
            ;; - description :: p-summary, #+DESCRIPTION, :description, just
            ;;     used in the META tags in the default HTML template whereas
            ;;     we use it in the h-card

            ;; How we handle it on this page depends on the post type:

            ;; - nil :: if :with-title is on, and if there is a :title, display it
            ;;     ignore the subtitle & description (if any)
            ;; - article :: you MUST have a title and you MAY specify a
            ;;     sub-title. If :with-title is on, it will be displayed in the
            ;;     markup. If :with-title is off the title will still go into
            ;;     the mf as p-name. Ignore the description.
            ;; - else :: if :with-title is on, and if there's a :subtitle,
            ;;     display it; ignore the title & description (if any)
            (let ((title (plist-get info :title))
                  (subtitle (plist-get info :subtitle)))
              (cond
               ((not post-type)
                (if (and with-title title)
                    (format "<h1 class=\"title p-name\">%s</h1>\n" (org-export-data title info))))
               ((eq post-type 'article)
                (if with-title
                    (progn
                      (format
                       "<h1 class=\"title p-name\">%s</h1>\n%s"
                       (org-export-data title info)
                       (if subtitle
                           (format
		                        (concat "\n" (org-html-close-tag "br" nil info) "\n"
			                              "<span class=\"subtitle\">%s</span>\n")
	                          (org-export-data subtitle info))
                         "")))))
               (t
                (if (and with-title subtitle)
                    (format
                     "<span class=\"subtitle\">%s</span>\n"
                     (org-export-data subtitle info))))))

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;                          h-card                               ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            (let* ((date (org-timestamp-to-time (car (plist-get info :date))))
                   (timestamp (format-time-string "%Y-%m-%d %H:%M:%S" date))
                   (datestamp (format-time-string "%B %e %Y" date))
                   (permalink (concat
                               (plist-get info :html-link-up)
                               "/"
                               (file-name-nondirectory (plist-get info :output-file))))
                   (summary
                    (if description
                        (format "<span class=\"p-summary\">%s</span>" description)
                      "")))
              (format
               "<p style=display:none class=\"h-card\">Published by <a class=\"p-author\" href=\"%s\">indie-org.sh <img class=\"u-photo\" src=\"/img/avatar.jpg\" alt=\"\"/></a>.
on <time class=\"dt-published\" datetime=\"%s\">%s</time>. <a href=\"%s\" class=\"u-url\">permalink</a>%s
<br><span class=\"p-note\">Putting Org Mode on the Indieweb.</span>
</p>
"
               (plist-get info :h-card-link) timestamp datestamp permalink
               summary))

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;                         contents                              ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            "<div class=\"e-content\">"

            (if (and (listp post-type) (memq (car post-type) '(reply repost like)))
                (let* ((type (car post-type))
                       (link (cdr post-type))
                       (components (indie-org-posse-parse-link link))
                       (name (nth 0 components))
                       (dsc (nth 2 components)))
                  (cond
                   ((eq type 'reply)
                    (format "Replying to %s's <a href=\"%s\" class=\"u-in-reply-to\">%s</a>: " name link dsc))
                   ((eq type 'repost)
                    (format "Reposting %s's <a href=\"%s\" class=\"u-in-repost-of\">%s</a>: " name link dsc))
                   ((eq type 'like)
                    (format "Liking %s's <a href=\"%s\" class=\"u-in-like-of\">%s</a>: " name link dsc)))))

            contents

            (if toot-text
                (concat
                 (format "<p class=\"p-bridgy-twitter-content\" style=\"display:none\">%s</p>" toot-text)
                 "\n"
                 (format "<p class=\"p-bridgy-mastodon-content\" style=\"display:none\">%s</p>" toot-text)))

            "</div>"

            ;; Close the document content div here:
            (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))

            (if (memq post-type '(article note))
                (concat
                 "<div class=\"iosh-send-wm-div\">\n"
                 "<br>"
                 "<form class=\"webmention-form\" action=\"https://webmention.io/indie-org.sh/webmention\" method=\"post\">\n"
                 "<label>Have you written a <a href=\"https://indieweb.org/responses\">response</a> to this? Let me know the URL: </label>"
                 "<input type=\"url\" name=\"source\">&nbsp;"
                 "<input type=\"submit\" value=\"Send Webmention\">\n"
                 (format
                  "<input type=\"hidden\" name=\"target\" value=\"https://indie-org.sh/%s\">\n"
                  (indie-org-page-key-at-pub info))
                 "</form>\n"
                 "</div>\n"))

           ;; Postamble.
            (org-html--build-pre/postamble 'postamble info)
            ;; Closing document.
            "</body>\n</html>")))
      ;; If this post is being POSSE'd anywyere, note that now.
      (if posse-targets (indie-org-record-posse-request posse-targets page-key posse-requests))
      document)))

(defun iosh/post-postamble (info)
  "Return the HTML defining the page footer for posts.
INFO is the communications channel describing the currentpage."
  (let* ((posse-responses
          (indie-org-pub-posse-responses-at-pub info (plist-get info :iosh/posse-responses))))
      (concat
       "<a href=\"../index.html\">home</a>"     "<div class=\"iosh-wm-div\">\n"
       (if posse-responses
           (concat
            "<br>\n<div class=\"iosh-syndications\">\n"
            "View on "
            (string-join
             (mapcar
              (lambda (rsp)
                (concat
                 "<a class=\"u-syndication\" rel=\"syndication\" href=\""
                 (indie-org-posse-response-url rsp)
                 "\">"
                 (indie-org-posse-target-to-string
                  (indie-org-posse-response-sort rsp))
                 "</a>"))
              posse-responses)
             ",")
            "\n</div>\n"))
       ;; Add empty links for POSSE through brid.gy
       "\n<a href=\"https://brid.gy/publish/mastodon\"></a>\n"
       "<a href=\"https://brid.gy/publish/twitter\"></a>\n"
       "<a href=\"https://brid.gy/publish/github\"></a>\n"
       "<a href=\"https://brid.gy/publish/flickr\"></a>\n"
       "</div>\n"
       (iosh/postamble info))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             utility functions                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iosh/publish-page (plist filename pub-dir backend)
  "Publish an indie-org.sh page or post to HTML.
FILENAME is the filename of the Org file to be published.
PLIST is the property list for the given project.
PUB-DIR is the publishing directory.
BACKEND is the backend to use.

Return output file name.

Use this as the :publishing-function to use a non-'html backend."
  (let ((h-feed
         (with-temp-buffer
           (find-file
            (concat
             (file-name-as-directory
              (plist-get plist :publishing-directory))
             "h-feed.html"))
           (buffer-string))))
    (plist-put plist :h-feed h-feed)
    (org-publish-org-to
     backend
     filename
     ;; This curious logic is copied directly from `org-html-publish-to-html'.
     ;;
     ;; `org-publish-org-to' explicitly demands a leading dot in this argument
     ;; "EXTENSION is the extension used for the output string, with the leading
     ;; dot." Commit 0c72a1c4138c5d65fe87cfe44b800f668cee1843 contains the
     ;; comment "ox-html.el: No trailing dot when HTML extension is empty"
     ;;
     ;; I *suspect* what happened here is that Bastien's commit was intended
     ;; to add the leading dot _when `org-html-extension' was given (non-nil),
     ;; but to avoid an EXTENSION argument of "." when it was nil (i.e. an
     ;; explicit request of no extension).
     ;;
     ;; Then the project property :html-extension was introduced/incorporated
     ;; here without updating this logic. As-is, if `org-html-extension' is nil,
     ;; but the project gives an :html-extension of, say, "html", the EXTENSION
     ;; argument will be "html"-- incorrect.

     ;; I strongly suspect this is incorrect, but leaving it for now for
     ;; consistency's sake.
	   (concat (when (> (length org-html-extension) 0) ".")
			       (or (plist-get plist :html-extension)
				         org-html-extension
				         "html"))
	   plist
     pub-dir)))

(defun iosh/post/filter-parse-tree (parse-tree _back-end info)
  "Edit the parse tree prior to export.
PARSE-TREE is the org syntax tree.
INFO is the communications channel used during publication.

Remove any heading that is tagged :toot-text:.  Insert the
text in the following paragraph into INFO as :toot-text.

This implementation is brittle: the logic will trigger when we
see a `headline' with contents `paragraph'.  There is no check
against multiple matches."
  (let* ((properties (nth 1 parse-tree))
	       (contents (cddr parse-tree))
	       (new-contents
	        (cl-remove-if
	         (lambda (item)
             (if (eq 'headline (org-element-type item))
		             (let ((raw-value (org-element-property :raw-value item)))
		               (if (and (stringp raw-value) (string-match ":toot-text:" raw-value))
		                   (let* ((contents1 (car (org-element-contents item)))
                              (contents2 (car (org-element-contents contents1))))
			                   (if (eq 'paragraph (org-element-type contents2))
			                       (let* ((contents (org-element-interpret-data contents2))
				                            (toot-text (substring-no-properties contents)))
                               (plist-put info :toot-text toot-text))))))))
	         contents)))
    (cons 'org-data (cons properties new-contents))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              functions meant to be invoked from the Makefile              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iosh/publish (prod)
  "Publish indie-org.sh to production if PROD is non-nil, locally else."
  (indie-org-enable)
  ;; Define a back-end for indie-org.sh generic pages (not a post of some
  ;; sort-- e.g. the home page).
  (org-export-define-derived-backend
      'iosh/page-html
      'indie-org
    ;; This will associate a template for the final HTML document with our new
    ;; backend. That's where we get to layout the document itself (head, body,
    ;; add headers & footers, and so forth)
    :translate-alist '((template . iosh/page-html)))
  ;; Define a back-end for indie-org.sh posts-- here's where we define "post
  ;; type".
  (org-export-define-derived-backend
      'iosh/post-html
      'indie-org
    ;; This will associate a template for the final HTML document with our new backend.
    :translate-alist '((template . iosh/post-html))
    ;; Introduce a new keyword, `POST-TYPE`
    :options-alist   '((:post-type "POST-TYPE" nil nil split))
    ;; This will be invoked after each buffer is parsed into Org elements and
    ;; before they are transcoded to HTML.  We use this to remove sections with
    ;; silo-specific text for POSSE.
    :filters-alist   '((:filter-parse-tree . (iosh/post/filter-parse-tree))))
  (org-export-define-derived-backend
      'iosh/h-feed-html
      'html
    :translate-alist '((headline . iosh/partial-h-feed-headline)
                       (section  . iosh/partial-h-feed-section)
                       (template . iosh/partial-h-feed-template)))
  ;; Load the site's publication state(s)
  (let* ((env (if prod :prod :local))
         (link-home
          (if prod "https://indie-org.sh/posts" "."))
         (pub-states
          (if (file-exists-p publication-state-file)
              (indie-org-state-read publication-state-file)
            `(:prod ,(indie-org-state-make) :local ,(indie-org-state-make))))
         (publication-state (plist-get pub-states env))
         (webmentions-made
          (or (indie-org-state-v2-webmentions-made publication-state)
              (indie-org-webmentions-make-made)))
         (webmentions-received
          (or (indie-org-state-v2-webmentions-received publication-state)
              (indie-org-webmentions-make-received)))
         (posse-requests
          (or (indie-org-state-v2-posse-requests publication-state)
              (indie-org-posse-make-requests)))
         (posse-responses
          (or (indie-org-state-v2-posse-responses publication-state)
              (indie-org-posse-make-responses))))
    ;; This is all to setup the call to `org-publish-all'.
    (let* ((publishing-root (concat (file-name-as-directory project-dir) "www"))
           (included-posts
            (indie-org-pub-find-posts
             (concat (file-name-as-directory project-dir) "posts") prod
             :exclude '("index.org" "rss.org" "h-feed.org")))
           (org-publish-timestamp-directory (concat project-dir ".org-timestamps/"))
           (org-publish-use-timestamps-flag nil)
           (org-html-head-include-default-style nil)
           (link-home
            (if prod "https://indie-org.sh/posts" "/posts"))
           (rss-project
            (indie-org-rss-enable-rss-2.0
             (cons
              "rss"
              (list
               :base-directory              (concat project-dir "posts")
               :exclude                     ".*\\.org$"
               :html-link-home              link-home
               :html-link-org-files-as-html t
               :html-link-use-abs-url       t
               :include                     included-posts
               :indie-org/publishing-root   publishing-root
               :publishing-directory        (concat project-dir "www/")))
             "rss.org"
             "indie-org.sh"
             "Putting Org Mode on the Indieweb"))
           (org-publish-project-alist
            `(("indie-org.sh" :components ("h-feed" "pages" "posts" "rss"))
              ("h-feed"
               :auto-sitemap                t
               :base-directory              ,(concat project-dir "posts")
               :description                 "Putting Org Mode on the Indieweb"
               :exclude                     ".*\\.org$"
               :h-feed-backend              iosh/h-feed-html
               :hfeed-name                  "indie-org.sh"
               :html-link-org-files-as-html t
               :html-link-use-abs-url       t
               :include                     ,included-posts
               :indie-org/publishing-root   ,publishing-root
               :indie-org/webmentions-made  ,webmentions-made
               :publishing-directory        ,(concat project-dir "www/")
               :publishing-function         indie-org-h-feed--publish
               :sitemap-filename            "h-feed.org"
               :sitemap-format-entry        indie-org-h-feed--format-entry
               :sitemap-function            indie-org-h-feed--format-sitemap
               :sitemap-sort-files          anti-chronologically
               :with-toc                    nil)
              ("pages"
               :base-directory             ,(concat project-dir "pages")
               :html-doctype               "html4-strict"
               :html-head-extra            ,(iosh/extra-head 0)
               :html-postamble             iosh/page-postamble
               :publishing-directory       ,(concat project-dir "www")
               ;; The outer list is a work-around to a peculiarity (bug?) in `org-publish-file'-- in the `pcase'
               ;; statment, `(pred lisp)' will evaluate to true for the resulting lambda, which is not what
               ;; we want. Wrapping in a list will make it work.
               :publishing-function        ,(list
                                             (lambda (plist filename pub-dir)
                                               (iosh/publish-page plist filename pub-dir 'iosh/page-html)))
               :indie-org/webmentions-made ,webmentions-made
               :indie-org/publishing-root  ,publishing-root
               :iosh/posse-requests        ,posse-requests
               :iosh/posse-responses       ,posse-responses
               :iosh/webmentions-received  ,webmentions-received
               :section-numbers            nil
               :with-author                nil
               :with-toc                   nil)
              ("posts"
               :auto-sitemap               t
               :base-directory             ,(concat project-dir "posts")
               :exclude                    ".*\\.org$"
               :html-content-class         "content h-entry"
               :html-doctype               "html4-strict"
               :html-head-extra            ,(iosh/extra-head 1)
               :html-link-up               ,link-home
               :html-postamble             iosh/post-postamble
               :publishing-directory       ,(concat project-dir "www/posts")
               :publishing-function        ,(list
                                             (lambda (plist filename pub-dir)
                                               (iosh/publish-page plist filename pub-dir 'iosh/post-html)))
               :include                    ,included-posts
               :indie-org/webmentions-made ,webmentions-made
               :indie-org/publishing-root  ,publishing-root
               :iosh/posse-requests        ,posse-requests
               :iosh/posse-responses       ,posse-responses
               :iosh/webmentions-received  ,webmentions-received
               :sitemap-filename           "index.org"
               :sitemap-sort-files         anti-chronologically
               :sitemap-title              "Posts"
               :with-author                nil
               :with-date                  t
               :with-title                 t
               :with-toc                   nil)
              ,rss-project)))
      ;; OK-- do it!
      (org-publish-all t)
      ;; During the course of publication, we may have updated state: in
      ;; particular, we may have made new webmentions & POSSE requests.  Update
      ;; the publication state's most recent publication time...
      (setf (indie-org-state-v2-last-published publication-state)   (current-time)
            ;; along with new requests:
            (indie-org-state-v2-webmentions-made publication-state) webmentions-made
            (indie-org-state-v2-posse-requests publication-state)   posse-requests)
      (plist-put pub-states env publication-state)
      ;; and write it all out to disk.
      (indie-org-state-write pub-states publication-state-file))))

(defun iosh/send-webmentions (prod)
  "Send webmentions post-publication.
PROD shall be set to t for production and nil for local."
  (message "Sending webmentions (if any)...")
  (indie-org-enable)
  (let* ((env (if prod :prod :local))
         (all-pub-states
          (if (file-exists-p publication-state-file)
              (indie-org-state-read publication-state-file)))
         (publication-state
          (or (plist-get all-pub-states env)
              (indie-org-state-make)))
         (webmentions-made
          (or (indie-org-state-v2-webmentions-made publication-state)
              (indie-org-webmentions-make-made)))
         (webmentions-sent
          (or (indie-org-state-v2-webmentions-sent publication-state)
              (indie-org-webmentions-make-sent)))
         (webmentions-to-send
          (indie-org-webmentions-required webmentions-made webmentions-sent))
         (token
          (indie-org-read-token
           telegraph-io-token-file))
         (now (current-time)))
    (while webmentions-to-send
      (let* ((wm (car webmentions-to-send))
             (location
              (if prod
                  ;; The source is just the page key
                  ;; (e.g. "blog/foo.html")-- it is only here that we
                  ;; know we're sending this for prod, so prepend the
                  ;; "https://www.unwoundstack.com" here:
                  (let ((src (car wm))
                        (dst (cdr wm)))
                    (indie-org-webmentions-send
                     (cons (concat "https://indie-org.sh/" src) dst)
                     token))
                (message "Will send webmention: %s :=> %s" (car wm) (cdr wm))
                nil)))
        (indie-org-webmentions-record-sent
         wm
         now
         webmentions-sent
         location))
      (setq webmentions-to-send (cdr webmentions-to-send)))
    (setf (indie-org-state-v2-webmentions-sent publication-state) webmentions-sent)
    (plist-put all-pub-states env publication-state)
    (indie-org-state-write all-pub-states publication-state-file)
      (message "Sending webmentions (if any)...done.")))

(defun iosh/check-webmentions ()
  "Check for new webmentions; update publication state."
  (indie-org-enable)
  (let* ((env :prod) ;; no staging support ATM
         (publication-state
          (if (file-exists-p publication-state-file)
              (indie-org-state-read publication-state-file)))
         (state-for-env
          (or (plist-get publication-state env) (indie-org-state-make)))
         (webmentions-received
          (or (indie-org-state-v2-webmentions-received state-for-env)
              (indie-org-webmentions-make-received)))
         (token
          (indie-org-read-token
           webmentions-io-token-file)))
    (indie-org-webmentions-check "indie-org.sh" token webmentions-received)
    (setf (indie-org-state-v2-webmentions-received state-for-env) webmentions-received)
    (plist-put publication-state env state-for-env)
    (indie-org-state-write publication-state publication-state-file)))

(defun iosh/send-posse-requests (prod)
  "Send POSSE requests post-publication.
PROD shall be t to select production & nil to select staging."
  (message "Sending POSSE requests (if any)...")
  (indie-org-enable)
  (let* ((env (if prod :prod :local))
         (all-pub-states
          (if (file-exists-p publication-state-file)
              (indie-org-state-read publication-state-file)))
         (publication-state
          (or (plist-get all-pub-states env)
              (indie-org-state-v2-make-publication-state)))
         (requests
          (or (indie-org-state-v2-posse-requests publication-state)
              (indie-org-posse-make-requests)))
         (responses
          (or (indie-org-state-v2-posse-responses publication-state)
              (indie-org-posse-make-responses)))
         (to-send
          (indie-org-posse-required requests responses)))
    ;; `to-send' will be a list of cons cells, each of whose car is a
    ;; page key & whose cdr is a list of POSSE symbols.
    (while to-send
      (let* ((page-key (caar to-send))
             (source
              (concat
               "https://indie-org.sh/"
               page-key))
             (symbols (cdar to-send)))
        (while symbols
          (if prod
              (let ((rsp (indie-org-send-posse-request source (car symbols))))
                (indie-org-record-sent-posse page-key rsp responses))
            (message "Will send POSSE request: %s :=> %s" source (car symbols)))
          (setq symbols (cdr symbols))))
      (setq to-send (cdr to-send)))
    (setf (indie-org-state-v2-posse-responses publication-state) responses)
    (plist-put all-pub-states env publication-state)
    (indie-org-state-write all-pub-states publication-state-file))
    (message "Sending POSSE requests (if any)...done."))

(defun iosh/show-state ()
  "Pretty-print indie-org.sh publication states."
  (indie-org-enable)
  (let* ((publication-state-file
          (concat (file-name-as-directory project-dir) ".state"))
         (publication-state
          (if (file-exists-p publication-state-file)
              (indie-org-state-read publication-state-file))))
    (indie-org-state-pp publication-state)))

(defun iosh/show-drafts ()
  "Pretty-print draft posts."
  (indie-org-enable)
  (indie-org-pub-pp-drafts
   (concat (file-name-as-directory project-dir) "posts")))

(provide 'indie-org.sh)
;;; indie-org.sh.el ends here
