;;; .itunes.el --- Package to interact with iTunes & Apple music from within Emacs

;;; Commentary:
;;

;;; Code:
(require 'dash)

(defvar -itunes-playlist "Emacs - temporary"
  "The name of the playlist to create for temporary housing tracks.")

(defun -itunes-cmd (cmd)
  "Convert CMD into a command string for shell execution."
  (funcall
   (-compose
    (-partial 's-replace "{PLAYLIST}" -itunes-playlist)
    (-partial 'format "osascript -e 'tell application \"iTunes\"' %s -e 'end tell'")
    (-partial 's-join " ")
    (-partial '-map (-partial 'format "-e '%s'")))
   cmd))

(defun -itunes-tell (cmd)
  "Run osascript to tell iTunes application CMD."
  (shell-command (-itunes-cmd cmd)))

(defun -itunes-run (cmd &optional uniq)
  "Run osascript to tell application CMD.
When UNIQ is non-nil, return a unique list of items."
  (funcall
   (-compose
    (if uniq '-uniq 'identity)
    (-partial '-map 'capitalize)
    (-partial 's-split ", ")
    's-trim
    'shell-command-to-string)
   (-itunes-cmd cmd)))

(defun -itunes-clear-playlist ()
  "Clear the temporary playlist `-itunes-playlist' or create it."
  (-itunes-tell
   '(
     "if user playlist \"{PLAYLIST}\" exists then"
     "  try"
     "    delete tracks of user playlist \"{PLAYLIST}\""
     "  end try"
     "else"
     "  make new user playlist with properties {name:\"{PLAYLIST}\"}"
     "end if"
     )))

(defmacro defitunes-get (type &optional uniq)
  "Macro to define function to get list TYPE items from iTunes.
When UNIQ is non-nil, return a uniq list."
  `(defun ,(intern (format "itunes-get-%s" type)) ()
     ,(format "Get a list of every %s from your itunes library" type)
     (interactive)
     (-itunes-run
      '(,(format "%s of (every track of playlist \"Library\") as list" type))
      ,uniq)))

(defitunes-get "artist" t)
(defitunes-get "album" t)
(defitunes-get "genre" t)

(defun itunes-get-songs ()
  "WIP.  Get all songs from iTunes with meta data."
  (interactive)
  (let ((names (-itunes-run '("name of (every track of playlist \"Library\")")))
        (albums (-itunes-run '("album of (every track of playlist \"Library\")")))
        (artists (-itunes-run '("artist of (every track of playlist \"Library\")"))))
    (-zip-lists names artists albums)))

(defun itunes-get-albums-for-artist (artist)
  "Get a list of albums for ARTIST."
  (-itunes-run
   `(,(format "album of (every track of playlist \"Library\" whose artist is \"%s\") as list" artist))
   t))

(defun itunes-get-tracks-for-album (album)
  "Get a list of tracks for ALBUM."
  (-zip-lists
   (-itunes-run
    `(,(format "name of (every track of playlist \"Library\" whose album is \"%s\") as list" album)))
   (-itunes-run
    `(,(format "duration of (every track of playlist \"Library\" whose album is \"%s\") as list" album)))))

(defun itunes-get-album-summary (album)
  "Get summary data about an ALBUM."
  (let* ((aInfo (itunes-get-tracks-for-album album))
         (aLength (length aInfo))
         (aPPTracks (format "%s tracks" aLength))
         (aDuration (-sum (-map (-compose 'string-to-number 'cadr) aInfo))))
    (pcase-let* ((`(,seconds ,minutes ,hours) (decode-time aDuration))
                 (aPPDuration (if (= 0 (1- hours))
                                  (format "%s mins" minutes)
                                (format "%s hr %s mins" hours minutes))))
      (list aLength aPPTracks aDuration aPPDuration))))

(defun -itunes-add-artist (artist)
  "Clear playlist and add all tracks for ARTIST to `-itunes-playlist'."
  (-itunes-clear-playlist)
  (-itunes-tell
   `(
     ,(format "set S to every track of playlist \"Library\" whose artist is \"%s\"" artist)
     "repeat with s in S"
     "  duplicate s to the end of user playlist \"{PLAYLIST}\""
     "end repeat"
     )))

(defun -itunes-add-album (album)
  "Clear playlist and add all tracks for ALBUM to `-itunes-playlist'."
  (-itunes-clear-playlist)
  (-itunes-tell
   `(
     ,(format "set S to every track of playlist \"Library\" whose album is \"%s\"" album)
     "repeat with s in S"
     "  duplicate s to the end of user playlist \"{PLAYLIST}\""
     "end repeat"
     )))

(defun -itunes-play-artist (artist)
  "Add tracks for ARTIST and then play `-itunes-playlist'."
  (-itunes-add-artist artist)
  (-itunes-tell '("play user playlist \"{PLAYLIST}\"")))


(defun -itunes-play-album (album)
  "Add tracks for ALBUM and then play `-itunes-playlist'."
  (-itunes-add-album album)
  (-itunes-tell '("play user playlist \"{PLAYLIST}\"")))

(defun itunes-play ()
  "Interactive completing form of `-itunes-play-artist'."
  (interactive)
  (let ((candidates (itunes-get-artist)))
    (ivy-read "Play artist: " candidates
              :action (lambda (x) (-itunes-play-artist x)))))

(defun itunes-play-album ()
  "Interactive completing form of `-itunes-play-album'."
  (interactive)
  (let* ((artist-candidates (itunes-get-artist))
         (artist (ivy-completing-read "👩‍🎤 Artist: " artist-candidates))

         (album-candidates (itunes-get-albums-for-artist artist))
         (album (ivy-completing-read "💿 Album: " album-candidates)))
    (-itunes-play-album album)))

(provide 'itunes)
;;; .itunes.el ends here
