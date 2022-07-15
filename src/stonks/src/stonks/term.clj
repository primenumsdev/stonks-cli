(ns stonks.term
  (:require [clojure.string :as str])
  (:import (java.io File)
           (java.lang ProcessBuilder$Redirect)))

;; ANSI terminal interaction
;; https://en.wikipedia.org/wiki/ANSI_escape_code
;; https://www.lihaoyi.com/post/BuildyourownCommandLinewithANSIescapecodes.html

;; Here we use CSI Control Sequence Introducer
;; Cheat sheet
;; https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
;; Gist
;; https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797

(defonce ^:private ESC (char 27))

;; /dev/tty is a special file, representing the terminal for the current process.
;; So, when you echo 1 > /dev/tty, your message ('1') will appear on your screen.
;; Likewise, when you cat /dev/tty, your subsequent input gets duplicated (until you press Ctrl-C).
;
;; /dev/tty doesn't 'contain' anything as such, but you can read from it and write to it (for what it's worth).
;; I can't think of a good use for it, but there are similar files which are very useful for simple IO operations
;; (e.g. /dev/ttyS0 is normally your serial port)
(def tty (File. "/dev/tty"))

;; stty is a tool used to set the input and output settings for the terminal communication interface
;; https://man7.org/linux/man-pages/man1/stty.1.html
(def stty "/bin/stty")

(defn csi
  "Control Sequence Introducer."
  [code]
  (.write *out* (str ESC "[" code))
  (flush))

(defn bell
  "Makes bell sound."
  []
  (.write *out* 7)
  (flush))

(defn move-cursor
  "
  Moves the cursor to row n, column m.
  The values are 1-based, and default to 1 (top left corner) if omitted.
  CSI n ; m H, where n - row, m - column.
  "
  ([] (csi ";H"))
  ([x y]
   (csi (str y ";" x "H"))))

(defn cls
  "Clears screen and moves cursor to top left corner."
  []
  (csi "2J")
  (move-cursor))

(defn print-at
  "
  Prints text at specific coordinates x, y, starting from the top left corner of terminal window.
  CSI n ; m H
  n - row, m - col
  "
  [x y txt]
  (csi (str y ";" x "H" txt)))

(defn print-cr
  "Prints and return cursor to initial position."
  [txt]
  (print (str "\r" txt)))

(defn save-cursor
  "Saves cursor position to allow restore it later."
  []
  (csi "s"))

(defn restore-cursor
  "Restores previously saved cursor position."
  []
  (csi "u"))

(defn run-ssty!
  "Runs /bin/stty command."
  [cmds-vec]
  (let [cmds (into-array (cons stty cmds-vec))]
    (->
      (ProcessBuilder. cmds)
      (.redirectInput (ProcessBuilder$Redirect/from tty))
      .start
      .waitFor)))

(defn switch-canonical!
  "Turn on/off canonical mode for the next input read."
  [enabled?]
  (run-ssty! [(if enabled? "icanon" "-icanon")]))

(defn switch-echo!
  "Turn on/off echo for current terminal session."
  [enabled?]
  (run-ssty! [(if enabled? "echo" "-echo")]))

(defn get-cursor-position []
  ;; turn off canonical and echo
  (run-ssty! ["-echo" "-icanon"])
  (csi "6n")
  (let [res (StringBuilder.)
        c   (atom nil)]
    ;; reports as ESC[#;#R
    ;; (char 82) => \R
    (while (not= @c 82)
      ;; wait till has something to read
      (while (not (.ready *in*))
        (Thread/sleep 250))
      (reset! c (.read *in*))
      ;; (char 27) => \ESC
      (if (= @c 27)
        (.append res "^")
        (.append res (char @c))))
    ;; enable echo back
    (switch-echo! true)
    (let [parts (-> res
                    .toString
                    (str/replace "^[" "")
                    (str/replace "R" "")
                    (str/split #";"))]
      [(Integer/parseInt (second parts))
       (Integer/parseInt (first parts))])))

(defn read-secret-line []
  (switch-echo! false)
  (let [secret (read-line)]
    (switch-echo! true)
    secret))

(defn prompt-secret [msg]
  (println msg)
  (read-secret-line))

(defn read-big-dec []
  (BigDecimal. (str (read-line))))

(defn read-int []
  (Integer/parseInt (str (read-line))))

(defn read-char
  "Reads char from System.in and convert to upper case (default ASCII) char."
  []
  (char (.hashCode (str/upper-case (char (.read *in*))))))

(defn prompt-str [msg]
  (println msg)
  (read-line))

(defn prompt-int [msg]
  (println msg)
  (try
    (read-int)
    (catch NumberFormatException _
      (println "Invalid number format, try again.")
      (prompt-int msg))))

(defn prompt-big-dec [msg]
  (println msg)
  (try
    (read-big-dec)
    (catch NumberFormatException _
      (println "Invalid number format, try again.")
      (prompt-big-dec msg))))