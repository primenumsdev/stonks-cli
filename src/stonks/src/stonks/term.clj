(ns stonks.term
  (:refer-clojure :exclude [print printf println read-line newline *in* *out* flush])
  (:require [clojure.string :as str])
  (:import (java.io File InputStreamReader OutputStreamWriter BufferedReader)
           (java.lang ProcessBuilder ProcessBuilder$Redirect)
           (java.util ArrayList List Vector)
           (java.nio.charset Charset)))

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
(defonce ^:private tty (File. "/dev/tty"))

;; stty is a tool used to set the input and output settings for the terminal communication interface
;; https://man7.org/linux/man-pages/man1/stty.1.html
(defonce ^:private stty "/bin/stty")

(def ^:dynamic ^BufferedReader *stdin* (BufferedReader. (InputStreamReader. System/in)))

(def ^:dynamic ^OutputStreamWriter *stdout* (OutputStreamWriter. System/out ^Charset (Charset/defaultCharset)))

(def ^String sys-newline
  (System/getProperty "line.separator"))

(defn flush []
  (.flush *stdout*))

(defn append [^CharSequence csq]
  (.append *stdout* csq))

(defn write [^String txt]
  (.write *stdout* txt 0 ^Integer (.length txt)))

(defn newline []
  (append sys-newline)
  (flush))

(defn print [txt]
  (write txt)
  (flush))

(defn printf [fmt & args]
  (print (apply format fmt args)))

(defn println [txt]
  (write txt)
  (append sys-newline)
  (flush))

(defn read-line []
  (.readLine *stdin*))

(defn with-csi [code]
  (str ESC "[" code))

(defn csi
  "Control Sequence Introducer."
  [code]
  (print (with-csi code)))

(defn bell
  "Makes bell sound."
  []
  (.write *stdout* 7)
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

(defn move-cursor-to-col [x]
  (csi (str x "G")))

(defn erase-line-to-cursor []
  (csi "1K"))

(defn erase-line-after-cursor []
  (csi "0K"))

(defn erase-cur-line []
  (csi "2K"))

(defn cls
  "Clears screen and moves cursor to top left corner."
  []
  (csi "2J")
  (move-cursor))

(defn with-fg-color [id txt]
  (str (with-csi (str "38;5;" id "m" txt))
       (with-csi "0m")))

(defn set-fg-color [id]
  (csi (str "38;5;" id "m")))

(defn reset-fg-color []
  (csi "0m"))

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
  (let [cmds (ArrayList. ^Vector (vec (cons stty cmds-vec)))]
    (->
      (ProcessBuilder. ^List cmds)
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
      (while (not (.ready *stdin*))
        (Thread/sleep 250))
      (reset! c (.read *stdin*))
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
  (char (.hashCode (str/upper-case (char (.read *stdin*))))))

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

;; frame = [ bar , color ]
(defonce default-spinner-frames [["▰▱▱▱▱▱▱" 22]
                                 ["▰▰▱▱▱▱▱" 23]
                                 ["▰▰▰▱▱▱▱" 28]
                                 ["▰▰▰▰▱▱▱" 29]
                                 ["▰▰▰▰▰▱▱" 34]
                                 ["▰▰▰▰▰▰▱" 35]
                                 ["▰▰▰▰▰▰▰" 40]])

(defonce default-spinner-frame-timeout 125)

(defonce default-spinner-color 40)

(defn spinner [{:keys [msg loading-fn? frames timeout color]
                :or   {frames  default-spinner-frames
                       timeout default-spinner-frame-timeout
                       color   default-spinner-color}}]
  (while (loading-fn?)
    (doseq [[bar bar-color] frames]
      (let [bar-color (if (some? bar-color) bar-color color)]
        (print-cr (str (with-fg-color color msg) " " (with-fg-color bar-color bar)))
        (Thread/sleep timeout))))
  (erase-cur-line)
  (move-cursor-to-col 1))

;; https://github.com/spectreconsole/spectre.console/blob/main/src/Spectre.Console/Rendering/Borders/Tables/SquareTableBorder.cs
(defn table
  ([ks rows & {:keys [header-color row-color-1 row-color-2]
               :or   {header-color 40
                      row-color-1  40
                      row-color-2  28}}]
   (when (seq rows)
     (let [widths     (map
                        (fn [k]
                          (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                        ks)
           spacers    (map #(apply str (repeat % "─")) widths)
           fmts       (map #(str "%" % "s") widths)

           fmt-fn     (fn [color]
                        (fn [leader divider trailer row]
                          (str leader
                               (apply str (interpose divider
                                                     (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                       (if (some? color)
                                                         (with-fg-color color (format fmt (str col)))
                                                         (format fmt (str col))))))
                               trailer)))

           fmt-border (fmt-fn nil)
           fmt-header (fmt-fn header-color)
           fmt-row-1  (fmt-fn row-color-1)
           fmt-row-2  (fmt-fn row-color-2)]
       (newline)
       ;; header
       (println (fmt-border "┌─" "─┬─" "─┐" (zipmap ks spacers)))
       (println (fmt-header "│ " " │ " " │" (zipmap ks ks)))
       (println (fmt-border "├─" "─┼─" "─┤" (zipmap ks spacers)))
       ;; body
       (->> rows
            (map-indexed (fn [i row]
                           (if (even? i)
                             (println (fmt-row-1 "│ " " │ " " │" row))
                             (println (fmt-row-2 "│ " " │ " " │" row)))))
            doall)
       ;; footer
       (println (fmt-border "└─" "─┴─" "─┘" (zipmap ks spacers))))))
  ([rows] (table (keys (first rows)) rows)))

(comment

  (table
    [{:name "test"}
     {:name "tes2"}]
    )

  (print (str (with-fg-color 28 "test")
              " and normal"
              (with-fg-color 57 "anothrr")
              ))
  (print "test")

  )
