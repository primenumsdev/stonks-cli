(ns stonks.term
  (:refer-clojure :exclude [*in* *out* flush newline print printf println read-line])
  (:require [clojure.string :as str])
  (:import (java.io BufferedReader File InputStreamReader OutputStreamWriter)
           (java.lang ProcessBuilder ProcessBuilder$Redirect)
           (java.nio.charset Charset)
           (java.util ArrayList List Vector)))

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

(defn with-bg-color [id txt]
  (str (with-csi (str "48;5;" id "m" txt))
       (with-csi "0m")))

(defn set-fg-color [id]
  (csi (str "38;5;" id "m")))

(defn set-bg-color [id]
  (csi (str "48;5;" id "m")))

(defn reset-colors []
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

(defn zip [c1 c2]
  (map vector c1 c2))

(defn distribute [total ratios]
  (let [ratios          (map float ratios)
        total-ratio     (volatile! (reduce + ratios))
        total-remaining (volatile! total)
        res             (volatile! [])
        mins            (repeat (count ratios) 0)]
    (doseq [[ratio minimum] (zip ratios mins)]
      (let [distributed (if (> @total-ratio 0)
                          (max minimum
                               (int (Math/round ^float (* ratio (/ @total-remaining @total-ratio)))))
                          @total-remaining)]
        (vswap! res conj distributed)
        (vswap! total-ratio - ratio)
        (vswap! total-remaining - distributed)))
    @res))

(defn hlegend [legend-map width]
  (print "│ ")
  (let [left-over-space (volatile! width)]
    (doseq [[title {:keys [percent color]}] legend-map
            :let [icon            (str (with-fg-color color "■") " ")
                  icon-space      2
                  item            (str title " " percent "%")
                  item-space      (count item)
                  separator       "  "
                  separator-space 2
                  used-space      (+ icon-space item-space separator-space)]]
      (vswap! left-over-space - used-space)
      ;; todo: fix if left-over-space = 0 it's ok to print here but need to reset left-over-space
      (if (> @left-over-space 0)
        (print (str icon item separator))
        (let [left-space (+ used-space @left-over-space)]
          ;; end current line filled with empty spaces
          (print (str (format (str "%" left-space "s") " ") " │\n"))
          ;; start a new line
          (print (str "│ " icon item separator))
          (vreset! left-over-space (- width used-space)))))
    ;; fill in the rest with empty spaces
    (if (> @left-over-space 0)
      (print (str (format (str "%" @left-over-space "s") " ") " │\n"))
      (print " │\n"))))

(defn vlegend [legend-map width]
  (doseq [[title {:keys [percent color]}] legend-map
          :let [icon            (str (with-fg-color color "■") " ")
                icon-space      2
                item            (str title " " percent "%")
                item-space      (count item)
                left-over-space (- width icon-space item-space)
                empty-spaces    (format (str "%" left-over-space "s") " ")]]
    (print (str "│ " icon item empty-spaces " │\n"))))

(defn header [width title border]
  (let [title-space      (count title)
        half-w           (/ width 2)
        half-t           (/ title-space 2)
        pre-title-space  (- half-w half-t 1)
        post-title-space (if (even? title-space)
                           pre-title-space
                           (inc pre-title-space))]
    (str (apply str (repeat pre-title-space border))
         " " title " "
         (apply str (repeat post-title-space border)))))

(defn border [leader trailer row]
  (str leader (format (str "%" (count row) "s") row) trailer))

(defn panel [txt & {:keys [width title]
                    :or   {width 50
                           title "Panel"}}]
  (let [words           (-> txt
                            (str/replace "\n" "")
                            (str/split #"\s+"))
        head-lines      (header width title "─")
        lines           (apply str (repeat width "─"))
        left-over-space (volatile! width)]
    ;; header
    (println (border "┌─" "─┐" head-lines))
    ;; panel body
    (doseq [word words
            :let [word-space      (count word)
                  separator       " "
                  separator-space 1]]
      (when (= @left-over-space width)
        ;; when a new line started
        (print (str "│ ")))
      (vswap! left-over-space - word-space)
      (if (> @left-over-space 0)
        (do
          ;; when it's enough space for a word with a space
          (vswap! left-over-space - separator-space)
          (print (str word separator)))
        (if (= @left-over-space 0)
          (do
            ;; when it's enough space only for a word - line end
            (vreset! left-over-space width)
            (print (str word " │\n")))
          ;; when space is not enough for a word - line end and new line
          (let [left-space   (+ word-space @left-over-space)
                empty-spaces (if (> left-space 0)
                               (format (str "%" left-space "s") " ")
                               "")]
            ;; end current line filled with empty spaces
            (print (str empty-spaces " │\n"))
            ;; start a new line
            (print (str "│ " word separator))
            (vreset! left-over-space (- width word-space separator-space))))))
    ;; end unfinished line if any
    ;; fill in the rest with empty spaces
    (when (and (> @left-over-space 0)
               (< @left-over-space width))
      (print (str (format (str "%" @left-over-space "s") " ") " │\n")))
    ;; footer
    (println (border "└─" "─┘" lines))))

(defn definition-list [dl-map & {:keys [width title]
                                 :or   {width 50
                                        title "Definitions"}}]
  (let [txt (StringBuilder.)]
    (doseq [[k v] dl-map
            :let [k-space    (count k)
                  v-space    (count v)
                  used-space (+ k-space v-space)
                  left-space (- width used-space)
                  dots       (apply str (repeat left-space "."))]]
      (.append txt (str k dots v " ")))
    (panel (.toString txt) {:title title
                            :width width
                            })))

(defn allocation-chart [alloc-map & {:keys [width title legend-pos]
                                     :or   {width      50
                                            title      "Allocation"
                                            legend-pos :horizontal}}]
  ;; todo: validate that sum of allocation percents is 100
  (let [ks         (keys alloc-map)
        vs         (vals alloc-map)
        legend-map (volatile! {})
        spaces     (apply str (repeat width " "))
        lines      (apply str (repeat width "─"))
        head-lines (header width title "─")]
    ;; header
    (println (border "┌─" "─┐" head-lines))
    ;; chart body
    (print "│ ")
    ;; todo: show | for items where bar-width will be ~0 on the given scale
    (doseq [[k v bar-width] (map vector ks vs (distribute width vs))
            :let [color (rand-int 231)]]
      (vswap! legend-map assoc k {:percent v :color color})
      (print (with-bg-color color (apply str (repeat bar-width " ")))))
    (print " │\n")
    ;; space
    (println (border "│ " " │" spaces))
    (case legend-pos
      :horizontal (hlegend @legend-map width)
      :vertical (vlegend @legend-map width))
    (println (border "└─" "─┘" lines))))


(comment

  (table [{:title "Total spent"
           :val   "57863.86USD"}
          {:title "Current evaluation"
           :val   "16204.13USD"}
          {:title "Total profit"
           :val   "-41659.73USD"}
          {:title "Performance"
           :val   "-72.00%"}])

  (definition-list {"Total-spent"        "57863.86USD"
                    "Current-evaluation" "16204.13USD"
                    "Total-profit"       "-41659.73USD"
                    "Performance"        "-72.00%"}
                   {:width 50
                    :title "Stats"})

  (panel "Lorem ipsum dolor sit amet, consectetur adipiscing elit,
  sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
  ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit
  in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
  Excepteur sint occaecat cupidatat non proident, sunt in culpa qui
  officia deserunt mollit anim id est laborum.")

  (allocation-chart {:AAPL 82
                     :AMZN 6
                     :NFLX 4
                     :XOM  10})

  (allocation-chart {:AAPL 34.15M
                     :AMZN 58.54M
                     :NFLX 7.32M})

  (allocation-chart {:AAPL   24
                     :AZM    22
                     :XOM    10
                     :NFLX   3
                     :AMZN   5
                     :TPL    12
                     :HES    4
                     :ABNORM 10
                     :X1     5
                     :Y2     5
                     }
                    {:title "Tes"})

  )
