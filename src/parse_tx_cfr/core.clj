;;;; (c) 2013 Bradon Robertz ... RIP EWOK VDB
;;;; GPLv3+ (I'm considering Snowtide PDFTextStream a system lib for now)
(ns parse-tx-cfr.core
  (:gen-class)
  (:require [clojure.string :refer [join]])
  (:import [com.snowtide.pdf OutputTarget RegionOutputTarget PDFTextStream]))

;;; NOTE:
;;; Right now I'm using snowtide's PDFTextStream library. I will
;;; change to Apache PDFBox of possibly iText, because of the single-
;;; thread restrictions put on the *free* version of PDFTextStream and the
;;; license incompatabilities (PDFTextStream is nonredistributable).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  R E G I O N S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn do-region
  "Adds a region with name s to our RegionOutputTarget object
   tgt. Specify x, y, w, and h."
  [tgt x y w h s]
  (.addRegion tgt x y w h s))

(defn do-regions
  "Take a list of key-value maps specifying regions to grab.
   i.e.:
   [{:name 'cool1' :x 100 :y 100 :w 100 :h 10}
      ...
    {:name 'coolN' :x 200 :y 100 :w 100 :h 10}]"
  [tgt m]
  (doseq [i m]
    (let [s (:name i)
          x (:x i)
          y (:y i)
          w (:w i)
          h (:h i)]
      (do-region tgt x y w h s))))

(defn region->string
  "When given an area to look, region->string will grab
   the information contained inside that area in a list
   of strings."
  [pg x y w h]
  (let [tgt (RegionOutputTarget.)]
    (do-region tgt x y w h "lol")
    (.pipe pg tgt)
    (.getRegionText tgt "lol")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  P D F  B A S I C  U N I T S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All the disgusting java interop goes here ... waiting to be replaced
;;; by free code.

(defn blocks
  "When given a blockParent, returned by .getTextContent (usually from
   a  page), it will return a list of all of the blocks contained in
   that blockParent."
  [block-parent]
  (for [x (range (.getChildCnt block-parent))]
    (.getChild block-parent x)))

(defn lines
  "For a list of blocks, this function will return a list of lines for
   each block in the list."
  [blocks]
  (map #(for [x (range (.getLineCnt %))] (.getLine % x)) blocks))

(defn text-units
  "Takes a list of text units (possibly 2-dimensional, list of lists of
   text units) and returns a list of strings of the text within."
  [lines]
  (map
    (fn [line]
      (map
        #(for [x (range (.getTextUnitCnt %))] (.getTextUnit % x))
        line))
    lines))

(defn num-pages
  "Return the number of pages in a PDF document."
  [stream]
  (.getPageCnt stream))

(defn get-pg
  "Grab n page from a PDF document (a stream in this case)."
  [stream n]
  (.getPage stream n))

(defn get-stream
  "Get the PDF stream object."
  [filename]
  (PDFTextStream. filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  C O N V E R S I O N  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lines->infomap
  "Take a list of lists of lines and turn it into a list of
   maps containing rendered strings, and their properties (like
   position)."
  [lines]
  (map
    (fn [line]
      (map
        #(let [txt (StringBuilder. 1024)]
          (.pipe % (OutputTarget. txt))
          {:txt (str txt) :x (.xpos %) :y (.ypos %)}
          )
        line))
    lines))

(defn page->infomap
  "Take a page and convert the structured information into lists of lists
   of strings, representing the heriarchy of the document's structure."
   [pg]
   (->> (.getTextContent pg)
        (blocks)
        (lines)
        (lines->infomap)
        (flatten)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  F I N D I N G  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; by 2-dimensional distance
(defn dist
  "Euclidean distance for two collections, assumed x y pairs."
  [c1 c2]
  (->> (map - c1 c2) (map #(* % %)) (reduce +)))

(defn closest
  "Get closest object (infomap) in PDF infomap by Euclidean distance."
  [c m]
  ;(do (println "c:" c) (println "m:" m) (flush)) ;DEBUG
  (apply min-key #(dist ((juxt :x :y) c) ((juxt :x :y) %)) (remove #(= c %) m)))

;;;; TRUTH ;;;;

(defn contributors?
  "Check a list to see if it contains phrases that indicate we're looking
   at a page that contains contribution information."
  [l]
  (if (filter #(re-find #"CONTRIBUTIONS" %) (flatten l)) true false))

(defn header-contributor?
  "Check a string/infomap to see if it contains the contributor header."
  [m]
  (if (re-find #"name\sof\scontributor" (:txt m m)) true false))

;;; TODO
;;; Approximate or fuzzy matching instead of regex
;;; possibly construct a list of commonly error-prone characters
;;; and construct a search of these groups from a string ...
;;; or defmacro something to construct sets of regexes
(defn str-in-map?
  "Take a string/infomap and see if it matches the given string."
  [s m]
  ;; right now, we're just normalizing spaces and case
  (if (re-find (re-pattern (.toUpperCase s))
               (.toUpperCase (clojure.string/replace (:txt m m) #"\s+" " ")))
    true
    false))

(defn find-by-str
  "Take a string and a page, and return the infomap(s)
   containing that string, for every match found on the page."
  [pg s]
  (for [i (page->infomap pg)
        :when (str-in-map? s i)]
    i))

;;; NOTE
;;; I considered implementing width and height ... the problem is that for some
;;; PDF objects, their widths and heights are mostly empty space, which overlap
;;; with other objects. This makes it useless when you grab the text.
(defn delta
  "When given two strings and a page, find the correct infomaps and return
   a delta y and x, which is used to find one (s2) from the other (s1).
   Since situations can arrise where the strings will bring up many infomaps,
   (as in the case of a repeated header), use s1 as the string that can contain
   many matches and s2 as a unique string. We will pick the s1 closest to s2.

   NOTE: This will be used to generate a generic distance from a known header
   to a value that we want. We will query the page for headers, subtract
   a delta from the header's position to get the values we want."
  [pg s1 s2]
  (let [a (first (find-by-str pg s2)) ;unique
        b (closest a (find-by-str pg s1))]
    {:dy (- (:y b) (:y a)) :dx (:x a)}))

(defn batch-deltas
  "Take a page & a list of headers and example (training) values and return the
   appropriate delta values with their header strings.

   Headers & example vals (m) are in the following format:
   [[\"header1\" \"example value1\"]
    ...
    [\"headerN\" \"example valueN\"]]"
  [m pg]
  (for [x m
        :let [s1 (first  x)
              s2 (second x)]]
    (into {:txt s1} (delta pg s1 s2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  G E T  S T U F F  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These functions generally wrap the above, which are more raw tools for
;;; matching strings and pulling matches out of pages.

(defn contributor-headers
  "Take a list of strings or infomaps and return the contributor headers. For
   use with regions and the coords they provide. Assuming list is flat."
  [l]
  (for [i l
        :let [s (:txt i i)]
        :when (header-contributor? s)]
    i))

;;; TODO
;;; In the future there will have to be an auto-configuration
;;; of delta & width & height to achieve maximum righteousness
(defn contributor-names
  "Take a page, and return the names under the header, based
   on region estimation."
  [pg]
  (for [i (page->infomap pg)
        :let [x (:x i)
              y (:y i)
              ;make these configurable in the future
              delta 20 width 200 height 10]
        :when (header-contributor? i)]
    (region->string pg x (- y delta) width height)))

;;; This is the main search function that we will use to grab our values
;;; all that's needed is a header value of the value we want and a delta
;;; (the distance from the header to our value)
(defn vals-by-header
  "On a given page, find a header, indicated by a match
   of head-str, and then look delta units down (positive int,
   or neg for up) within an area specified by width (w) and
   height (h)."
  [pg head-str dy dx w h]
  (for [i (page->infomap pg)
        :let [y (:y i)]
        :when (str-in-map? head-str i)]
    (region->string pg dx (- y dy) w h)))

(defn vals-from-deltamaps
  "Take a list of delta maps, resulting from a call to batch-deltas,
   and search a given page for the values pointed at by the header/delta
   pairs."
  [pg m]
  (for [x m
        :let [txt (:txt x)
              dy  (:dy  x)
              dx  (:dx  x)
              w 200
              h 5]]
    (vals-by-header pg txt dy dx w h)))

(defn restruct
  "Turn our one list per type of info to one-record per vector."
  [m]
  (map #(apply vector (identity %))
       (partition (count m) (apply interleave m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  C O N F I G U R E  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These functions will facilitate the configuration of what values to grab
;;; and allow the user to configure the program preferences easily.
;;; TODO: MAKE THESE!

(defn config
  "This is a placeholder config. For use with data/test.pdf page #10"
  []
  [["name of contribut"  "byron"]
   ["utor address"      "wooten"]
   ["amount of"            "100"]
   ["occupat"       "accountant"]
   ["employe"          "prophet"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  M A I N  F U N C T I O N S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO
;;; Right now this is basically a placeholder. Eventually, this will
;;; loop through all the pages in a supplies PDF, find the proper pages
;;; with contributor data, and parse the tables into structured CSV.

(defn scrape-page
  "Scrape data off page in PDF stream, specified by cfg."
  [pg deltas]
  (restruct (vals-from-deltamaps pg deltas)))

(defn scrape-pages
  [stream]
  (let [cfg (config) ; TODO change this to a real config
        cfg-map (get-pg stream 10)
        deltas (batch-deltas cfg cfg-map)]
    ;(println "cfg:" cfg)         ;DEBUG
    ;(println "cfg-map:" cfg-map) ;DEBUG
    ;(println "deltas:" deltas)   ;DEBUG
    (for [n '(10 11) ;[n (range (num-pages stream))]
          :let [pg (get-pg stream n)]]
      (scrape-page pg deltas))))

(defn do-pages-DBG
  [stream]
  (let [cfg (config)]
    (for [n '(10 11)];[n (range (num-pages stream))]
      (scrape-page stream n cfg))))

; for convenience in live coding:
(defn testing-convenience
  "Set up some variables so that I can easily test functions."
  []
  (def stream (get-stream "data/test.pdf"))
  ; needed for restrictions on multi-threaded use in PDFTextStream
  (def pg (get-pg stream 10))
  ; this is a sample config based on pg 10 in test.pdf
  (def cfg [["name of contributor" "byron"]
            ["utor address"        "wooten"]
            ["amount of"           "100"]
            ["occupation"          "accountant"]
            ["employer"            "prophet"]])
  (def dm (batch-deltas cfg pg))
  (def vm (vals-from-deltamaps pg dm)))

;(def stream (get-stream "data/test.pdf"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (scrape-pages stream))
