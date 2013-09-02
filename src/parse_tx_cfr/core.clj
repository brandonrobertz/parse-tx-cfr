;; (c) 2013 Bradon Robertz ... RIP EWOK
;; GPLv3+ license
(ns parse-tx-cfr.core
  (:gen-class)
  (:require [clojure.string :refer [join]])
  (:import [com.snowtide.pdf OutputTarget RegionOutputTarget PDFTextStream]))

;; NOTE:
;; Right now I'm using snowtide's PDFTextStream library. I could and definitley
;; should change to Apache PDFBox of possibly iText, because of the single-
;; thread restrictions put on the *free* version of PDFTextStream and the
;; license incompatabilities (PDFTextStream is nonredistributable).

;;;;  R E G I O N S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;  P D F  B A S I C  U N I T S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;  C O N V E R S I O N  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lines->strings
  "Take a list of lists of lines and turn it into a list of
   rendered strings."
  [lines]
  (map
    (fn [line]
      (map
        #(let [txt (StringBuilder. 1024)]
          (.pipe % (OutputTarget. txt))
          (str txt))
        line))
    lines))

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

(defn page->strings
  "Take a page and convert the structured information into lists of lists
   of strings, representing the heriarchy of the document's structure."
   [pg]
   (->> (.getTextContent pg)
        (blocks)
        (lines)
        (lines->strings)))

(defn page->infomap
  "Take a page and convert the structured information into lists of lists
   of strings, representing the heriarchy of the document's structure."
   [pg]
   (->> (.getTextContent pg)
        (blocks)
        (lines)
        (lines->infomap)
        (flatten)))

;;;;  F I N D I N G  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; by 2-dimensional distance
(defn dist
  "Euclidean distance for two collections, assumed x y pairs."
  [c1 c2]
  (->> (map - c1 c2) (map #(* % %)) (reduce +)))

(defn dist-down
  "Custom distance for two collections, assumed x y pairs.
   Penalizes y-distance and above-distance."
  [c1 c2]
  (do
    (println "c1:" c1 "c2:" c2)
    (println "x1-x2:" (reduce * (repeat 2 (- (first c1)  (first c2))))))
  (+
   ; penalize x distance heavy (squared)
   (reduce * (repeat 2 (- (first c1)  (first c2))))
   ; penalize if c1 above c2 (c2 > c1)
   (let [d (- (second c1) (second c2))]
     (if (pos? d) d (* d d)))))

(defn closest
  "Get closest object (infomap) in PDF infomap by Euclidean distance."
  [c m]
  (apply min-key #(dist ((juxt :x :y) c) ((juxt :x :y) %)) (remove #(= c %) m)))

;; TRUTH ;;

(defn contributors?
  "Check a list to see if it contains phrases that indicate we're looking
   at a page that contains contribution information."
  [l]
  (if (filter #(re-find #"CONTRIBUTIONS" %) (flatten l)) true false))

(defn header-contributor?
  "Check a string/infomap to see if it contains the contributor header."
  [m]
  (if (re-find #"name\sof\scontributor" (:txt m m)) true false))

;; TODO
;; Approximate or fuzzy matching instead of regex
;; possibly construct s list of commonly error-prone characters
;; and construct a search of these groups from a string
(defn str-in-map?
  "Take a string/infomap and see if it matches the given string."
  [s m]
  ; right now, we're just normalizing spaces and case
  (if (re-find (re-pattern (.toUpperCase s))
               (.toUpperCase (clojure.string/replace (:txt m m) #"\s+" " ")))
    true
    false))

;; Helpers for finding header, value, & relationship btwn (delta)
(defn find-by-str
  "Take a string and a page, and return the infomap(s)
   containing that string, for every match found on the page."
  [pg s]
  (for [i (page->infomap pg)
        :when (str-in-map? s i)]
    i))

;; NOTE
;; I considered implementing width and height ... the problem is that for some
;; PDF objects, their widths and heights are mostly empty space, which overlap
;; with other objects. This makes it useless when you grab the text.
(defn delta
  "When given two strings and a page, find the correct infomaps and return
   a delta x and y, which are used to find one (s2) from the other (s1).
   Since situations can arrise where the strings will bring up many infomaps,
   (as in the case of a repeated header), use s1 as the string that can contain
   many matches and s2 as a unique string. We will pick the s1 closest to s2."
  [pg s1 s2]
  (let [a (first (find-by-str pg s2)) ;unique
        b (closest a (find-by-str pg s1))]
    (println "a:" a)
    (println "b:" b)
    {:dy (- (:y b) (:y a)) :dx (- (:x a) (:x b))}))

;;;;  G E T  S T U F F  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These functions generally wrap the above, which are more raw tools for
;; matching strings and pulling matches out of pages.

(defn get-contributors-by-order
  "Take a list of infomaps (or strings), representing a PDF page, and pull out
   contributor data (not contribution amount)."
  [l]
  (remove nil?
          (let [f (flatten l)]
            (for [i (range (count f))
                  ; if we're dealing with infomap, pull out txt, else string
                  :let [s (:txt (nth f i) (nth f i))]]
              (cond
               (re-find #"contributor" s) (nth f (+ i 1))
               ;(re-find #"tributor address" s) (join ", " [(nth f (+ i 1)) (nth f (+ i 2))])
               )))))

(defn contributor-headers
  "Take a list of strings or infomaps and return the contributor headers. For
   use with regions and the coords they provide. Assuming list is flat."
  [l]
  (for [i l
        :let [s (:txt i i)]
        :when (header-contributor? s)]
    i))

;; TODO
;; In the future there will have to be an auto-configuration
;; of delta & width & height to achieve maximum righteousness
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
    (region->string pg x (- y delta) width height )))

;; GENERIC
(defn vals-by-header
  "On a given page, find a header, indicated by a match
   of head-str, and then look delta units down (positive int,
   or neg for up) within an area specified by width (w) and
   height (h)."
  [pg head-str dy w h]
  (for [i (page->infomap pg)
        :let [x (:x i)
              y (:y i)]
        :when (str-in-map? head-str i)]
    (region->string pg x (- y dy) w h)))

(defn get-amounts
  "Take a list of strings, from the PDF page, and pull out the contribution
   about, in the order that they appear in the PDF 'DOM'."
  [l]
  (let [f (flatten l)]
    (for [i (range (count f))
          :let [s (nth f i)]
          :when (re-find #"[$][0-9]+[.,]?[0-9]+" s)]
      (nth f i))))

;;;;  M A I N  F U N C T I O N S  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; Right now this is basically a placeholder. Eventually, this will
;; loop through all the pages in a supplies PDF, find the proper pages
;; with contributor data, and parse the tables into structured CSV.
(defn do-pages
  [filename]
  (with-open [stream (PDFTextStream. filename)]
    (let [pg (.getPage stream 10)]
      (page->strings pg))))

;; for convenience in live coding:
;; (def pg (.getPage (PDFTextStream. "data/test.pdf") 10))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (do-pages "data/test.pdf"))
