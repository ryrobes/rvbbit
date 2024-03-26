 
(ns rvbbit-backend.search
  (:require [clojure.edn :as edn]
            [clojure.string :as cstr]
            ;; [clucie.core :as core]
            ;; [clucie.analysis :as analysis]
            ;; [clucie.store :as store]
            )
  (:import ;[org.apache.lucene.analysis Analyzer Analyzer$TokenStreamComponents]
           [org.apache.lucene.document Document StringField TextField Field$Store StoredField]
           [org.apache.lucene.index IndexWriter Term]
           [org.apache.lucene.analysis.standard StandardAnalyzer StandardTokenizer]
           ;[org.apache.lucene.analysis.ngram NGramTokenizer NGramTokenFilter]
           ;[org.apache.lucene.analysis.ngram NGramTokenizerFactory]
           ;[org.apache.lucene.analysis.custom CustomAnalyzer$Builder]
           ;[org.apache.lucene.analysis.custom CustomAnalyzer]
           [org.apache.lucene.analysis.core LowerCaseFilter  ]
           ;[org.apache.lucene.analysis TokenStream]
           [org.apache.lucene.index IndexWriterConfig DirectoryReader]
           [org.apache.lucene.store Directory FSDirectory]
           [org.apache.lucene.queryparser.classic QueryParser]
           [org.apache.lucene.search IndexSearcher BooleanClause$Occur TermQuery BooleanQuery$Builder WildcardQuery]
           ;java.io.StringReader
           ;[java.util HashMap]
           [java.nio.file Paths]))

(def idx-path "./data/search-index")

;; (def analyzer (analysis/standard-analyzer))

;; (def index-store 
;;   (store/memory-store) 
;;   ;(store/disk-store "./data/search-index2")
;;   )

(defn create-index-writer [index-path]
  (let [path (Paths/get index-path (into-array String []))
        dir (FSDirectory/open path)
        analyzer (StandardAnalyzer.)
        config (IndexWriterConfig. analyzer)]
    (IndexWriter. dir config)))

(defn generate-ngrams [text n]
  (let [words (try (clojure.string/split (str text) #"\s+") (catch Exception _  (do (println (str "ngrams-issue! " text)) [])))
        ngrams (mapcat (fn [word]
                         (let [word-length (count word)
                               max-ngram-length (min n word-length)]
                           (for [i (range max-ngram-length)]
                             (subs word 0 (inc i)))))
                       words)]
    (clojure.string/join " " (conj ngrams text))))

(defn ngrams [text] 
  ;(str (generate-ngrams text 3) " " (generate-ngrams text 4))
  (generate-ngrams text 3))

(defn add-or-update-document [writer id data-map] ;; whole word only. faster, but at what search cost? TBD
  ;(core/add! index-store [data-map] (vec (keys data-map)) analyzer)
    (let [;data-map (assoc data-map :ext (ngrams (get data-map :content)))
          doc (Document.)]
      (.add doc (StringField. "id" id Field$Store/YES))
      (doseq [[k v] data-map]
        (.add doc (TextField. (name k) (str v) Field$Store/YES))) 
      (.updateDocument writer (Term. "id" id) doc)))

(defn commit-writer [writer]
  (.commit writer))

(defonce index-writer (create-index-writer idx-path))

(defn count-documents [index-path]
  (let [reader (DirectoryReader/open (FSDirectory/open (Paths/get index-path (into-array String []))))]
    (.numDocs reader)))

(defn count-documents-by-type [index-path]
  (let [reader (DirectoryReader/open (FSDirectory/open (Paths/get index-path (into-array String []))))
        searcher (IndexSearcher. reader)
        max-doc (.maxDoc reader)
        counts (atom {})]
    (dotimes [i max-doc]
      (let [doc (.doc searcher i)
            type (str (.get doc "type"))]
        (swap! counts update type (fnil inc 0))))
    @counts))

(defn delete-document [writer id]
  (.deleteDocuments writer (into-array Term [(Term. "id" id)])))

(defn close-index-writer [writer]
  (.close writer))

(defn search-index [index-path query-str & [max-hits type]]
  (let [reader (DirectoryReader/open (FSDirectory/open (Paths/get index-path (into-array String []))))
        searcher (IndexSearcher. reader)
        query-parser (QueryParser. "content" (StandardAnalyzer.))
        base-query (.parse query-parser query-str)
        query (if type
                (let [type-query (TermQuery. (Term. "type" (name type))) ; Convert the type keyword to a string
                      boolean-query (BooleanQuery$Builder.)
                      _ (.add boolean-query base-query BooleanClause$Occur/MUST)
                      _ (.add boolean-query type-query BooleanClause$Occur/FILTER)]
                  (.build boolean-query))
                base-query)
        max-hits (or max-hits Integer/MAX_VALUE) ; Use max-hits if provided, otherwise return all matching documents
        topDocs (.search searcher query max-hits)
        hits (.scoreDocs topDocs)]
    (into {} (map (fn [hit]
                    (let [doc (.doc searcher (.doc hit)) ; Fetch the document
                          fields (into {} (map (fn [field] [(keyword (.name field)) (.stringValue field)]) (.getFields doc))) ; Extract field values as strings
                          doc-id (:id fields) ; Retrieve the document ID
                          fields (dissoc fields :content :id)] ; Remove the content and id fields
                      [doc-id fields])) ; Return a map entry with the document ID as the key and the fields as the value
                  hits))))





