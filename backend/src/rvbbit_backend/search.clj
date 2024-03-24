 
(ns rvbbit-backend.search
  (:import [org.apache.lucene.analysis Analyzer Analyzer$TokenStreamComponents]
           [org.apache.lucene.document Document StringField TextField Field$Store]
           [org.apache.lucene.index IndexWriter Term]
           [org.apache.lucene.analysis.standard StandardAnalyzer StandardTokenizer]
           [org.apache.lucene.analysis.ngram NGramTokenizer NGramTokenFilter]
           [org.apache.lucene.analysis.core LowerCaseFilter]
           [org.apache.lucene.index IndexWriterConfig DirectoryReader]
           [org.apache.lucene.store Directory FSDirectory]
           [org.apache.lucene.queryparser.classic QueryParser]
           [org.apache.lucene.search IndexSearcher BooleanClause$Occur TermQuery BooleanQuery$Builder WildcardQuery]
           [java.nio.file Paths]))

(def idx-path "./data/search-index")

(defn create-index-writer [index-path]
  (let [path (Paths/get index-path (into-array String []))
        dir (FSDirectory/open path)
        analyzer (StandardAnalyzer.)
        config (IndexWriterConfig. analyzer)]
    (IndexWriter. dir config)))

;; (defn add-or-update-document [writer id data-map] ;; whole word only. faster, but at what search cost? TBD
;;   (future ;; dont want to block since we dont care about a return, its just a side-effect 
;;     (let [doc (Document.)]
;;       (.add doc (StringField. "id" id Field$Store/YES)) ; Correct usage here
;;       (doseq [[k v] data-map]
;;         (.add doc (TextField. (name k) (str v) Field$Store/YES))) ; And here
;;       (.updateDocument writer (Term. "id" id) doc))))

;; (defn add-or-update-document [writer id data-map]
;;   (future
;;     (let [doc (Document.)
;;           analyzer (NGramTokenizer. 3 3)] ; Create an NGramTokenizer with min and max n-gram sizes of 3
;;       (.add doc (StringField. "id" id Field$Store/YES))
;;       (doseq [[k v] data-map]
;;         (let [field (TextField. (name k) (str v) Field$Store/YES)]
;;           (.setTokenStream field (Analyzer$TokenStreamComponents. analyzer)) ; Set the field's token stream to the NGramTokenizer
;;           (.add doc field)))
;;       (.updateDocument writer (Term. "id" id) doc))))

(def executor (java.util.concurrent.Executors/newFixedThreadPool 10))

(defn add-or-update-document [writer id data-map]
  (.submit executor ; Submit a task to the executor
           (reify java.util.concurrent.Callable
             (call [_]
               (let [doc (Document.)
                     analyzer (NGramTokenizer. 3 3)] ; Create an NGramTokenizer with min and max n-gram sizes of 3
                 (.add doc (StringField. "id" id Field$Store/YES))
                 (doseq [[k v] data-map]
                   (let [field (TextField. (name k) (str v) Field$Store/YES)]
                     (.setTokenStream field (Analyzer$TokenStreamComponents. analyzer)) ; Set the field's token stream to the NGramTokenizer
                     (.add doc field)))
                 (.updateDocument writer (Term. "id" id) doc))))))

(defn ngram-analyzer []
  (proxy [Analyzer] []
    (createComponents [fieldname]
      (let [source (NGramTokenizer. 3 3)]
        (Analyzer$TokenStreamComponents. source)))))

;; (defn ngram-analyzer []
;;   (proxy [Analyzer] []
;;     (createComponents [fieldname]
;;       (let [source (StandardTokenizer. fieldname)
;;             filter1 (LowerCaseFilter. source)
;;             filter2 (NGramTokenFilter. filter1 1 3)] ; Create an NGramTokenFilter with min and max n-gram sizes of 1 and 3
;;         (Analyzer$TokenStreamComponents. source filter2)))))


(defn search-index [index-path query-str & [max-hits type]]
  (let [reader   (DirectoryReader/open (FSDirectory/open (Paths/get index-path (into-array String []))))
        searcher (IndexSearcher. reader)
        analyzer (ngram-analyzer) ; Use the custom NGramAnalyzer here
        query-parser (QueryParser. "content" analyzer)
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
            type (keyword (.get doc "type"))]
        (swap! counts update type (fnil inc 0))))
    @counts))

(defn delete-document [writer id]
  (.deleteDocuments writer (into-array Term [(Term. "id" id)])))

(defn close-index-writer [writer]
  (.close writer))

;; (defn search-index [index-path query-str & [max-hits type]]
;;   (let [reader (DirectoryReader/open (FSDirectory/open (Paths/get index-path (into-array String []))))
;;         searcher (IndexSearcher. reader)
;;         query-parser (QueryParser. "content" (StandardAnalyzer.))
;;         base-query (.parse query-parser query-str)
;;         query (if type
;;                 (let [type-query (TermQuery. (Term. "type" (name type))) ; Convert the type keyword to a string
;;                       boolean-query (BooleanQuery$Builder.)
;;                       _ (.add boolean-query base-query BooleanClause$Occur/MUST)
;;                       _ (.add boolean-query type-query BooleanClause$Occur/FILTER)]
;;                   (.build boolean-query))
;;                 base-query)
;;         max-hits (or max-hits Integer/MAX_VALUE) ; Use max-hits if provided, otherwise return all matching documents
;;         topDocs (.search searcher query max-hits)
;;         hits (.scoreDocs topDocs)]
;;     (into {} (map (fn [hit]
;;                     (let [doc (.doc searcher (.doc hit)) ; Fetch the document
;;                           fields (into {} (map (fn [field] [(keyword (.name field)) (.stringValue field)]) (.getFields doc))) ; Extract field values as strings
;;                           doc-id (:id fields) ; Retrieve the document ID
;;                           fields (dissoc fields :content :id)] ; Remove the content and id fields
;;                       [doc-id fields])) ; Return a map entry with the document ID as the key and the fields as the value
;;                   hits))))





