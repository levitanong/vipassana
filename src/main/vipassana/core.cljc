(ns vipassana.core
  (:refer-clojure :exclude [ident?])
  (:require
   [clojure.spec.alpha :as s]
   [ghostwheel.core :as g :refer [>defn >defn- >fdef => | <- ?]]))

(s/def ::id keyword?)

(s/def ::ident #(= (::type (meta %)) :ident))

(s/def ::link (s/and ::ident #(= 1 (count %))))

(s/def ::join-one (s/and #(= (::type (meta %)) :join-one)
                         (s/tuple keyword? ::model)))

(s/def ::join-many (s/and #(= (::type (meta %)) :join-many)
                          (s/tuple keyword? ::model)))

(s/def ::union (s/coll-of ::model :kind set? :into #{}))

(s/def ::query-element
  (s/or :field keyword?
        :link ::link
        :join-one ::join-one
        :join-many ::join-many))

(s/def ::query
  (s/or :standard-query (s/coll-of ::query-element)
        :union-query ::union))

(s/def ::fields ::query)

(s/def ::model
  (s/keys :req-un [::id-key ::fields]))

(s/def ::query-or-model
  (s/or :query ::query :model ::model))

(defrecord Ident [x])

#?(:clj
   (defmethod print-method Ident [r ^java.io.Writer writer]
     (.write writer "#ident ")
     (print-method (:x r) writer))
   :cljs
   (extend-protocol IPrintWithWriter
     Ident
     (-pr-writer [x writer _]
       (write-all writer "#ident " x))))

#?(:clj
   (defmethod print-dup Ident [o w]
     (print-method o w)))

(defn as-ident [x]
  (Ident. x)
  #_(with-meta x {::type :ident}))

(def ident as-ident)

(defn join-one [x]
  (with-meta x {::type :join-one}))

(defn join-many [x]
  (with-meta x {::type :join-many}))

(defn join-one?
  [subquery]
  (= :join-one (::type (meta subquery))))

(defn join-many?
  [subquery]
  (= :join-many (::type (meta subquery))))

(defn ident?
  [subquery]
  (= :ident (::type (meta subquery))))

(defn model? [x]
  (when (coll? x)
    (contains? x :id-key)))



(defn with-fields [model query]
  (assoc model :fields query))

(defn with-remap [model remap]
  (update model :fields
          (fn [fields]
            (mapv (fn [field]
                    (let [accessor (if (or (join-one? field) (join-many? field))
                                     (first field)
                                     field)]
                      (get remap accessor field)))
                  fields))))

(defn without-fields [model query-blacklist]
  (update model :fields
          (fn [fields]
            (filterv (fn [field]
                       (let [accessor (if (or (join-one? field) (join-many? field))
                                        (first field)
                                        field)]
                         (not (contains? query-blacklist accessor))))
                     fields))))

(defn deep-merge
"Merges nested maps without overwriting existing keys."
  [& xs]
  (if (every? map? xs)
    (apply merge-with deep-merge xs)
    (last xs)))

(defn safe-conj [xs x]
  (if (sequential? xs)
    (conj xs x)
    [x]))

(defn query->ast
  [query]
  (cond
    (model? query)
    (let [{:keys [id-key fields]} query]
      {:type     :model
       :query    query
       :id-key   id-key
       :children [(query->ast fields)]})

    (ident? query)
    {:type         :ident
     :dispatch-key (first query)
     :key          query}

    (set? query)
    {:type     :union
     :query    query
     :children (mapv (fn [field]
                       (query->ast field))
                     query)}

    (join-one? query)
    (let [[field-key subquery] query]
      (when-not field-key (throw (ex-info "Join-one had no field-key!" {:query query})))
      {:type         :join-one
       :query        query
       :dispatch-key (if (ident? field-key) (first field-key) field-key)
       :key          field-key
       :children     [(query->ast subquery)]})

    (join-many? query)
    (let [[field-key subquery] query]
      (when-not field-key (throw (ex-info "Join-many had no field-key!" {:query query})))
      {:type         :join-many
       :query        query
       :dispatch-key field-key
       :key          field-key
       :children     [(query->ast subquery)]})

    (vector? query)
    {:type     :anon-query
     :query    query
     :children (mapv (fn [subquery]
                       (query->ast subquery))
                     query)}

    (keyword? query)
    {:type         :prop
     :dispatch-key query
     :key          query}

    :else
    (throw (ex-info "Invalid query!" {:query query}))))

(defn normalize
  [{:keys [type] :as ast} tree]
  (case type
    :model      (if (ident? tree)
                  ;; Already normalized, so stop.
                  {:data tree
                   :dict {}}
                  (let [{:keys [id-key
                                children]}  ast
                        [child-node]        children
                        id                  (get tree id-key)
                        model-ident         (as-ident [id-key id])
                        {:keys [data dict]} (normalize child-node tree)]
                    {:data model-ident
                     :dict (assoc-in dict model-ident data)}))
    ;; ident is a no-op
    :ident      (throw (ex-info "Idents do not make sense during normalization" {:ast ast}))
    :union      (let [{:keys [children]}  ast
                      matching-child-node (->> children
                                               (filter (fn [{:keys [id-key]}]
                                                         (contains? tree id-key)))
                                               (first))]
                  (normalize matching-child-node tree))
    :join-one   (let [{:keys [dispatch-key
                              children]} ast
                      [child-node]       children]
                  (if (nil? tree)
                    {:data nil :dict {}}
                    (normalize child-node tree)))
    :join-many  (let [{:keys [dispatch-key
                              children]} ast
                      [child-node]       children]
                  (if (nil? tree)
                    {:data nil :dict {}}
                    (reduce (fn [acc element]
                              (let [{:keys [data dict]} (normalize child-node element)]
                                (-> acc
                                    (update-in [:data] safe-conj data)
                                    (update-in [:dict] deep-merge dict))))
                            {:data nil :dict {}}
                            tree)))
    :anon-query (let [{:keys [children]} ast]
                  (reduce (fn [acc {:keys [dispatch-key] :as child-node}]
                            (let [subtree             (get tree dispatch-key)
                                  {:keys [data dict]} (normalize child-node subtree)]
                              (-> acc
                                  (assoc-in [:data dispatch-key] data)
                                  (update-in [:dict] deep-merge dict))))
                          {:data nil :dict {}}
                          children))
    :prop       {:data tree
                 :dict {}}

    (throw (ex-info "Invalid AST" {:ast  ast
                                   :tree tree}))))

(defn denormalize
  [{:keys [type] :as ast} data db]
  (case type
    :model      (let [{:keys [id-key
                              children]} ast
                      [child-node]       children]
                  ;; child-node should be of type :anon-query
                  (cond
                    ;; expand an ident
                    (ident? data) (denormalize child-node (get-in db data) db)
                    ;; data already partially inflated
                    (map? data)   (denormalize child-node data db)
                    (nil? data)   {:data nil :dict {}}
                    :else         (throw (ex-info "Invalid data at ast node" {:data data
                                                                              :ast  ast}))))
    :ident      (let [{:keys [key]} ast]
                  (get-in db key))
    :union      (let [{:keys [children]}  ast
                      matching-child-node (->> children
                                               (filter (fn [{:keys [id-key]}]
                                                         (let [[table id] data]
                                                           (= id-key table))))
                                               (first))]
                  (denormalize matching-child-node data db))
    :join-one   (let [{:keys [dispatch-key
                              children]} ast
                      [child-node]       children]
                  (when data
                    (denormalize child-node data db)))
    :join-many  (let [{:keys [dispatch-key
                              children]} ast
                      [child-node]       children]
                  (reduce (fn [acc model-ident]
                            (conj acc (denormalize child-node model-ident db)))
                          []
                          data))
    :anon-query (let [{:keys [children]} ast]
                  (reduce (fn [acc {:keys [dispatch-key] :as child-node}]
                            (assoc acc dispatch-key
                                   (denormalize child-node (get data dispatch-key) db)))
                          {}
                          children))
    :prop       data
    (throw (ex-info "Invalid AST" {:ast  ast
                                   :data data}))))

(defn tree->db
  [query data]
  (normalize (query->ast query) data))

(defn db->tree
  [query data db]
  (denormalize (query->ast query) data db))
