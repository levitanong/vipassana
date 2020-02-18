(ns vipassana.core2
  (:require
   [edn-query-language.core :as eql]))

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

(declare schema->ast)

(defn select-ast
  [schema selection-ast]
  (when-not (= :query (:type selection-ast))
    (throw (ex-info "Can only select if selection is of type query")))
  )

(defn key->dispatch-key
  [key]
  (if (eql/ident? key) (first key) key))

(defn schema-id-key
  [schema]
  (->> schema
       (filter (fn [[key id]]
                 (= id :id)))
       (map (fn [[key]] key))
       (first)))

(defn schema->ast
  [schema]
  {:type   :schema
   :id-key (schema-id-key schema)
   :children
   (mapv (fn [map-entry]
           (when-not (map-entry? map-entry)
             (throw (ex-info "Invalid schema entry" {:schema-entry map-entry})))
           (let [[key entry-type]  map-entry
                 [type type-param] (if (sequential? entry-type)
                                     entry-type
                                     [entry-type])]
             (merge
              {:type         type
               :key          key
               :dispatch-key (key->dispatch-key key)}
              (cond
                (set? type-param) {:children (mapv schema->ast type-param)}
                (map? type-param) {:children (mapv schema->ast [type-param])}
                :else             nil))))
         schema)})

(defn normalize
  [{:keys [type] :as ast} tree]
  (case type
    :schema    (if (eql/ident? tree)
                 {:data tree
                  :dict {}}
                 (let [{:keys [id-key
                               children]} ast
                       id                 (get tree id-key)
                       model-ident        [id-key id]
                       dict               (reduce (fn [acc {:keys [dispatch-key key] :as child-node}]
                                                    (let [{:keys [data dict]} (normalize child-node
                                                                                         (get tree dispatch-key))]
                                                      (-> acc
                                                          (deep-merge dict)
                                                          ((fn [d]
                                                             (if (eql/ident? key)
                                                               (assoc-in d (filter keyword? key) data)
                                                               (update-in d model-ident assoc
                                                                          dispatch-key data)))))))
                                                  {}
                                                  children)]
                   {:data model-ident
                    :dict dict}))
    :union     (let [{:keys [children]}  ast
                     matching-child-node (->> children
                                              (filter (fn [{:keys [id-key]}]
                                                        (contains? tree id-key)))
                                              (first))]
                 (normalize matching-child-node tree))
    :join-one  (let [{:keys [dispatch-key
                             children]} ast
                     [child-node]       children]
                 (if (nil? tree)
                   {:data nil :dict {}}
                   (normalize child-node tree)))
    :join-many (let [{:keys [dispatch-key
                             children]} ast
                     [child-node]       children]
                 (if (nil? tree)
                   {:data nil :dict {}}
                   (reduce (fn [acc element]
                             (let [{:keys [data dict]} (normalize child-node element)]
                               (-> acc
                                   (update :data safe-conj data)
                                   (update :dict deep-merge dict))))
                           {:data nil :dict {}}
                           tree)))
    :prop      {:data tree :dict {}}
    :id        {:data tree :dict {}}
    (throw (ex-info "Invalid AST" {:ast ast :tree tree}))))
