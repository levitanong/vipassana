(ns vipassana.core
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

(s/def ::query-element
  (s/or :field keyword?
        :link ::link
        :join-one ::join-one
        :join-many ::join-many))

(s/def ::query (s/coll-of ::query-element))

(s/def ::model
  (s/keys :req-un [::id ::query]))

(s/def ::query-or-model
  (s/or :query ::query :model ::model))

(defn annotate-ident [x]
  (with-meta x {::type :ident}))

(defn annotate-join-one [x]
  (with-meta x {::type :join-one}))

(defn annotate-join-many [x]
  (with-meta x {::type :join-many}))

(defn annotate-model [x]
  (with-meta x {::type :model}))

(defn with-query [model query]
  (assoc model :query query))

(defn deep-merge
"Merges nested maps without overwriting existing keys."
  [& xs]
  (if (every? map? xs)
    (apply merge-with deep-merge xs)
    (last xs)))

(defn denormalize
  "the object level. like `{:foo/id 0, :foo/field1 1, :foo/bar [:bar 0]}`
  "
  [db {:keys [query] :as submodel} entity]
  (cond
    ;; union query
    (map? query)
    (throw (ex-info "union queries not supported yet." {:model submodel}))

    ;; normal query
    (vector? query)
    (reduce (fn [acc head]
              (case (::type (meta head))
                :join-one  (let [[field contextual-model] head
                                 field-ident              (get entity field)
                                 field-entity             (get-in db field-ident)
                                 expanded-entity          (denormalize
                                                           db contextual-model field-entity)]
                             (assoc acc field expanded-entity))
                :join-many (let [[field contextual-model] head
                                 field-idents             (get entity field)]
                             (assoc acc field
                                    (mapv (fn [field-ident]
                                            (let [field-entity (get-in db field-ident)]
                                              (denormalize
                                               db contextual-model field-entity)))
                                          field-idents)))
                (if (keyword? head)
                  (assoc acc head (get entity head))
                  (do
                    (throw (ex-info "entry in query is not recognized." {:acc   acc
                                                                         :head  head
                                                                         :model submodel}))
                    acc))))
            {}
            query)))

(defn db->tree
  "e.g. to denormalize #v/ident [:foo/id 0], you'd need to pass a querymodel like so:
  {:id :foo/id, query [:foo/id :foo/field1]}."
  [db querymodel data]
  (cond
    (= (::type (meta data)) :ident) (denormalize db querymodel (get-in db data))
    (vector? data)                  (mapv (fn [datum]
                                            (if (= (::type datum) :ident)
                                              (denormalize db querymodel (get-in db datum))
                                              ;; assume shallow-entity
                                              (denormalize db querymodel datum)))
                                          data)
    ;; assume shallow-entity
    :else                           (denormalize db querymodel data)))

(>defn normalize
  ([query-or-model subtree]
   [::query-or-model map? => map?]
   (normalize query-or-model subtree
              {:on-not-found (constantly nil)}))
  ([query-or-model subtree {:keys [on-not-found] :as config}]
   [::query-or-mode map? map? => map?]
   (let [id-key (:id query-or-model)
         id     (get subtree id-key)]
     #_(when-not (and id-key id)
         (throw (ex-info "Passed a model but data doesn't have id")))
     (cond
       (nil? subtree)
       (on-not-found query-or-model subtree)
       ;; query-or-model is a model
       (and id-key id)
       (let [model            query-or-model
             ident            #v/ident [id-key id]
             {sub-data :data
              sub-dict :dict} (normalize (:query model) subtree config)]
         {:data ident
          :dict (merge {id-key {id sub-data}}
                       sub-dict)})

       ;; query-or-model is a query
       (and (not id-key) (vector? query-or-model))
       (let [query query-or-model]
         (reduce (fn [acc query-element]
                   (case (::type (meta query-element))
                     :join-one  (let [[field context-model] query-element
                                      sub-entity            (get subtree field)
                                      {out-data :data
                                       out-dict :dict}      (normalize context-model sub-entity config)]
                                  (-> acc
                                      (assoc-in [:data field] out-data)
                                      (update :dict deep-merge out-dict)))
                     :join-many (let [[field context-model] query-element
                                      sub-entities          (get subtree field)
                                      {sub-data :data
                                       sub-dict :dict}      (reduce (fn [acc sub-entity]
                                                                      (let [{sub-data :data
                                                                             sub-dict :dict} (normalize context-model sub-entity config)]
                                                                        (-> acc
                                                                            (update-in [:data] conj sub-data)
                                                                            (update :dict deep-merge sub-dict))))
                                       {:data [] :dict {}}
                                       sub-entities)]
                                  (-> acc
                                      (assoc-in [:data field] sub-data)
                                      (update :dict deep-merge sub-dict)))
                     (if (keyword? query-element)
                       (assoc-in acc [:data query-element] (get subtree query-element))
                       (do (throw (ex-info "entry in query is not recognized." {:acc            acc
                                                                                :query-element  query-element
                                                                                :query-or-model query-or-model}))
                           acc))))
                 {:data {} :dict {}}
                 query))

       :else
       (throw (ex-info "Something went wrong" {:query-or-model query-or-model
                                               :subtree        subtree}))))))

(defn tree->db
  [querymodel data]
  (normalize querymodel data))
