(ns vipassana.core)

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

(defn denormalize-shallow-entity
  "the object level. like `{:foo/id 0, :foo/field1 1, :foo/bar [:bar 0]}`
  "
  [db {:keys [query] :as submodel} entity]
  (cond
    ;; union query
    (map? query)
    {}

    ;; normal query
    (vector? query)
    (reduce (fn [acc head]
              (case (::type (meta head))
                :join-one  (let [[field contextual-model] head
                                 field-ident              (get entity field)
                                 field-entity             (get-in db field-ident)
                                 expanded-entity          (denormalize-shallow-entity
                                                           db contextual-model field-entity)]
                             (assoc acc field expanded-entity))
                :join-many (let [[field contextual-model] head
                                 field-idents             (get entity field)]
                             (assoc acc field
                                    (mapv (fn [field-ident]
                                            (let [field-entity (get-in db field-ident)]
                                              (denormalize-shallow-entity db contextual-model field-entity)))
                                          field-idents)))
                (if (keyword? head)
                  (assoc acc head (get entity head))
                  (do
                    (throw (ex-info "entry in query is not recognized." {:acc  acc
                                                                         :head head}))
                    acc))))
            {}
            query)))

(defn db->tree
  "e.g. to denormalize #v/ident [:foo/id 0], you'd need to pass a querymodel like so:
  {:id :foo/id, query [:foo/id :foo/field1]}."
  [db querymodel data]
  (cond
    (= (::type (meta data)) :ident) (denormalize-shallow-entity db querymodel (get-in db data))
    (vector? data)                  (mapv (fn [datum]
                                            (if (= (::type datum) :ident)
                                              (denormalize-shallow-entity db querymodel (get-in db datum))
                                              ;; assume shallow-entity
                                              (denormalize-shallow-entity db querymodel datum)))
                                          data)
    ;; assume shallow-entity
    :else                           (denormalize-shallow-entity db querymodel data)))

(def bar-model
  {:id :bar/id
   :query [:bar/id
           :bar/field1]})

(def baz-model
  {:id :baz/id
   :query [:baz/id
           :baz/field1]})

(def foo-model
  {:id :foo/id
   :query [:foo/id
           :foo/field1
           #v/join-one [:foo/bar bar-model]
           #v/join-many [:foo/baz baz-model]]})

(def foo-example
  {:foo/id     0
   :foo/field1 1
   :foo/bar    {:bar/id     0
                :bar/field1 10}
   :foo/baz    [{:baz/id     0
                 :baz/field1 10}
                {:baz/id     1
                 :baz/field2 20}]})

(def foo-ident-example
  #v/ident [:foo 0])
