(ns vipassana.core-test
  (:require
   [vipassana.core :as v]
   [vipassana.core2 :as v2]
   [clojure.test :as test :refer [is testing deftest]]))

(def bar-schema
  {:bar/id     :id
   :bar/field1 :prop
   :bar/field2 :prop})

(def foo-schema
  {:foo/id        :id
   :foo/field1    :prop
   :foo/field2    :prop
   :foo/bar       [:join-one bar-schema]
   [:root/field1 '_] :prop})

[:foo/id {:foo/bar [:bar/field1]}]

{:foo/id  0
 :foo/bar {:bar/field1 1}}

(def foo-schema1
  (merge foo-schema
         {(v/ident [:root/field1]) :prop
          (v/ident [:bar/id 0]) [:join-one bar-schema]}))

(def root-schema
  {:root/foos   [:join-many foo-schema]
   :root/field1 :prop})

(def arbitrary-schema
  {(v/ident [:root/foos]) [:join-many foo-schema]
   })

(deftest test-schema->ast
  (testing "schema"
    (is (= (v2/schema->ast foo-schema)
           {:type     :schema
            :id-key   :foo/id
            :children [{:type         :id
                        :key          :foo/id
                        :dispatch-key :foo/id}
                       {:type         :prop
                        :key          :foo/field1
                        :dispatch-key :foo/field1}
                       {:type         :prop
                        :key          :foo/field2
                        :dispatch-key :foo/field2}
                       {:type         :join-one
                        :key          :foo/bar
                        :dispatch-key :foo/bar
                        :children     [{:type     :schema
                                        :id-key   :bar/id
                                        :children [{:type         :id
                                                    :key          :bar/id
                                                    :dispatch-key :bar/id}
                                                   {:type         :prop
                                                    :key          :bar/field1
                                                    :dispatch-key :bar/field1}
                                                   {:type         :prop
                                                    :key          :bar/field2
                                                    :dispatch-key :bar/field2}]}]}
                       {:type         :prop
                        :key          [:root/field1 '_]
                        :dispatch-key :root/field1}]}))))

(def bar-model
  {:id-key :bar/id
   :fields [:bar/id
            :bar/field1]})

(def baz-model
  {:id-key :baz/id
   :fields [:baz/id
            :baz/field1]})

(def foo-model
  {:id-key :foo/id
   :fields [:foo/id
            :foo/field1
            (v/join-one [:foo/bar bar-model])
            (v/join-many [:foo/baz baz-model])]})

(def foo-1-model
  {:id-key :foo/id
   :fields [:foo/id
            :foo/field]})

(def foo-example
  {:foo/id     0
   :foo/field1 1
   :foo/bar    {:bar/id     0
                :bar/field1 10}
   :foo/baz    [{:baz/id     0
                 :baz/field1 10}
                {:baz/id     1
                 :baz/field1 20}]})

(def foo-ident-example
  (v/as-ident [:foo/id 0]))

(def test-db
  {:foo/id {0 {:foo/id     0
               :foo/field1 1
               :foo/bar    (v/as-ident [:bar/id 0])
               :foo/baz    [(v/as-ident [:baz/id 0])
                            (v/as-ident [:baz/id 1])]}}
   :bar/id {0 {:bar/id     0
               :bar/field1 10}}
   :baz/id {0 {:baz/id     0
               :baz/field1 10}
            1 {:baz/id     1
               :baz/field1 20}}})

(def participants
  [{:participant/id       0
    :participant/icon     "mouse-tail"
    :participant/alias    "Me"
    :ui/geocoding-mode    :forward
    :ui/place-placeholder "Freelance, Bahay ko, Krusty Krab"
    :participant/place    {:place/id       "temporary-server-generated-0"
                           :place/primary  "Ateneo de Manila University, Katipunan Ave"
                           :place/location {:latitude 14.638607, :longitude 121.076782}}
    :ui/temp-place        {:place/id       "temporary-server-generated-0"
                           :place/primary  "Ateneo de Manila University, Katipunan Ave"
                           :place/location {:latitude 14.638607, :longitude 121.076782}}}
   {:participant/id       1
    :participant/icon     "koala-bamboo"
    :participant/alias    "Bae"
    :ui/geocoding-mode    :forward
    :ui/place-placeholder "Sa Metro Manila, E di sa puso mo, Freelance"}])

(def participant-data+dict
  {:data {:value [[:participant/id 0] [:participant/id 1]]},
   :dict
   {:participant/id
    {0 #:participant{:id    0,
                     :alias "Me",
                     :icon  "mouse-tail",
                     :place [:place/id "temporary-server-generated-0"]},
     1 #:participant{:id 1, :alias "Bae", :icon "koala-bamboo", :place nil}},
    :place/id
    {"temporary-server-generated-0"
     #:place{:id        "temporary-server-generated-0",
             :primary   "Ateneo de Manila University, Katipunan Ave",
             :secondary nil,
             :location  {:latitude 14.638607, :longitude 121.076782}}}}})


(def place-model
  {:id-key :place/id
   :fields [:place/id
            :place/primary
            :place/secondary
            :place/source
            :place/location
            :place/place-id]})

(def participant-model
  {:id-key :participant/id
   :fields [:participant/id
            :participant/icon
            :participant/alias
            (v/join-one [:participant/place place-model])
            (v/join-one [:ui/temp-place place-model])
            :ui/geocoding-mode
            :ui/place-placeholder]})

(def category-model
  {:id-key :category/id
   :fields [:category/id
            :category/enabled?]})

(def candidate-model
  {:id-key :candidate/id
   :fields [:candidate/id
            :candidate/location
            :candidate/primary
            :candidate/secondary
            :candidate/type
            :candidate/address]})

(def test-tree-1
  {:root/participants [{:participant/id 0
                        :participant/icon "amphibian-frog"
                        :participant/alias "Me"
                        :ui/geocoding-mode :forward
                        :ui/place-placeholder "Freelance, E di sa puso mo, Sa Metro Manila"
                        :participant/place {:place/id "temporary-server-generated-0"
                                            :place/primary "1768 Simoun St, Barangay 484"
                                            :place/location {:latitude 14.618439, :longitude 120.989298}}
                        :ui/temp-place {:place/id "temporary-server-generated-0"
                                        :place/primary "1768 Simoun St, Barangay 484"
                                        :place/location {:latitude 14.618439, :longitude 120.989298}}}
                       {:participant/id 1
                        :participant/icon "swan"
                        :participant/alias "Honey Bunny"
                        :ui/geocoding-mode :forward
                        :ui/place-placeholder "E di sa puso mo, Bahay ko, Freelance"}]
   :root/categories [{:category/id "Food", :category/enabled? true}
                     {:category/id "Parks and Outdoors", :category/enabled? true}
                     {:category/id "Hotel,Motel,Bed and Breakfast,Resort", :category/enabled? true}
                     {:category/id "Arts and Entertainment", :category/enabled? true}
                     {:category/id "Shopping Center", :category/enabled? true}]
   :root/candidate nil})


(deftest test-db->tree
  (testing "db->tree"
    (is (= foo-example
           (v/db->tree foo-model (v/as-ident [:foo/id 0]) test-db))))
  (testing "db->tree, subquery"
    (is (= {:foo/id  0
            :foo/bar {:bar/id 0}}
           (v/db->tree (v/with-fields foo-model
                         [:foo/id
                          (v/join-one [:foo/bar (v/with-fields bar-model
                                                  [:bar/id])])])
                       (v/as-ident [:foo/id 0])
                       test-db))))
  (testing "db->tree, union"
    (is (= {:onions [{:foo/id 0 :foo/field1 1}
                     {:bar/id 0 :bar/field1 10}
                     {:baz/id 0 :baz/field1 10}]}
           (v/db->tree [(v/join-many [:onions (into #{}
                                                    (map (fn [model]
                                                           (let [model-ns (namespace (:id-key model))]
                                                             (v/with-fields model
                                                               [(keyword model-ns "id")
                                                                (keyword model-ns "field1")]))))
                                                    #{foo-model bar-model baz-model})])]
                       {:onions [(v/as-ident [:foo/id 0])
                                 (v/as-ident [:bar/id 0])
                                 (v/as-ident [:baz/id 0])]}
                       test-db))))
  (testing "db->tree, link-ref"
    (is (= {:herpes [{:foo/id 0 :foo/field1 3 :root/derpes 1}
                     {:foo/id 1 :foo/field1 2 :root/derpes 1}]}
           (v/db->tree [(v/join-many [:herpes (v/with-fields foo-1-model
                                                [:foo/id
                                                 :foo/field1
                                                 (v/as-ident [:root/derpes])])])]
                       {:herpes [(v/as-ident [:foo/id 0])
                                 (v/as-ident [:foo/id 1])]}
                       {:foo/id      {0 {:foo/id     0
                                         :foo/field1 3}
                                      1 {:foo/id     1
                                         :foo/field1 2}}
                        :root/derpes 1})))))

(deftest test-normalize
  (testing "normalize"
    (is (= test-db
           (:dict (v/tree->db foo-model foo-example))))
    (is (= participant-data+dict
           (v/tree->db [(v/join-many [:value (v/with-fields participant-model
                                               [:participant/id
                                                :participant/alias
                                                :participant/icon
                                                (v/join-one [:participant/place (v/with-fields place-model
                                                                                  [:place/id
                                                                                   :place/location
                                                                                   :place/primary
                                                                                   :place/secondary])])])])]
                       {:value participants})))
    (is (= {:participant/id {0 {:participant/id       0,
                                :participant/icon     "amphibian-frog",
                                :participant/alias    "Me",
                                :ui/geocoding-mode    :forward,
                                :ui/place-placeholder "Freelance, E di sa puso mo, Sa Metro Manila",
                                :participant/place    [:place/id "temporary-server-generated-0"],
                                :ui/temp-place        [:place/id "temporary-server-generated-0"]},
                             1 {:participant/id       1,
                                :participant/icon     "swan",
                                :participant/alias    "Honey Bunny",
                                :ui/geocoding-mode    :forward,
                                :ui/place-placeholder "E di sa puso mo, Bahay ko, Freelance",
                                :participant/place    nil,
                                :ui/temp-place        nil}},
            :place/id       {"temporary-server-generated-0"
                             {:place/id        "temporary-server-generated-0",
                              :place/primary   "1768 Simoun St, Barangay 484",
                              :place/secondary nil
                              :place/source    nil
                              :place/place-id  nil
                              :place/location  {:latitude 14.618439, :longitude 120.989298}}},
            :category/id    {"Food" {:category/id "Food", :category/enabled? true},
                             "Parks and Outdoors"
                             {:category/id "Parks and Outdoors", :category/enabled? true},
                             "Hotel,Motel,Bed and Breakfast,Resort"
                             {:category/id       "Hotel,Motel,Bed and Breakfast,Resort",
                              :category/enabled? true},
                             "Arts and Entertainment"
                             {:category/id "Arts and Entertainment", :category/enabled? true},
                             "Shopping Center"
                             {:category/id "Shopping Center", :category/enabled? true}}}
           (:dict
            (v/tree->db [(v/join-many [:root/participants participant-model])
                         (v/join-many [:root/categories category-model])
                         (v/join-one [:root/candidate candidate-model])]
                        test-tree-1))))))
