(ns test.vipassana.core-test
  (:require
   [vipassana.core :as v]
   [clojure.test :as test :refer [is testing deftest]]))

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
                 :baz/field1 20}]})

(def foo-ident-example
  #v/ident [:foo/id 0])

(def test-db
  {:foo/id {0 {:foo/id     0
               :foo/field1 1
               :foo/bar    #v/ident [:bar/id 0]
               :foo/baz    [#v/ident [:baz/id 0]
                            #v/ident [:baz/id 1]]}}
   :bar/id {0 {:bar/id     0
               :bar/field1 10}}
   :baz/id {0 {:baz/id     0
               :baz/field1 10}
            1 {:baz/id     1
               :baz/field1 20}}})

(def place-model
  {:id-key :place/id
   :fields [:place/id
            :place/primary
            :place/secondary
            :place/location]})

(def participant-model
  {:id-key :participant/id
   :fields [:participant/id
            :participant/alias
            :participant/icon
            #v/join-one [:participant/place place-model]]})

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


(deftest test-db->tree
  (testing "db->tree"
    (is (= foo-example
           (v/db->tree foo-model #v/ident [:foo/id 0] test-db))))
  (testing "db->tree, subquery"
    (is (= {:foo/id  0
            :foo/bar {:bar/id 0}}
           (v/db->tree (v/with-fields foo-model
                         [:foo/id
                          #v/join-one [:foo/bar (v/with-fields bar-model
                                                  [:bar/id])]])
                       #v/ident [:foo/id 0]
                       test-db))))
  (testing "db->tree, union"
    (is (= {:onions [{:foo/id 0 :foo/field1 1}
                     {:bar/id 0 :bar/field1 10}
                     {:baz/id 0 :baz/field1 10}]}
           (v/db->tree [#v/join-many [:onions (into #{}
                                                    (map (fn [model]
                                                           (let [model-ns (namespace (:id-key model))]
                                                             (v/with-fields model
                                                               [(keyword model-ns "id")
                                                                (keyword model-ns "field1")]))))
                                                    #{foo-model bar-model baz-model})]]
                       {:onions [#v/ident [:foo/id 0]
                                 #v/ident [:bar/id 0]
                                 #v/ident [:baz/id 0]]}
                       test-db)))))

(deftest test-normalize
  (testing "normalize"
    (is (= test-db
           (:dict (v/normalize foo-model foo-example))))
    (is (= participant-data+dict
           (v/tree->db [#v/join-many [:value participant-model]]
                       {:value participants})))))
