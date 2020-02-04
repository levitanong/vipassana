(ns test.vipassana.core-test
  (:require
   [vipassana.core :as v]
   [clojure.test :as test :refer [is testing deftest]]))

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

(deftest test-db->tree
  (testing "db->tree"
    (is (= foo-example
           (v/db->tree test-db foo-model #v/ident [:foo/id 0]))))
  (testing "db->tree, subquery"
    (is (= {:foo/id  0
            :foo/bar {:bar/id 0}}
           (v/db->tree test-db
                       (v/with-query foo-model
                         [:foo/id
                          #v/join-one [:foo/bar (v/with-query bar-model
                                                  [:bar/id])]])
                       #v/ident [:foo/id 0])))))

(deftest test-normalize
  (testing "normalize"
    (is (= test-db
           (:dict (v/normalize foo-model foo-example))))))
