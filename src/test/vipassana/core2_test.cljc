(ns vipassana.core2-test
  (:require
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

(def root-schema
  {:root/foos   [:join-many foo-schema]
   :root/field1 :prop})

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

(deftest test-normalize
  (testing "normalize"
    (is (= (v2/normalize (v2/schema->ast foo-schema)
                         {:foo/id     0
                          :foo/field1 1
                          :foo/field2 2
                          :foo/bar    {:bar/id     0
                                       :bar/field1 1
                                       :bar/field2 2}})
           {:data [:foo/id 0]
            :dict {:foo/id      {0 {:foo/id     0
                                    :foo/field1 1
                                    :foo/field2 2
                                    :foo/bar    [:bar/id 0]}}
                   :bar/id      {0 {:bar/id     0
                                    :bar/field1 1
                                    :bar/field2 2}}
                   :root/field1 nil}}))))
