(ns cheshire.decoder
  (:import (org.codehaus.jackson JsonToken JsonParser JsonFactory
                                 JsonParser$Feature)
           (java.io StringReader)))

(def #^JsonFactory factory
  (doto (JsonFactory.)
    (.configure JsonParser$Feature/ALLOW_UNQUOTED_CONTROL_CHARS true)))

(defn decode*
  "doc"
  [#^JsonParser parser first keywords?]
  (when first (.nextToken parser))
  (when-let [token (.getCurrentToken parser)]
    (println :token token)
    (println (= JsonToken/START_OBJECT token))
    (condp (partial = token)
        JsonToken/START_OBJECT
      (do
        (println :start)
        (.nextToken parser)
        (loop [m (transient {})]
          (println :inner-token (.getCurrentToken parser))
          (if (= (.getCurrentToken parser) JsonToken/END_OBJECT)
            (persistent! m)
            (do
              (let [key (.getText parser)]
                (.nextToken parser)
                (let [m (assoc! m (if keywords? (keyword key) key)
                                (decode* parser false keywords?))]
                  (.nextToken parser)
                  (recur m)))))))

      JsonToken/START_ARRAY
      (do
        (println :start-array)
        (.nextToken parser)
        (loop [v (transient [])]
          (println :inner-token (.getCurrentToken parser))
          (if (= (.getCurrentToken parser) JsonToken/END_ARRAY)
            (persistent! v)
            (do
              (let [key (.getText parser)]
                (.nextToken parser)
                (let [v (conj! v (decode* parser false keywords?))]
                  (.nextToken parser)
                  (recur v)))))))

      JsonToken/VALUE_STRING
      (.getText parser)

      JsonToken/VALUE_NUMBER_INT
      (.getNumberValue parser)

      JsonToken/VALUE_NUMBER_FLOAT
      (.getDoubleValue parser)

      JsonToken/VALUE_TRUE
      true

      JsonToken/VALUE_FALSE
      false

      JsonToken/VALUE_NULL
      nil

      :default (throw (Exception. (str "Couldn't decode: " token))))))

(defn decode
  ([s] (decode s false))
  ([s keywords?] (decode true s keywords?))
  ([first s keywords?]
     (decode* (.createJsonParser factory (StringReader. s)) first keywords?)))

