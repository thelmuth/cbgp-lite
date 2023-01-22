clj -X:benchmarks erp12.cbgp-lite.benchmark.ga/run \
  :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
  :data-dir '"data/psb"' \
  :problem "fuel-cost" | tee results/bushiness/out-fuel-cost.txt

clj -X:benchmarks erp12.cbgp-lite.benchmark.ga/run \
  :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
  :data-dir '"data/psb"' \
  :problem "substitution-cipher" | tee results/bushiness/out-substitution-cipher.txt

clj -X:benchmarks erp12.cbgp-lite.benchmark.ga/run \
  :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
  :data-dir '"data/psb"' \
  :problem "middle-character" | tee results/bushiness/out-middle-character.txt

clj -X:benchmarks erp12.cbgp-lite.benchmark.ga/run \
  :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
  :data-dir '"data/psb"' \
  :problem "vector-average" | tee results/bushiness/out-vector-average.txt

clj -X:benchmarks erp12.cbgp-lite.benchmark.ga/run \
  :suite-ns erp12.cbgp-lite.benchmark.suite.psb \
  :data-dir '"data/psb"' \
  :problem "negative-to-zero" | tee results/bushiness/out-negative-to-zero.txt

