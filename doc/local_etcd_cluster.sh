## just copy out the scripts, and then paste to different console, you will get a cluster opening port 2379,2381,2383 locally
./etcd --name infra0 --initial-advertise-peer-urls http://localhost:2380 \
  --listen-peer-urls http://localhost:2380 \
  --listen-client-urls http://localhost:2379 \
  --advertise-client-urls http://localhost:2379 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://localhost:2380,infra1=http://localhost:2382,infra2=http://localhost:2384 \
  --initial-cluster-state new

./etcd --name infra1 --initial-advertise-peer-urls http://localhost:2382 \
  --listen-peer-urls http://localhost:2382 \
  --listen-client-urls http://localhost:2381 \
  --advertise-client-urls http://localhost:2381 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://localhost:2382,infra1=http://localhost:2382,infra2=http://localhost:2384 \
  --initial-cluster-state new

./etcd --name infra2 --initial-advertise-peer-urls http://localhost:2384 \
  --listen-peer-urls http://localhost:2384 \
  --listen-client-urls http://localhost:2383 \
  --advertise-client-urls http://localhost:2383 \
  --initial-cluster-token etcd-cluster-1 \
  --initial-cluster infra0=http://localhost:2380,infra1=http://localhost:2382,infra2=http://localhost:2384 \
  --initial-cluster-state new
