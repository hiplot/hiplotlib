#pkill R
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/miniconda/3/lib/:/usr/local/lib:/usr/local/lib64:/usr/lib64
rm /app/hiplot/data/plumber/locks/* 
rm /app/hiplot/data/plumber/active.pool
kill -9 `ps axu | grep start.R | grep -v ' dev' |awk '{print $2}'`
mkdir -p /app/hiplot/log/plumber/
export NODE_OPTIONS="--max-old-space-size=18192"
for i in {8020..8029}
do
  pm2 start "Rscript start.R ${i} 127.0.0.1 nightly" --name plumber-nightly-${i} --max-memory-restart 12000M --exp-backoff-restart-delay=100
  sleep 1
done

wait
