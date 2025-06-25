import argparse
import json
import time
import socket
import logging
import subprocess
import redis
import signal

shutdown_flag = False

def handle_sigint(signum, frame):
    global shutdown_flag
    shutdown_flag = True
    logging.info("SIGINT received. Initiating graceful shutdown.")

signal.signal(signal.SIGINT, handle_sigint)

# -----------------------
# Command Line Argument Parsing
# -----------------------
parser = argparse.ArgumentParser(description="Distributed Job Scheduler (Async Execution)")
parser.add_argument("--total-cores", type=int, default=4,
                    help="Total number of CPU cores available on this node (default: 4)")
parser.add_argument("--redis-host", type=str, default="your_redis_host",
                    help="Redis server hostname (default: your_redis_host)")
parser.add_argument("--redis-port", type=int, default=6379,
                    help="Redis server port (default: 6379)")
parser.add_argument("--log-level", type=str, default="INFO",
                    help="Logging level (default: INFO)")
parser.add_argument("--report-interval", type=int, default=300,
                    help="Reporting interval in seconds (default: 300)")
parser.add_argument("--redis-password", type=str, default=None,
                    help="Password for Redis (default: None)")
args = parser.parse_args()

# -----------------------
# Logging Configuration
# -----------------------
logging.basicConfig(
    level=getattr(logging, args.log_level.upper(), logging.INFO),
    format='[%(asctime)s] %(levelname)s: %(message)s'
)

# -----------------------
# Configuration from Command Line Arguments
# -----------------------
TOTAL_CORES = args.total_cores
available_cores = TOTAL_CORES  # Global variable tracking free cores
NODE_ID = socket.gethostname()
REDIS_HOST = args.redis_host
REDIS_PORT = args.redis_port
REPORT_INTERVAL = args.report_interval
REDIS_PASSWORD = args.redis_password

# -----------------------
# Redis Connection Handling
# -----------------------
def connect_redis():
    """Attempt to connect to Redis indefinitely until successful."""
    while True:
        try:
            r_instance = redis.Redis(host=REDIS_HOST, port=REDIS_PORT, password=REDIS_PASSWORD, db=0)
            r_instance.ping()
            logging.info("Connected to Redis at %s:%d", REDIS_HOST, REDIS_PORT)
            return r_instance
        except redis.RedisError as e:
            logging.error("Could not connect to Redis: %s. Retrying in 5 seconds...", e)
            time.sleep(5)

r = connect_redis()

# -----------------------
# Lua Script for Atomic Job Fetching
# -----------------------
lua_script_code = """
local key = KEYS[1]
local available = tonumber(ARGV[1])
local candidate = redis.call('ZRANGEBYSCORE', key, '-inf', available, 'LIMIT', 0, 1)
if (#candidate == 0) then
    return nil
end
redis.call('ZREM', key, candidate[1])
return candidate[1]
"""

def register_lua_script(redis_conn):
    try:
        return redis_conn.register_script(lua_script_code)
    except redis.RedisError as e:
        logging.error("Failed to register Lua script: %s", e)
        return None

fetch_job = register_lua_script(r)

def ensure_redis_connection():
    """Ensure that Redis is connected; if not, reconnect and re-register the Lua script."""
    global r, fetch_job
    try:
        r.ping()
    except redis.RedisError as e:
        logging.error("Lost Redis connection: %s. Reconnecting...", e)
        r = connect_redis()
        fetch_job = register_lua_script(r)
    return r

# -----------------------
# Job Fetching and Validation
# -----------------------
def get_pending_job(current_available):
    """
    Atomically fetch a job from the sorted set "pending_jobs" whose required cores
    are <= current_available. Returns the job as a dict or None if no job exists.
    """
    ensure_redis_connection()
    try:
        result = fetch_job(keys=["pending_jobs"], args=[current_available])
        if result:
            try:
                job = json.loads(result)
                if not all(k in job for k in ("id", "command", "cores")):
                    logging.error("Malformed job data: %s", result)
                    return None
                return job
            except json.JSONDecodeError as je:
                logging.error("JSON decode error: %s. Data: %s", je, result)
                return None
    except redis.RedisError as e:
        logging.error("Error fetching job from Redis: %s", e)
    return None

def push_job_back(job):
    """
    If a job cannot be executed due to insufficient available cores,
    push it back into the sorted set.
    """
    ensure_redis_connection()
    try:
        job_json = json.dumps(job)
        r.zadd("pending_jobs", {job_json: job['cores']})
        logging.info("Pushed job %s back to Redis", job.get('id', 'unknown'))
    except (redis.RedisError, TypeError, ValueError) as e:
        logging.error("Error pushing job back to Redis: %s", e)

# -----------------------
# Running Jobs Asynchronously and Tracking Them
# -----------------------
running_jobs = []

def poll_running_jobs():
    """
    Poll the list of running jobs, and if any have finished,
    free up the cores they were using and log their outputs.
    The stdout and stderr are logged as debug, so they will only show
    if the log level is set to DEBUG.
    """
    global available_cores
    finished_jobs = []
    for job_info in running_jobs:
        process = job_info['process']
        if process.poll() is not None:  # Job finished
            stdout, stderr = process.communicate()
            job_id = job_info['job'].get('id', 'unknown')
            if stdout:
                for line in stdout.splitlines():
                    logging.debug("[%s/stdout] %s", job_id, line)
            if stderr:
                for line in stderr.splitlines():
                    logging.debug("[%s/stderr] %s", job_id, line)
            finished_jobs.append(job_info)
    for job_info in finished_jobs:
        available_cores += job_info['cores']
        logging.info("[%s] Job %s finished, freed %d cores (available: %d)",
                     NODE_ID, job_info['job'].get('id', 'unknown'),
                     job_info['cores'], available_cores)
        running_jobs.remove(job_info)

def run_job_async(job, required_cores):
    """
    Launch the job asynchronously using subprocess.Popen and record it in running_jobs.
    Capture output by redirecting stdout and stderr.
    """
    job_id = job.get('id', 'unknown')
    command = job['command']
    logging.info("[%s] Starting job %s: %s (using %d cores)", NODE_ID, job_id, command, required_cores)
    try:
        process = subprocess.Popen(
            command, shell=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            text=True
        )
        running_jobs.append({"job": job, "process": process, "cores": required_cores})
    except Exception as e:
        logging.exception("Exception while starting job %s: %s", job_id, e)
        available_cores += required_cores  # Release cores if job fails to start

def cleanup_running_jobs():
    """
    Terminate any running jobs, push them back to the queue, and free up their cores.
    """
    global available_cores
    for job_info in running_jobs:
        job_id = job_info['job'].get('id', 'unknown')
        process = job_info['process']
        if process.poll() is None:
            try:
                process.terminate()
                process.wait(timeout=5)
            except Exception as e:
                logging.error("Error terminating job %s: %s", job_id, e)
        push_job_back(job_info['job'])
        available_cores += job_info['cores']
        logging.info("[%s] Job %s pushed back to queue.", NODE_ID, job_id)
    running_jobs.clear()

# -----------------------
# Main Scheduler Loop with Pub/Sub
# -----------------------
def scheduler_loop(pubsub):
    global available_cores
    last_report_time = time.time()
    while not shutdown_flag:
        try:
            poll_running_jobs()

            # Report running jobs and available cores periodically.
            current_time = time.time()
            if current_time - last_report_time >= REPORT_INTERVAL:
                running_count = len(running_jobs)
                running_job_ids = [job_info['job'].get('id', 'unknown') for job_info in running_jobs]
                logging.info("[%s] Report: %d running jobs, %d remaining cores. Running job IDs: %s",
                             NODE_ID, running_count, available_cores, ", ".join(running_job_ids))
                last_report_time = current_time

            current_available = available_cores
            if current_available > 0:
                job = get_pending_job(current_available)
                if job:
                    try:
                        required_cores = int(job['cores'])
                    except ValueError:
                        logging.error("Invalid 'cores' value for job %s: %s", job.get('id', 'unknown'), job['cores'])
                        continue
                    if required_cores <= available_cores:
                        available_cores -= required_cores
                        run_job_async(job, required_cores)
                        time.sleep(0.5)
                        continue
                    else:
                        push_job_back(job)
                        time.sleep(0.5)
                        continue
            message = pubsub.get_message(timeout=1)
            if message and message['type'] == 'message':
                logging.info("[%s] Received job notification: %s", NODE_ID, message['data'])
            else:
                time.sleep(0.5)
        except Exception as e:
            logging.exception("Error in scheduler loop: %s", e)
            time.sleep(1)
    cleanup_running_jobs()

def main():
    logging.info("[%s] Distributed scheduler starting with %d cores.", NODE_ID, TOTAL_CORES)
    ensure_redis_connection()
    pubsub = r.pubsub()
    try:
        pubsub.subscribe("job_notification")
    except redis.RedisError as e:
        logging.error("Failed to subscribe to pub/sub: %s", e)
        return

    try:
        scheduler_loop(pubsub)
    finally:
        try:
            pubsub.close()
        except Exception as e:
            logging.error("Error closing pubsub: %s", e)
        logging.info("[%s] Scheduler terminated.", NODE_ID)

if __name__ == "__main__":
    main()
