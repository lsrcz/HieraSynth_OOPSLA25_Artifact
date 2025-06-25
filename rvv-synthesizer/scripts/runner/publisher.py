#!/usr/bin/env python3

import argparse
import json
import time
import logging
import uuid
import redis
import sys

# -----------------------
# Command Line Argument Parsing
# -----------------------
parser = argparse.ArgumentParser(description="Job Publisher for Distributed Scheduler")
parser.add_argument("--redis-host", type=str, default="your_redis_host",
                    help="Redis server hostname (default: your_redis_host)")
parser.add_argument("--redis-port", type=int, default=6379,
                    help="Redis server port (default: 6379)")
parser.add_argument("--command", type=str,
                    help="Command to execute (required for simple mode)")
parser.add_argument("--cores", type=int, default=1,
                    help="Number of CPU cores required for the job (default: 1)")
parser.add_argument("--batch-file", type=str,
                    help="Path to JSON file containing multiple jobs to submit")
parser.add_argument("--stdin", action="store_true",
                    help="Read batch jobs as JSON from stdin")
parser.add_argument("--job-id", type=str,
                    help="Custom job ID (optional, will generate UUID if not provided)")
parser.add_argument("--redis-password", type=str, default=None,
                    help="Password for Redis (default: None)")
args = parser.parse_args()

# -----------------------
# Logging Configuration
# -----------------------
logging.basicConfig(
    level=logging.INFO,
    format='[%(asctime)s] %(levelname)s: %(message)s'
)

# -----------------------
# Redis Connection Handling
# -----------------------
def connect_redis(host, port, password):
    """Attempt to connect to Redis, with a few retries."""
    retries = 3
    while retries > 0:
        try:
            r_instance = redis.Redis(host=host, port=port, password=password, db=0)
            r_instance.ping()
            logging.info("Connected to Redis at %s:%d", host, port)
            return r_instance
        except redis.RedisError as e:
            retries -= 1
            if retries == 0:
                logging.error("Failed to connect to Redis after multiple attempts: %s", e)
                sys.exit(1)
            logging.error("Could not connect to Redis: %s. Retrying in 2 seconds...", e)
            time.sleep(2)

# -----------------------
# Job Publishing
# -----------------------
def publish_job(r, job):
    """
    Publish a job to Redis:
    1. Add the job to the 'pending_jobs' sorted set with score = cores required
    2. Publish a notification to the 'job_notification' channel
    """
    try:
        # Ensure job has all required fields
        if not all(k in job for k in ("id", "command", "cores")):
            logging.error("Job missing required fields: %s", job)
            return False
        
        # Convert job to JSON
        job_json = json.dumps(job)
        
        # Add job to sorted set, score = cores required
        r.zadd("pending_jobs", {job_json: job['cores']})
        
        # Publish notification
        r.publish("job_notification", f"New job {job['id']} added")
        
        logging.info("Published job %s (requires %s cores): %s", 
                    job['id'], job['cores'], job['command'])
        return True
    except (redis.RedisError, TypeError, ValueError) as e:
        logging.error("Error publishing job: %s", e)
        return False

def publish_jobs_from_json(r, jobs_data):
    """Process and publish a list of jobs from JSON data."""
    try:
        if not isinstance(jobs_data, list):
            logging.error("Jobs data must be a list of job objects")
            return 0
        
        successful = 0
        for job in jobs_data:
            # Generate ID if not present
            if 'id' not in job:
                job['id'] = str(uuid.uuid4())
                
            if publish_job(r, job):
                successful += 1
        
        logging.info("Published %d/%d jobs", successful, len(jobs_data))
        return successful
    except (TypeError, ValueError) as e:
        logging.error("Error processing jobs data: %s", e)
        return 0

def publish_from_batch_file(r, file_path):
    """Read multiple jobs from a JSON file and publish them."""
    try:
        with open(file_path, 'r') as f:
            jobs = json.load(f)
        
        return publish_jobs_from_json(r, jobs)
    except (json.JSONDecodeError, FileNotFoundError) as e:
        logging.error("Error reading batch file: %s", e)
        return 0
        
def publish_from_stdin(r):
    """Read multiple jobs as JSON from stdin and publish them."""
    try:
        logging.info("Reading jobs from stdin...")
        jobs = json.load(sys.stdin)
        return publish_jobs_from_json(r, jobs)
    except json.JSONDecodeError as e:
        logging.error("Error parsing JSON from stdin: %s", e)
        return 0

def main():
    # Connect to Redis
    r = connect_redis(args.redis_host, args.redis_port, args.redis_password)
    
    if args.stdin:
        # Batch mode: read jobs from stdin
        publish_from_stdin(r)
    elif args.batch_file:
        # Batch mode: publish multiple jobs from file
        publish_from_batch_file(r, args.batch_file)
    elif args.command:
        # Simple mode: publish a single job from command line arguments
        job = {
            "id": args.job_id or str(uuid.uuid4()),
            "command": args.command,
            "cores": args.cores,
            "submitted_at": time.time()
        }
        publish_job(r, job)
    else:
        logging.error("One of --command, --batch-file, or --stdin must be specified")
        parser.print_help()
        sys.exit(1)

if __name__ == "__main__":
    main()
