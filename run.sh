#!/bin/sh

# Function to clean up and kill processes
cleanup() {
    echo "Stopping processes..."
    kill $NODE_PID
    kill $PYTHON_HTTP_SERVER
    echo "Processes stopped."
    exit 0
}

# Trap signals and call cleanup function
trap cleanup SIGINT SIGTERM

# Start node process and save its PID
node osc-web/bridge.js &
NODE_PID=$!

# Start http-server process and save its PID
python -m http.server 8080 &
HTTP_SERVER_PID=$!

# Save the PIDs to a file for reference
echo $NODE_PID > /tmp/node_bridge.pid
echo $PYTHON_HTTP_SERVER > /tmp/http_server.pid

echo "Processes started. NODE_PID: $NODE_PID, HTTP_SERVER_PID: $PYTHON_HTTP_SERVER"

# Wait indefinitely so the script doesn't exit immediately
while true; do
    sleep 1
done