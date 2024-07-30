#!/bin/sh

# Read the PIDs from the file
NODE_PID=$(cat /tmp/node_bridge.pid)
HTTP_SERVER_PID=$(cat /tmp/http_server.pid)

# Kill the processes
kill $NODE_PID
kill $HTTP_SERVER_PID

# Remove the PID files
rm /tmp/node_bridge.pid
rm /tmp/http_server.pid

echo "Processes stopped."
