#!/bin/bash
set -o nounset -o errexit -o pipefail

function tickle() {
    while [ true ]; do
        echo "[$(date +%H:%M:%S)] Tickling..."
        sleep 60
    done
}

echo "Forking tickle process..."
tickle &
TICKLE_PID=$!

echo "Forking build process..."
eval $@ &
BUILD_PID=$!

echo "Waiting for build thread ($BUILD_PID)..."
wait $BUILD_PID

echo "Killing tickle thread ($TICKLE_PID)..."
kill $TICKLE_PID
echo "All done!"
