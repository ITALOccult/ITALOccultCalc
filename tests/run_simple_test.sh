#!/bin/bash
cd "$(dirname "$0")/../build" || exit 1
make test_frame_consistency 2>&1 | tail -5
./tests/test_frame_consistency
