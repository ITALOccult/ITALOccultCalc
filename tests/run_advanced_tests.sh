#!/bin/bash
set -e
cd "$(dirname "$0")/../build" || exit 1

echo "=== TEST SHADOW PATH GEOMETRY ==="
./tests/test_shadow_path_geometry
echo "PASS"

echo "=== TEST KNOWN OCCULTATION (ref) ==="
./tests/test_known_occultation_ref
echo "PASS"

echo "=== TEST FRAME CONSISTENCY FULL ==="
./tests/test_frame_consistency_full
echo "PASS"

echo "=== TEST PERFORMANCE ==="
./tests/test_performance_shadow
echo "PASS"

echo "=== ALL ADVANCED TESTS PASSED ==="
