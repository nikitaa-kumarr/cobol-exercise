#!/usr/bin/env python3
"""
Run Evaluation Script
Executes evaluate_refactor.py to grade the COBOL to Python migration

This script:
- Imports and runs the evaluation system
- Displays the grading report
- Returns appropriate exit code

Usage:
    python run_evaluation.py

Exit codes:
    0 - All tests passed (100% score)
    1 - Some tests failed
    2 - Fatal error during evaluation
"""

import sys
import os


def main():
    """
    Execute the evaluation system and display results

    Returns:
        int: Exit code from evaluation
    """
    print("=" * 80)
    print(" " * 25 + "MIGRATION EVALUATION RUNNER")
    print("=" * 80)
    print()
    print("This script evaluates the Python migration (modern_app.py)")
    print("against the original COBOL behavior (legacy_app.cob)")
    print("using the comprehensive test suite (behavioral_tests.py).")
    print()
    print("Checking dependencies...")

    # Check if required files exist
    required_files = [
        'modern_app.py',
        'behavioral_tests.py',
        'evaluate_refactor.py',
        'behavioral_spec.md'
    ]

    missing_files = []
    for filename in required_files:
        if not os.path.exists(filename):
            missing_files.append(filename)

    if missing_files:
        print("\nERROR: Missing required files:")
        for filename in missing_files:
            print(f"  - {filename}")
        print("\nPlease ensure all files are in the current directory.")
        return 2

    print("✓ All required files found")
    print()

    # Import and run evaluation
    try:
        import evaluate_refactor

        print("Running evaluation system...")
        print()

        # Execute evaluation
        exit_code = evaluate_refactor.main()

        # Additional summary
        print()
        print("=" * 80)
        print("EVALUATION COMPLETE")
        print("=" * 80)

        if exit_code == 0:
            print("Result: ✓ ALL TESTS PASSED")
            print("The Python migration successfully replicates all COBOL behaviors.")
        elif exit_code == 1:
            print("Result: ✗ SOME TESTS FAILED")
            print("The Python migration does not fully match COBOL behavior.")
            print("Review the failed tests above for details.")
        else:
            print("Result: ✗ EVALUATION ERROR")
            print("An error occurred during evaluation.")

        print("=" * 80)

        return exit_code

    except ImportError as e:
        print(f"\nERROR: Cannot import evaluate_refactor.py: {e}")
        print("Ensure evaluate_refactor.py is in the current directory.")
        return 2

    except Exception as e:
        print(f"\nFATAL ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 2


if __name__ == "__main__":
    # Change to script directory
    script_dir = os.path.dirname(os.path.abspath(__file__))
    if script_dir:
        os.chdir(script_dir)

    # Run evaluation
    exit_code = main()

    # Exit with appropriate code
    sys.exit(exit_code)
