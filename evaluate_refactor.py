#!/usr/bin/env python3
"""
Evaluation System for COBOL to Python Migration
Grades modern_app.py against behavioral_tests.py

Provides:
- Test execution and scoring
- Detailed pass/fail breakdown
- Feature compliance report
- COBOL behavior verification
- Human-readable grading report

Exit codes:
- 0: All tests passed (100% score)
- 1: Some tests failed
"""

import unittest
import sys
from io import StringIO
from datetime import datetime


def run_evaluation():
    """
    Execute behavioral test suite and generate evaluation report

    Returns:
        dict: Evaluation results containing:
            - total_tests: Total number of tests run
            - passed: Number of tests passed
            - failed: Number of tests failed
            - errors: Number of tests with errors
            - score: Percentage score (0-100)
            - failures: List of failed test details
            - errors_list: List of error details
            - success: Boolean indicating 100% pass
    """
    # Import test suite
    try:
        import behavioral_tests
    except ImportError as e:
        print(f"ERROR: Cannot import behavioral_tests.py: {e}")
        sys.exit(2)

    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Load all test classes
    test_classes = [
        behavioral_tests.TestUserRegistration,
        behavioral_tests.TestUserLogin,
        behavioral_tests.TestPasswordChange,
        behavioral_tests.TestStringPaddingBehavior,
        behavioral_tests.TestMenuSystem,
        behavioral_tests.TestTokenGeneration,
        behavioral_tests.TestDataStructureInitialization,
        behavioral_tests.TestCompleteUserJourney,
        behavioral_tests.TestErrorMessages,
    ]

    for test_class in test_classes:
        suite.addTests(loader.loadTestsFromTestCase(test_class))

    # Run tests with custom result collector
    stream = StringIO()
    runner = unittest.TextTestRunner(stream=stream, verbosity=2)
    result = runner.run(suite)

    # Calculate metrics
    total_tests = result.testsRun
    passed = total_tests - len(result.failures) - len(result.errors)
    failed = len(result.failures)
    errors = len(result.errors)
    score = (passed / total_tests * 100) if total_tests > 0 else 0

    # Collect failure details
    failure_details = []
    for test, traceback in result.failures:
        failure_details.append({
            'test': str(test),
            'traceback': traceback
        })

    # Collect error details
    error_details = []
    for test, traceback in result.errors:
        error_details.append({
            'test': str(test),
            'traceback': traceback
        })

    return {
        'total_tests': total_tests,
        'passed': passed,
        'failed': failed,
        'errors': errors,
        'score': score,
        'failures': failure_details,
        'errors_list': error_details,
        'success': (failed == 0 and errors == 0),
        'test_output': stream.getvalue()
    }


def categorize_tests(results):
    """
    Categorize test results by feature area

    Args:
        results: Evaluation results dictionary

    Returns:
        dict: Feature categories with pass/fail status
    """
    # Import to get test class info
    import behavioral_tests

    categories = {
        'User Registration': {
            'tests': [],
            'class': 'TestUserRegistration',
            'cobol_behaviors': [
                'Register user successfully',
                'Reject duplicate username',
                'Reject when DB full (100 users)',
                'Empty username/password allowed',
                '20-char truncation',
                'Case-sensitive usernames'
            ]
        },
        'User Login': {
            'tests': [],
            'class': 'TestUserLogin',
            'cobol_behaviors': [
                'Login with valid credentials',
                'Reject invalid credentials',
                'Reject inactive users',
                'Generate valid 6-digit token (100000-999999)',
                'Token maps to correct user',
                'Multiple sessions same user',
                'Reject when session table full (50 sessions)'
            ]
        },
        'Password Change': {
            'tests': [],
            'class': 'TestPasswordChange',
            'cobol_behaviors': [
                'Change password with valid token + old password',
                'Reject invalid token',
                'Reject wrong old password',
                'CRITICAL BUG: Line 210 flag behavior',
                'Token remains valid after change',
                'Token collision uses first match'
            ]
        },
        'String Padding (COBOL PIC X(20))': {
            'tests': [],
            'class': 'TestStringPaddingBehavior',
            'cobol_behaviors': [
                '20-character space padding',
                'Truncation at 20 chars',
                'Space-sensitive comparison'
            ]
        },
        'Menu System': {
            'tests': [],
            'class': 'TestMenuSystem',
            'cobol_behaviors': [
                'Menu choices 1-4',
                'Invalid choice handling',
                'Input truncation (PIC 9)'
            ]
        },
        'Token Generation': {
            'tests': [],
            'class': 'TestTokenGeneration',
            'cobol_behaviors': [
                'Range: 100000-999999',
                'Integer values',
                'Pseudo-random (not cryptographic)'
            ]
        },
        'Data Structures': {
            'tests': [],
            'class': 'TestDataStructureInitialization',
            'cobol_behaviors': [
                '100 user slots initialized',
                '50 session slots initialized',
                'Counters start at 0'
            ]
        },
        'Integration Tests': {
            'tests': [],
            'class': 'TestCompleteUserJourney',
            'cobol_behaviors': [
                'Complete user journey',
                'Session table full scenario',
                'Database full scenario'
            ]
        },
        'Error Messages': {
            'tests': [],
            'class': 'TestErrorMessages',
            'cobol_behaviors': [
                'Exact COBOL error message text',
                'Exact COBOL success message text'
            ]
        }
    }

    return categories


def print_evaluation_report(results):
    """
    Print human-readable evaluation report

    Args:
        results: Evaluation results dictionary

    Returns:
        int: Exit code (0 for success, 1 for failures)
    """
    print("\n" + "=" * 80)
    print(" " * 20 + "COBOL TO PYTHON MIGRATION EVALUATION")
    print("=" * 80)
    print(f"Evaluation Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"Test Suite: behavioral_tests.py")
    print(f"System Under Test: modern_app.py")
    print(f"Original COBOL: legacy_app.cob (USERMGMT, 1987-03-15)")
    print("=" * 80)

    # Overall Score
    print("\n" + "─" * 80)
    print("OVERALL SCORE")
    print("─" * 80)
    score = results['score']
    total = results['total_tests']
    passed = results['passed']
    failed = results['failed']
    errors = results['errors']

    # Color coding for terminal (basic)
    if score == 100:
        score_label = "EXCELLENT"
        grade = "A+"
    elif score >= 90:
        score_label = "VERY GOOD"
        grade = "A"
    elif score >= 80:
        score_label = "GOOD"
        grade = "B"
    elif score >= 70:
        score_label = "SATISFACTORY"
        grade = "C"
    elif score >= 60:
        score_label = "NEEDS IMPROVEMENT"
        grade = "D"
    else:
        score_label = "UNSATISFACTORY"
        grade = "F"

    print(f"Score: {score:.1f}% ({score_label}) - Grade: {grade}")
    print(f"Tests Passed: {passed}/{total}")
    print(f"Tests Failed: {failed}/{total}")
    print(f"Tests Errors: {errors}/{total}")

    # Feature Breakdown
    print("\n" + "─" * 80)
    print("FEATURE COMPLIANCE BREAKDOWN")
    print("─" * 80)

    categories = categorize_tests(results)

    # Analyze which tests passed/failed by examining test names
    for category_name, category_info in categories.items():
        class_name = category_info['class']

        # Count tests for this category
        category_passed = 0
        category_total = 0
        category_failures = []

        for failure in results['failures']:
            test_name = failure['test']
            if class_name in test_name:
                category_total += 1
                category_failures.append(test_name)

        for error in results['errors_list']:
            test_name = error['test']
            if class_name in test_name:
                category_total += 1
                category_failures.append(test_name)

        # Estimate total tests in category (from test output or assume proportional)
        # This is a simple heuristic
        if category_name == 'User Registration':
            estimated_total = 8
        elif category_name == 'User Login':
            estimated_total = 8
        elif category_name == 'Password Change':
            estimated_total = 8
        elif category_name == 'String Padding (COBOL PIC X(20))':
            estimated_total = 3
        elif category_name == 'Menu System':
            estimated_total = 5
        elif category_name == 'Token Generation':
            estimated_total = 3
        elif category_name == 'Data Structures':
            estimated_total = 3
        elif category_name == 'Integration Tests':
            estimated_total = 3
        elif category_name == 'Error Messages':
            estimated_total = 2
        else:
            estimated_total = category_total

        category_passed = estimated_total - category_total
        category_status = "✓ PASS" if category_total == 0 else "✗ FAIL"

        print(f"\n{category_name}: {category_status}")
        print(f"  Passed: {category_passed}/{estimated_total}")

        if category_total > 0:
            print(f"  Failed Tests:")
            for failure in category_failures[:5]:  # Show first 5
                test_method = failure.split('(')[0].replace('test_', '').replace('_', ' ')
                print(f"    - {test_method}")

        print(f"  COBOL Behaviors Tested:")
        for behavior in category_info['cobol_behaviors']:
            print(f"    • {behavior}")

    # Failed Tests Detail
    if failed > 0 or errors > 0:
        print("\n" + "─" * 80)
        print("FAILED TESTS DETAIL")
        print("─" * 80)

        if failed > 0:
            print(f"\n{failed} Test Failure(s):")
            for i, failure in enumerate(results['failures'][:10], 1):  # Show first 10
                test_name = failure['test'].split('(')[0].replace('test_', '').replace('_', ' ')
                print(f"\n  {i}. {test_name}")
                print(f"     Test: {failure['test']}")
                # Print first line of traceback
                lines = failure['traceback'].split('\n')
                for line in lines:
                    if 'AssertionError' in line or 'Error' in line:
                        print(f"     {line.strip()}")
                        break

        if errors > 0:
            print(f"\n{errors} Test Error(s):")
            for i, error in enumerate(results['errors_list'][:10], 1):
                test_name = error['test'].split('(')[0].replace('test_', '').replace('_', ' ')
                print(f"\n  {i}. {test_name}")
                print(f"     Test: {error['test']}")
                # Print error type
                lines = error['traceback'].split('\n')
                for line in lines[-3:]:
                    if line.strip():
                        print(f"     {line.strip()}")

    # Critical COBOL Behaviors
    print("\n" + "─" * 80)
    print("CRITICAL COBOL BEHAVIORS VERIFICATION")
    print("─" * 80)

    critical_behaviors = [
        ("20-character space-padded strings", "String Padding"),
        ("100 user limit enforcement", "User Registration"),
        ("50 session limit enforcement", "User Login"),
        ("Token range 100000-999999", "Token Generation"),
        ("Password change bug (COBOL line 210)", "Password Change"),
        ("Case-sensitive username comparison", "User Registration"),
        ("Plaintext password storage", "Password Change"),
        ("No session expiration", "User Login"),
        ("Sequential search (no optimization)", "Integration Tests"),
        ("Exact error message text", "Error Messages"),
    ]

    for behavior, category in critical_behaviors:
        # Check if category has failures
        has_failure = any(
            categories.get(category, {}).get('class', '') in f['test']
            for f in results['failures'] + results['errors_list']
        )
        status = "✗ FAIL" if has_failure else "✓ PASS"
        print(f"  {status}  {behavior}")

    # Summary
    print("\n" + "─" * 80)
    print("EVALUATION SUMMARY")
    print("─" * 80)

    if results['success']:
        print("✓ MIGRATION SUCCESSFUL")
        print("  All behavioral tests passed.")
        print("  Python implementation faithfully replicates COBOL behavior.")
        print("  All quirks, bugs, and edge cases preserved.")
    else:
        print("✗ MIGRATION INCOMPLETE")
        print(f"  {failed + errors} test(s) failed.")
        print("  Python implementation does not fully match COBOL behavior.")
        print("  Review failed tests and behavioral_spec.md for requirements.")

    print("\n" + "=" * 80)

    # Return exit code
    return 0 if results['success'] else 1


def main():
    """
    Main evaluation entry point

    Returns:
        int: Exit code (0 for all pass, 1 for failures)
    """
    print("Starting COBOL to Python migration evaluation...")

    try:
        # Run evaluation
        results = run_evaluation()

        # Print report
        exit_code = print_evaluation_report(results)

        return exit_code

    except Exception as e:
        print(f"\nFATAL ERROR during evaluation: {e}")
        import traceback
        traceback.print_exc()
        return 2


if __name__ == "__main__":
    sys.exit(main())
