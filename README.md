# COBOL to Python Migration Project

## User Management System - Legacy Modernization

This project demonstrates a **faithful migration** of a legacy COBOL mainframe application to modern Python, preserving all original behaviors, quirks, and bugs for compatibility testing and educational purposes.

---

## 📋 Project Overview

### Original System
- **Program**: USERMGMT (User Management System)
- **Language**: COBOL (IBM-370)
- **Date**: 1987-03-15
- **Author**: LEGACY-SYSTEMS-DEPT
- **Purpose**: Mainframe user registration, authentication, and password management

### Migration Goals
✅ **Preserve all COBOL behaviors** - Including bugs and quirks
✅ **Maintain data structure constraints** - Fixed arrays, string padding
✅ **Replicate exact error messages** - Character-for-character match
✅ **Keep security vulnerabilities** - For compatibility testing
✅ **Document all edge cases** - Comprehensive behavioral specification

---

## 📁 Project Structure

```
cobol-screening/
├── legacy_app.cob           # Original COBOL source code
├── modern_app.py            # Python migration (faithful conversion)
├── behavioral_spec.md       # Complete behavioral specification
├── behavioral_tests.py      # Comprehensive test suite (43 tests)
├── evaluate_refactor.py     # Grading and evaluation system
├── run_evaluation.py        # Automated test execution script
└── README.md               # This file
```

---

## 🎯 Key Features

### Functionality
- **User Registration** - Create accounts with username/password
- **User Login** - Authenticate and receive session tokens
- **Password Change** - Update password using session token
- **Session Management** - Token-based session tracking

### COBOL Behaviors Preserved
- ✓ **20-character space-padded strings** (COBOL `PIC X(20)`)
- ✓ **Fixed array sizes** (100 users, 50 sessions)
- ✓ **1-based to 0-based indexing conversion**
- ✓ **Sequential search algorithms** (O(n) complexity)
- ✓ **6-digit token generation** (100000-999999)
- ✓ **Case-sensitive comparisons**
- ✓ **In-memory only** (no persistence)

### Bugs Preserved (For Compatibility)
- 🐛 **Password change bug** (COBOL line 210) - Success flag set incorrectly
- 🐛 **No session expiration** - Sessions never cleaned up
- 🐛 **Token collision vulnerability** - No uniqueness check
- 🐛 **Plaintext password storage** - No encryption
- 🐛 **Predictable token generation** - Unseeded random

---

## 🔧 Prerequisites

### Required
- **Python 3.6 or higher**
- **Standard Library Only** - No external dependencies

### Optional (for COBOL reference)
- COBOL compiler (GnuCOBOL, IBM COBOL) - to run `legacy_app.cob`

### Verify Python Installation
```bash
python --version
# or
python3 --version
```

---

## 🚀 Quick Start

### 1. Run the Python Application
```bash
python modern_app.py
```

**Interactive Menu:**
```
========================================
   USER MANAGEMENT SYSTEM v1.0
   LEGACY MAINFRAME APPLICATION
========================================

MAIN MENU:
1. REGISTER NEW USER
2. LOGIN
3. CHANGE PASSWORD
4. EXIT
ENTER CHOICE (1-4):
```

### 2. Run the Test Suite
```bash
python run_evaluation.py
```

This will:
- Execute all 43 behavioral tests
- Generate a comprehensive grading report
- Display pass/fail status by feature
- Verify COBOL behavior compliance
- Return exit code (0 = all pass, 1 = failures)

---

## 📊 Expected Test Output

### Successful Evaluation (100% Pass)

```
================================================================================
                    COBOL TO PYTHON MIGRATION EVALUATION
================================================================================
Evaluation Time: 2025-10-08 14:30:15
Test Suite: behavioral_tests.py
System Under Test: modern_app.py
Original COBOL: legacy_app.cob (USERMGMT, 1987-03-15)
================================================================================

────────────────────────────────────────────────────────────────────────────────
OVERALL SCORE
────────────────────────────────────────────────────────────────────────────────
Score: 100.0% (EXCELLENT) - Grade: A+
Tests Passed: 43/43
Tests Failed: 0/43
Tests Errors: 0/43

────────────────────────────────────────────────────────────────────────────────
FEATURE COMPLIANCE BREAKDOWN
────────────────────────────────────────────────────────────────────────────────

User Registration: ✓ PASS
  Passed: 8/8
  COBOL Behaviors Tested:
    • Register user successfully
    • Reject duplicate username
    • Reject when DB full (100 users)
    • Empty username/password allowed
    • 20-char truncation
    • Case-sensitive usernames

User Login: ✓ PASS
  Passed: 8/8
  COBOL Behaviors Tested:
    • Login with valid credentials
    • Reject invalid credentials
    • Reject inactive users
    • Generate valid 6-digit token (100000-999999)
    • Token maps to correct user
    • Multiple sessions same user
    • Reject when session table full (50 sessions)

Password Change: ✓ PASS
  Passed: 8/8
  COBOL Behaviors Tested:
    • Change password with valid token + old password
    • Reject invalid token
    • Reject wrong old password
    • CRITICAL BUG: Line 210 flag behavior
    • Token remains valid after change
    • Token collision uses first match

String Padding (COBOL PIC X(20)): ✓ PASS
  Passed: 3/3
  COBOL Behaviors Tested:
    • 20-character space padding
    • Truncation at 20 chars
    • Space-sensitive comparison

Menu System: ✓ PASS
  Passed: 5/5
  COBOL Behaviors Tested:
    • Menu choices 1-4
    • Invalid choice handling
    • Input truncation (PIC 9)

Token Generation: ✓ PASS
  Passed: 3/3
  COBOL Behaviors Tested:
    • Range: 100000-999999
    • Integer values
    • Pseudo-random (not cryptographic)

Data Structures: ✓ PASS
  Passed: 3/3
  COBOL Behaviors Tested:
    • 100 user slots initialized
    • 50 session slots initialized
    • Counters start at 0

Integration Tests: ✓ PASS
  Passed: 3/3
  COBOL Behaviors Tested:
    • Complete user journey
    • Session table full scenario
    • Database full scenario

Error Messages: ✓ PASS
  Passed: 2/2
  COBOL Behaviors Tested:
    • Exact COBOL error message text
    • Exact COBOL success message text

────────────────────────────────────────────────────────────────────────────────
CRITICAL COBOL BEHAVIORS VERIFICATION
────────────────────────────────────────────────────────────────────────────────
  ✓ PASS  20-character space-padded strings
  ✓ PASS  100 user limit enforcement
  ✓ PASS  50 session limit enforcement
  ✓ PASS  Token range 100000-999999
  ✓ PASS  Password change bug (COBOL line 210)
  ✓ PASS  Case-sensitive username comparison
  ✓ PASS  Plaintext password storage
  ✓ PASS  No session expiration
  ✓ PASS  Sequential search (no optimization)
  ✓ PASS  Exact error message text

────────────────────────────────────────────────────────────────────────────────
EVALUATION SUMMARY
────────────────────────────────────────────────────────────────────────────────
✓ MIGRATION SUCCESSFUL
  All behavioral tests passed.
  Python implementation faithfully replicates COBOL behavior.
  All quirks, bugs, and edge cases preserved.

================================================================================

================================================================================
EVALUATION COMPLETE
================================================================================
Result: ✓ ALL TESTS PASSED
The Python migration successfully replicates all COBOL behaviors.
================================================================================
```

### Failed Test Example

If tests fail, you'll see detailed output:

```
────────────────────────────────────────────────────────────────────────────────
FAILED TESTS DETAIL
────────────────────────────────────────────────────────────────────────────────

1 Test Failure(s):

  1. reject duplicate username
     Test: test_reject_duplicate_username (behavioral_tests.TestUserRegistration)
     AssertionError: 'ERROR: USERNAME ALREADY EXISTS!' not found in output

────────────────────────────────────────────────────────────────────────────────
EVALUATION SUMMARY
────────────────────────────────────────────────────────────────────────────────
✗ MIGRATION INCOMPLETE
  1 test(s) failed.
  Python implementation does not fully match COBOL behavior.
  Review failed tests and behavioral_spec.md for requirements.
```

---

## 🧪 Test Categories

### 1. User Registration Tests (8 tests)
- ✓ Register user successfully
- ✓ Reject duplicate username
- ✓ Reject when database full (100 users)
- ✓ Empty username/password allowed
- ✓ 20-character username handling
- ✓ 21-character username truncation
- ✓ Case sensitivity

### 2. User Login Tests (8 tests)
- ✓ Login with valid credentials
- ✓ Reject invalid username
- ✓ Reject invalid password
- ✓ Reject inactive users
- ✓ Generate valid 6-digit token
- ✓ Token maps to correct user
- ✓ Multiple sessions for same user
- ✓ Reject when session table full (50 sessions)

### 3. Password Change Tests (8 tests)
- ✓ Change password with valid token
- ✓ Reject invalid token
- ✓ Reject wrong old password
- ✓ Verify COBOL line 210 bug
- ✓ Reject inactive session
- ✓ Allow changing to same password
- ✓ Token remains valid after change
- ✓ Token collision handling

### 4. String Padding Tests (3 tests)
- ✓ 20-character padding function
- ✓ Username comparison with padding
- ✓ Space-sensitive comparisons

### 5. Menu System Tests (5 tests)
- ✓ Menu choices 1-4
- ✓ Invalid choice handling
- ✓ Input truncation (PIC 9)

### 6. Token Generation Tests (3 tests)
- ✓ Token range validation (100000-999999)
- ✓ Integer token values
- ✓ Token variance

### 7. Data Structure Tests (3 tests)
- ✓ User table initialization (100 slots)
- ✓ Session table initialization (50 slots)
- ✓ Counter initialization

### 8. Integration Tests (3 tests)
- ✓ Complete user journey
- ✓ Session table full scenario
- ✓ Database full scenario

### 9. Error Message Tests (2 tests)
- ✓ Exact error message text
- ✓ Exact success message text

**Total: 43 comprehensive tests**

---

## 📖 Documentation

### `behavioral_spec.md`
Complete behavioral specification including:
- All operations and workflows
- Edge cases and boundary conditions
- Bug documentation with preservation rationale
- String padding and comparison rules
- Error message catalog
- Testing requirements
- Migration checklist

### Code Comments
Both `legacy_app.cob` and `modern_app.py` include:
- Line-by-line correspondence
- COBOL procedure names in comments
- Bug preservation notes
- Data structure documentation

---

## 🔍 Key Differences: COBOL vs Python

| Aspect | COBOL | Python |
|--------|-------|--------|
| **Indexing** | 1-based (1 to 100) | 0-based (0 to 99) |
| **Strings** | Fixed-length `PIC X(20)` | Dynamic, padded to 20 |
| **Arrays** | `OCCURS 100 TIMES` | `list` with 100 elements |
| **Loops** | `PERFORM VARYING` | `for range()` with break |
| **Switch** | `EVALUATE WHEN` | `if/elif/else` |
| **Random** | `FUNCTION RANDOM` | `random.random()` |
| **I/O** | `ACCEPT/DISPLAY` | `input()/print()` |

---

## 🛠️ Development Commands

### Run Individual Test Categories
```bash
# Run just registration tests
python -m unittest behavioral_tests.TestUserRegistration

# Run just login tests
python -m unittest behavioral_tests.TestUserLogin

# Run with verbose output
python -m unittest behavioral_tests -v
```

### Run Specific Test
```bash
python -m unittest behavioral_tests.TestUserRegistration.test_register_user_successfully
```

### View Test Coverage
```bash
# Manual test count
python -c "import behavioral_tests, unittest; \
  suite = unittest.defaultTestLoader.loadTestsFromModule(behavioral_tests); \
  print(f'Total tests: {suite.countTestCases()}')"
```

---

## ⚠️ Known Limitations (By Design)

These are **intentionally preserved** from the COBOL original:

### Security Issues
- 🔓 Passwords stored in plaintext
- 🔓 Predictable session tokens
- 🔓 No account lockout mechanism
- 🔓 Timing attack vulnerabilities

### Functional Limitations
- 💾 No data persistence (in-memory only)
- 🔢 Hard limits: 100 users, 50 sessions
- 🚫 No session expiration or logout
- 🐌 Sequential search (O(n) complexity)

### Bugs
- 🐛 Password change sets success flag incorrectly
- 🐛 Token collision possible
- 🐛 Session table fills permanently

**Note**: These are documented in `behavioral_spec.md` and tested in `behavioral_tests.py`.

---

## 📝 Example Usage Session

```bash
$ python modern_app.py

========================================
   USER MANAGEMENT SYSTEM v1.0
   LEGACY MAINFRAME APPLICATION
========================================

MAIN MENU:
1. REGISTER NEW USER
2. LOGIN
3. CHANGE PASSWORD
4. EXIT
ENTER CHOICE (1-4): 1

--- USER REGISTRATION ---
ENTER USERNAME: alice
ENTER PASSWORD: secret123
SUCCESS: USER REGISTERED!

MAIN MENU:
1. REGISTER NEW USER
2. LOGIN
3. CHANGE PASSWORD
4. EXIT
ENTER CHOICE (1-4): 2

--- USER LOGIN ---
ENTER USERNAME: alice
ENTER PASSWORD: secret123
SUCCESS: LOGIN APPROVED
YOUR SESSION TOKEN: 456789

MAIN MENU:
1. REGISTER NEW USER
2. LOGIN
3. CHANGE PASSWORD
4. EXIT
ENTER CHOICE (1-4): 3

--- CHANGE PASSWORD ---
ENTER SESSION TOKEN: 456789
ENTER OLD PASSWORD: secret123
ENTER NEW PASSWORD: newsecret
SUCCESS: PASSWORD CHANGED!

MAIN MENU:
1. REGISTER NEW USER
2. LOGIN
3. CHANGE PASSWORD
4. EXIT
ENTER CHOICE (1-4): 4
SYSTEM SHUTDOWN...
```

---

## 🤝 Contributing

This is an educational/demonstration project showing faithful COBOL-to-Python migration.

### If you find discrepancies:
1. Check `behavioral_spec.md` for expected behavior
2. Run `run_evaluation.py` to identify failures
3. Compare with COBOL source in `legacy_app.cob`
4. Ensure bug preservation is intentional (see spec section 10)

---

## 📜 License

Educational/Demonstration Project

**Original COBOL Program:**
- Author: LEGACY-SYSTEMS-DEPT
- Date: 1987-03-15

**Python Migration:**
- Date: 2025-10-08
- Purpose: Legacy system modernization demonstration

---

## 📚 Additional Resources

### Understanding COBOL Data Types
- `PIC X(n)` - Alphanumeric field, n characters
- `PIC 9(n)` - Numeric field, n digits
- `OCCURS n TIMES` - Array with n elements
- `VALUE` - Default initialization value

### COBOL to Python Mapping
- `PERFORM procedure` → `function()`
- `MOVE value TO variable` → `variable = value`
- `ACCEPT variable` → `variable = input()`
- `DISPLAY text` → `print(text)`
- `IF condition` → `if condition:`
- `EVALUATE` → `if/elif/else`

---

## 🎓 Learning Objectives

This project demonstrates:
- ✅ Legacy system analysis and documentation
- ✅ Behavioral specification creation
- ✅ Test-driven migration approach
- ✅ Bug preservation for compatibility
- ✅ Edge case handling
- ✅ COBOL data structure translation
- ✅ Comprehensive testing strategies

---

## 📞 Support

For questions about:
- **COBOL behavior**: See `behavioral_spec.md`
- **Test failures**: Run with `-v` flag for details
- **Migration approach**: Review code comments in `modern_app.py`

---

**Project Status**: ✅ Complete - All 43 tests passing

*Last Updated: 2025-10-08*
