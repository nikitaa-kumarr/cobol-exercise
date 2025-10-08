# Behavioral Specification: User Management System

## Overview
This document specifies the exact behavior of the legacy COBOL User Management System (USERMGMT). Any migration to another language MUST preserve all behaviors, edge cases, and limitations documented here.

---

## 1. System Initialization

### 1.1 Startup Sequence
**Operation**: System initialization before user interaction

**Behavior**:
1. Set `USER-COUNT` to 0
2. Set `SESSION-COUNT` to 0
3. Initialize all 100 user slots:
   - Username: filled with spaces (20 characters)
   - Password: filled with spaces (20 characters)
   - Active flag: 0 (inactive)
4. Initialize all 50 session slots:
   - Token: 0
   - Username: filled with spaces (20 characters)
   - Active flag: 0 (inactive)
5. Display welcome banner
6. Enter main menu loop

**Expected Behavior**:
- All data structures must be in-memory only (no persistence)
- Data is lost on program termination
- Initialization happens once per program execution

---

## 2. User Registration

### 2.1 Registration Flow
**Operation**: Register new user with username and password

**Input**:
- Username (up to 20 characters)
- Password (up to 20 characters)

**Behavior**:
1. Prompt for username
2. Accept username into temporary variable
3. Prompt for password
4. Accept password into temporary variable
5. Check for duplicate username:
   - Loop through indices 1 to `USER-COUNT` (not full array)
   - Compare each `USER-NAME[i]` with input username
   - If match found, set found flag and exit loop early
6. If duplicate found:
   - Display: "ERROR: USERNAME ALREADY EXISTS!"
   - Return to main menu
7. If not duplicate:
   - Check if `USER-COUNT < 100`
   - If yes:
     - Increment `USER-COUNT`
     - Set `USER-IDX` to `USER-COUNT`
     - Store username at `USER-NAME[USER-IDX]`
     - Store password at `USER-PASSWORD[USER-IDX]`
     - Set `USER-ACTIVE[USER-IDX]` to 1
     - Display: "SUCCESS: USER REGISTERED!"
   - If no (database full):
     - Display: "ERROR: USER DATABASE FULL!"

### 2.2 Edge Cases & Requirements

**String Handling**:
- Usernames/passwords are right-padded with spaces to exactly 20 characters
- Example: "admin" becomes "admin               " (15 trailing spaces)
- Comparison is exact character-by-character (space-sensitive)

**Case Sensitivity**:
- Usernames are case-sensitive
- "Admin", "admin", "ADMIN" are three different users

**Input Length**:
- Inputs longer than 20 characters are truncated
- Example: "verylongusernamehere123" becomes "verylongusernamehere"

**Duplicate Detection**:
- Only searches active user count, not entire 100-slot array
- Username "user1" cannot be registered twice
- Duplicate check happens before capacity check

**Capacity Limits**:
- Hard limit: 100 users maximum
- Counter can hold 999 but array only has 100 slots
- Registration fails at 101st attempt

**No Validation**:
- Empty usernames allowed (all spaces)
- Empty passwords allowed (all spaces)
- No special character restrictions
- No minimum/maximum length requirements
- No uniqueness requirement for passwords

---

## 3. User Login

### 3.1 Login Flow
**Operation**: Authenticate user and create session with token

**Input**:
- Username (up to 20 characters)
- Password (up to 20 characters)

**Behavior**:
1. Prompt for username
2. Accept username
3. Prompt for password
4. Accept password
5. Verify credentials:
   - Loop through indices 1 to `USER-COUNT`
   - For each user:
     - Check if `USER-NAME[i]` equals input username
     - AND `USER-PASSWORD[i]` equals input password
     - AND `USER-ACTIVE[i]` equals 1
   - If all conditions true, set found flag and exit loop
6. If credentials invalid:
   - Display: "ERROR: INVALID CREDENTIALS!"
   - Return to main menu
7. If credentials valid:
   - Generate random 6-digit token (call GENERATE-TOKEN)
   - Check if `SESSION-COUNT < 50`
   - If yes:
     - Increment `SESSION-COUNT`
     - Set `SESS-IDX` to `SESSION-COUNT`
     - Store token at `SESSION-TOKEN[SESS-IDX]`
     - Store username at `SESSION-USER[SESS-IDX]`
     - Set `SESSION-ACTIVE[SESS-IDX]` to 1
     - Display: "SUCCESS: LOGIN APPROVED"
     - Display: "YOUR SESSION TOKEN: [6-digit number]"
   - If no (session table full):
     - Display: "ERROR: SESSION TABLE FULL!"
     - User authenticated but no session created

### 3.2 Edge Cases & Requirements

**Authentication Logic**:
- All three conditions must be true (username AND password AND active)
- Inactive users (USER-ACTIVE = 0) cannot log in
- Early loop exit on first match (sequential search)

**Password Comparison**:
- Exact match required (case-sensitive, space-sensitive)
- "pass" != "pass                " (padding matters)

**Session Token Generation**:
- Uses pseudo-random number generator
- Range: 100000 to 999999 (6 digits)
- Formula: `RANDOM() * 900000 + 100000`
- No collision detection
- Same token can be generated multiple times
- Token displayed to user for later use

**Session Limits**:
- Maximum 50 concurrent sessions
- No session expiration mechanism
- Sessions persist until program termination
- Session count never decreases (no logout function)

**Multiple Logins**:
- Same user can log in multiple times (creates multiple sessions)
- Each login generates a new token
- No check for existing user sessions

**Security Issues** (must be preserved in migration):
- Passwords stored in plaintext
- No account lockout after failed attempts
- Timing attack vulnerability (early exit on username match)
- Predictable token generation

---

## 4. Password Change

### 4.1 Password Change Flow
**Operation**: Change user password using session token

**Input**:
- Session token (6-digit number)
- Old password (up to 20 characters)
- New password (up to 20 characters)

**Behavior**:
1. Prompt for session token
2. Accept token (numeric only)
3. Prompt for old password
4. Accept old password
5. Prompt for new password
6. Accept new password
7. Validate session token:
   - Reset found flag to 0
   - Clear temporary username variable (fill with spaces)
   - Loop through indices 1 to `SESSION-COUNT`
   - For each session:
     - Check if `SESSION-TOKEN[i]` equals input token
     - AND `SESSION-ACTIVE[i]` equals 1
   - If both true:
     - Copy `SESSION-USER[i]` to temporary username variable
     - Set found flag to 1
     - Exit loop
8. If token invalid:
   - Display: "ERROR: INVALID SESSION TOKEN!"
   - Return to main menu
9. If token valid:
   - Reset success flag to 0
   - Loop through indices 1 to `USER-COUNT`
   - For each user:
     - Check if `USER-NAME[i]` equals temporary username
     - If yes:
       - Check if `USER-PASSWORD[i]` equals old password
       - If yes:
         - Set `USER-PASSWORD[i]` to new password
         - Set success flag to 1
         - Display: "SUCCESS: PASSWORD CHANGED!"
       - If no:
         - Display: "ERROR: OLD PASSWORD INCORRECT!"
         - Set success flag to 1 (BUG!)
   - Exit loop when success flag = 1

### 4.2 Edge Cases & Requirements

**Session Token Validation**:
- Only active sessions (SESSION-ACTIVE = 1) are valid
- Inactive sessions rejected
- Token must be exact numeric match
- Token 000123 != 123 (leading zeros matter if stored)

**Two-Phase Validation**:
1. First: Validate session token → get username
2. Second: Validate old password → change password

**Critical Bug** (must be preserved):
- Line 210: Sets success flag to 1 even when old password is incorrect
- Effect: Loop exits after first username match, regardless of password correctness
- If user table has duplicate usernames (shouldn't happen), only first is checked

**Password Update**:
- New password replaces old password in-place
- No password history or restrictions
- Can change to same password as old password
- No confirmation of new password

**No Username Validation**:
- Assumes session username exists in user table
- If session username deleted/corrupted, no error handling

**Token Reuse**:
- Same token can exist multiple times in session table
- First matching active token is used
- Other sessions with same token ignored

---

## 5. Token Generation

### 5.1 GENERATE-TOKEN Procedure
**Operation**: Generate pseudo-random 6-digit session token

**Behavior**:
1. Call `FUNCTION RANDOM` (returns 0.0 to 1.0)
2. Multiply result by 900000
3. Add 100000 to result
4. Store in `WS-RANDOM-NUM` (6-digit numeric field)

**Output Range**:
- Minimum: 100000
- Maximum: 999999 (theoretical)
- Actual maximum: 999999 (depending on RANDOM implementation)

### 5.2 Edge Cases & Requirements

**Random Seed**:
- COBOL `FUNCTION RANDOM` may not be seeded
- Could produce same sequence each program run
- Platform-dependent behavior (IBM-370 specified)

**Not Cryptographically Secure**:
- Predictable sequence
- Linear congruential generator (typical COBOL implementation)
- Vulnerable to token prediction

**No Collision Detection**:
- Same token can be generated multiple times
- No uniqueness check against existing session tokens
- Tokens can collide within same session table

**Precision**:
- `RANDOM() * 900000` may have rounding behavior
- Integer truncation to 6-digit field
- Implementation-specific rounding rules

---

## 6. Main Menu System

### 6.1 Menu Loop
**Operation**: Display menu and route user choices

**Menu Options**:
```
MAIN MENU:
1. REGISTER NEW USER
2. LOGIN
3. CHANGE PASSWORD
4. EXIT
```

**Behavior**:
1. Display blank line
2. Display menu options
3. Display prompt: "ENTER CHOICE (1-4): " (no newline)
4. Accept single digit input
5. Evaluate choice:
   - 1: Call REGISTER-USER
   - 2: Call LOGIN-USER
   - 3: Call CHANGE-PASSWORD
   - 4: Display "SYSTEM SHUTDOWN..." and exit loop
   - Other: Display "INVALID CHOICE. TRY AGAIN."
6. Loop continues until choice = 4
7. After loop exits, program terminates (STOP RUN)

### 6.2 Edge Cases & Requirements

**Input Handling**:
- Accepts single digit only (PIC 9)
- Input "12" truncated to "1"
- Non-numeric input causes runtime error or reads as 0
- Input 0 triggers "INVALID CHOICE"

**No Logout Function**:
- No option to invalidate session tokens
- Sessions persist until program exit

**Loop Behavior**:
- Menu redisplays after each operation completes
- Exit is only way to terminate program
- No timeout or inactivity logout

---

## 7. Data Structure Specifications

### 7.1 User Table
**Structure**: Fixed-size array of 100 elements (1-indexed)

**Fields per Entry**:
- `USER-NAME`: 20 characters (alphanumeric)
- `USER-PASSWORD`: 20 characters (alphanumeric)
- `USER-ACTIVE`: 1 digit (0 or 1)

**Index**: `USER-IDX` (1 to 100)

**Counter**: `USER-COUNT` (0 to 100)

**Initialization**:
- All usernames: 20 spaces
- All passwords: 20 spaces
- All active flags: 0

### 7.2 Session Table
**Structure**: Fixed-size array of 50 elements (1-indexed)

**Fields per Entry**:
- `SESSION-TOKEN`: 6 digits (numeric, 0 to 999999)
- `SESSION-USER`: 20 characters (alphanumeric)
- `SESSION-ACTIVE`: 1 digit (0 or 1)

**Index**: `SESS-IDX` (1 to 50)

**Counter**: `SESSION-COUNT` (0 to 50)

**Initialization**:
- All tokens: 0
- All usernames: 20 spaces
- All active flags: 0

### 7.3 Indexing Convention
**Critical**: COBOL uses 1-based indexing

- Arrays start at index 1, not 0
- Loop: `FROM 1 BY 1 UNTIL index > count`
- Migrating to 0-based language requires offset adjustment

---

## 8. String Comparison Rules

### 8.1 Space Padding Behavior
**All strings are fixed-width, space-padded**

Examples:
- Input "admin" → Stored as "admin               " (20 chars)
- Input "a" → Stored as "a                   " (20 chars)
- Input "" (empty) → Stored as "                    " (20 spaces)

### 8.2 Comparison Rules
**Exact character-by-character comparison**

- "admin" vs "admin               " → NOT EQUAL (different lengths)
- "User" vs "user" → NOT EQUAL (case differs)
- "pass " vs "pass" → Depends on padding applied to both sides

**Migration Requirement**:
- Must pad all input to 20 characters before comparison
- Must pad all stored values to 20 characters
- Comparison must be case-sensitive
- Trailing spaces are significant

---

## 9. Error Messages

### 9.1 Complete Error Message List

**Registration**:
- `ERROR: USERNAME ALREADY EXISTS!` - Duplicate username
- `ERROR: USER DATABASE FULL!` - 100 user limit reached

**Login**:
- `ERROR: INVALID CREDENTIALS!` - Wrong username/password or inactive user
- `ERROR: SESSION TABLE FULL!` - 50 session limit reached (after valid login)

**Password Change**:
- `ERROR: INVALID SESSION TOKEN!` - Token not found or session inactive
- `ERROR: OLD PASSWORD INCORRECT!` - Old password doesn't match

**Menu**:
- `INVALID CHOICE. TRY AGAIN.` - Choice not 1-4

**Success Messages**:
- `SUCCESS: USER REGISTERED!`
- `SUCCESS: LOGIN APPROVED`
- `YOUR SESSION TOKEN: [6-digit number]`
- `SUCCESS: PASSWORD CHANGED!`

**System Messages**:
- `SYSTEM SHUTDOWN...` - Exit selected

### 9.2 Message Format Requirements
- All messages displayed on new line (unless "WITH NO ADVANCING")
- Exact wording must match
- Upper case conventions
- Exclamation marks included

---

## 10. Critical Bugs & Quirks (Must Preserve)

### 10.1 Password Change Bug
**Location**: Change-Password procedure, line 210

**Bug**: Sets success flag to 1 even when old password is incorrect

**Effect**:
- Search loop exits after first username match
- If duplicate usernames exist (shouldn't but possible with bugs), only first checked
- No retry or continued search

**Preservation**: Migration must replicate this exact bug

### 10.2 Session Table Full After Login
**Scenario**: User enters valid credentials, but session table is full

**Behavior**:
- Credentials validated successfully
- Token generated
- Session cannot be created
- Error displayed: "ERROR: SESSION TABLE FULL!"
- User must re-login when session slot available

**Preservation**: Must validate credentials before checking session capacity

### 10.3 No Session Cleanup
**Bug**: Sessions never expire or get invalidated

**Effect**:
- `SESSION-COUNT` only increments, never decrements
- After 50 logins, no more sessions possible until program restart
- No logout mechanism

**Preservation**: Must not add session cleanup unless specified

### 10.4 Token Collision Vulnerability
**Bug**: No uniqueness check on generated tokens

**Effect**:
- Two sessions can have identical tokens
- Password change uses first matching token
- Could change wrong user's password

**Preservation**: Must not add collision detection

### 10.5 Plaintext Password Storage
**Security Issue**: Passwords stored without encryption

**Preservation**: Must store passwords in plaintext (unless security upgrade specified)

### 10.6 Non-Seeded Random
**Issue**: Random number generator may not be seeded

**Effect**: Same token sequence each program run

**Preservation**: Use similar unseeded or platform-default seeding

---

## 11. Performance Characteristics

### 11.1 Search Algorithms
**All searches are sequential (linear)**

- User lookup: O(n) where n = USER-COUNT
- Session lookup: O(n) where n = SESSION-COUNT
- Early exit on first match (best case O(1), worst case O(n))

**Preservation**: Do not optimize to hash tables or binary search unless specified

### 11.2 Memory Usage
**Fixed allocation at startup**

- User table: 100 × (20 + 20 + 1) = 4,100 bytes
- Session table: 50 × (6 + 20 + 1) = 1,350 bytes
- Total: ~5.5 KB (plus working variables)

**No Dynamic Allocation**: Arrays never resize

---

## 12. Operational Constraints

### 12.1 Persistence
**None**: All data in-memory only

- No file I/O
- No database connections
- Data lost on program termination

### 12.2 Concurrency
**None**: Single-user, single-threaded

- No locking mechanisms
- No thread safety
- Designed for single mainframe terminal session

### 12.3 Input Validation
**Minimal**: Only data type enforcement

- Numeric fields reject non-numeric input (runtime error)
- No range checks
- No format validation
- No sanitization

---

## 13. Migration Checklist

### 13.1 Must Preserve
✅ 1-based indexing behavior (or convert correctly to 0-based)
✅ 20-character space-padded strings
✅ Case-sensitive comparisons
✅ Fixed array sizes (100 users, 50 sessions)
✅ Sequential search algorithms
✅ In-memory only (no persistence)
✅ Exact error messages
✅ Password change bug (line 210)
✅ No session expiration
✅ Token generation range (100000-999999)
✅ Plaintext password storage
✅ No collision detection
✅ Session-after-validation bug

### 13.2 Must Test
- Registration with duplicate usernames
- Registration at 100-user limit
- Login with inactive users
- Login at 50-session limit
- Password change with invalid token
- Password change with wrong old password
- Menu input validation
- String padding behavior
- Token generation range
- Empty username/password handling

### 13.3 Must Document
- All edge cases from this specification
- Security vulnerabilities (plaintext passwords, predictable tokens)
- Capacity limits (100/50)
- Bug preservation rationale
- Migration-specific changes (if any)

---

## 14. Testing Requirements

### 14.1 Functional Tests

**Registration**:
1. Register first user successfully
2. Register duplicate username (expect error)
3. Register 100 users (expect success)
4. Register 101st user (expect database full error)
5. Register with empty username
6. Register with empty password
7. Register with 20-character username
8. Register with 21-character username (truncation test)

**Login**:
1. Login with valid credentials
2. Login with wrong password
3. Login with wrong username
4. Login with inactive user (USER-ACTIVE = 0)
5. Login 50 times successfully
6. Login 51st time (expect session full error)
7. Login same user multiple times
8. Verify token displayed is 6 digits
9. Verify token is in range 100000-999999

**Password Change**:
1. Change password with valid token and old password
2. Change with invalid token
3. Change with wrong old password
4. Change with inactive session
5. Change to same password as old
6. Verify password updated in user table
7. Verify old token still valid after password change

**Menu**:
1. Select each option (1-4)
2. Select 0 (invalid)
3. Select 5-9 (invalid)
4. Exit program (option 4)

### 14.2 Edge Case Tests

**String Handling**:
1. Compare "user" vs "user                " (padding)
2. Compare "User" vs "user" (case sensitivity)
3. Store and retrieve 20-character strings
4. Store and retrieve 1-character strings

**Boundary Tests**:
1. Exactly 100 users
2. Exactly 50 sessions
3. Token = 100000 (minimum)
4. Token = 999999 (maximum)

**Bug Verification**:
1. Verify password change bug (line 210 behavior)
2. Verify no session cleanup
3. Verify token collision possible

---

## 15. Non-Functional Requirements

### 15.1 Compatibility
- Must behave identically to COBOL version
- Platform differences acceptable only if documented

### 15.2 Performance
- No performance improvements unless behavior-preserving
- Maintain O(n) search complexity

### 15.3 Security
- Preserve insecure behaviors (for compatibility)
- Document all security issues
- Optional: Add secure mode with documented differences

---

## Appendix A: Example Scenarios

### Scenario 1: Complete User Journey
1. Start program → Welcome banner displays
2. Select 1 (Register) → Enter "alice" / "pass123" → Success
3. Select 2 (Login) → Enter "alice" / "pass123" → Token: 456789
4. Select 3 (Change Password) → Enter 456789 / "pass123" / "newpass" → Success
5. Select 4 (Exit) → Program terminates
6. Restart program → Alice's account no longer exists (no persistence)

### Scenario 2: Edge Case - Session Full
1. Register 1 user
2. Login 50 times (creates 50 sessions)
3. Login 51st time → "ERROR: SESSION TABLE FULL!" (but credentials validated)
4. No way to free sessions except program restart

### Scenario 3: Edge Case - Database Full
1. Register 100 users
2. All succeed (user1 through user100)
3. Register 101st user → "ERROR: USER DATABASE FULL!"
4. No way to delete users

### Scenario 4: Bug Demonstration
1. Register "testuser" / "oldpass"
2. Login → Token: 123456
3. Change password: 123456 / "wrongpass" / "newpass"
4. Error: "ERROR: OLD PASSWORD INCORRECT!"
5. Success flag set to 1, loop exits (bug)
6. Password not changed, but search terminated

---

**Document Version**: 1.0
**Created**: 2025-10-08
**Purpose**: Behavioral specification for USERMGMT COBOL legacy system migration
**Status**: Complete
