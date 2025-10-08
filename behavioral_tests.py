#!/usr/bin/env python3
"""
Behavioral Test Suite for User Management System
Tests Python migration (modern_app.py) against COBOL spec (behavioral_spec.md)

Validates:
- All functional requirements
- Edge cases and boundary conditions
- Bug preservation (COBOL line 210, etc.)
- String padding behavior
- Capacity limits
- Error messages
"""

import unittest
import sys
from io import StringIO
from unittest.mock import patch
import random

# Import the system under test
from modern_app import UserManagementSystem


class TestUserRegistration(unittest.TestCase):
    """Test suite for user registration functionality"""

    def setUp(self):
        """Create fresh system instance for each test"""
        self.system = UserManagementSystem()

    def test_register_user_successfully(self):
        """Test successful user registration"""
        # Simulate registration
        with patch('builtins.input', side_effect=['alice', 'password123']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()

        # Verify success message
        self.assertIn("SUCCESS: USER REGISTERED!", output)

        # Verify user stored correctly
        self.assertEqual(self.system.user_count, 1)
        self.assertEqual(self.system.user_table[0]['username'], 'alice               ')  # 20 chars
        self.assertEqual(self.system.user_table[0]['password'], 'password123         ')  # 20 chars
        self.assertEqual(self.system.user_table[0]['active'], 1)

    def test_reject_duplicate_username(self):
        """Test rejection of duplicate username"""
        # Register first user
        with patch('builtins.input', side_effect=['alice', 'pass1']):
            self.system.register_user()

        # Try to register same username
        with patch('builtins.input', side_effect=['alice', 'pass2']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()

        # Verify error message
        self.assertIn("ERROR: USERNAME ALREADY EXISTS!", output)

        # Verify user count unchanged
        self.assertEqual(self.system.user_count, 1)

        # Verify first password unchanged
        self.assertEqual(self.system.user_table[0]['password'], 'pass1               ')

    def test_reject_when_db_full_100_users(self):
        """Test rejection when user database full (100 users)"""
        # Register 100 users
        for i in range(100):
            username = f'user{i}'
            password = f'pass{i}'
            with patch('builtins.input', side_effect=[username, password]):
                self.system.register_user()

        # Verify 100 users registered
        self.assertEqual(self.system.user_count, 100)

        # Try to register 101st user
        with patch('builtins.input', side_effect=['user101', 'pass101']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()

        # Verify error message
        self.assertIn("ERROR: USER DATABASE FULL!", output)

        # Verify count still 100
        self.assertEqual(self.system.user_count, 100)

    def test_register_empty_username(self):
        """Test registration with empty username (allowed per spec)"""
        with patch('builtins.input', side_effect=['', 'password']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()

        # Should succeed (no validation per COBOL spec)
        self.assertIn("SUCCESS: USER REGISTERED!", output)
        self.assertEqual(self.system.user_count, 1)
        self.assertEqual(self.system.user_table[0]['username'], ' ' * 20)

    def test_register_empty_password(self):
        """Test registration with empty password (allowed per spec)"""
        with patch('builtins.input', side_effect=['testuser', '']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()

        # Should succeed
        self.assertIn("SUCCESS: USER REGISTERED!", output)
        self.assertEqual(self.system.user_table[0]['password'], ' ' * 20)

    def test_register_20_char_username(self):
        """Test registration with exactly 20-character username"""
        username_20 = 'a' * 20
        with patch('builtins.input', side_effect=[username_20, 'password']):
            self.system.register_user()

        self.assertEqual(self.system.user_table[0]['username'], username_20)
        self.assertEqual(len(self.system.user_table[0]['username']), 20)

    def test_register_21_char_username_truncation(self):
        """Test username truncation at 20 characters"""
        username_21 = 'a' * 21
        with patch('builtins.input', side_effect=[username_21, 'password']):
            self.system.register_user()

        # Should be truncated to 20 chars
        self.assertEqual(self.system.user_table[0]['username'], 'a' * 20)
        self.assertEqual(len(self.system.user_table[0]['username']), 20)

    def test_username_case_sensitivity(self):
        """Test that usernames are case-sensitive"""
        # Register 'Admin'
        with patch('builtins.input', side_effect=['Admin', 'pass1']):
            self.system.register_user()

        # Register 'admin' - should succeed (different user)
        with patch('builtins.input', side_effect=['admin', 'pass2']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()

        self.assertIn("SUCCESS: USER REGISTERED!", output)
        self.assertEqual(self.system.user_count, 2)


class TestUserLogin(unittest.TestCase):
    """Test suite for user login functionality"""

    def setUp(self):
        """Create system and register test user"""
        self.system = UserManagementSystem()
        # Register test user
        with patch('builtins.input', side_effect=['testuser', 'testpass']):
            self.system.register_user()

    def test_login_with_valid_credentials(self):
        """Test successful login with valid credentials"""
        with patch('builtins.input', side_effect=['testuser', 'testpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.login_user()
                output = fake_out.getvalue()

        # Verify success messages
        self.assertIn("SUCCESS: LOGIN APPROVED", output)
        self.assertIn("YOUR SESSION TOKEN:", output)

        # Verify session created
        self.assertEqual(self.system.session_count, 1)
        self.assertEqual(self.system.session_table[0]['username'], 'testuser            ')
        self.assertEqual(self.system.session_table[0]['active'], 1)

    def test_reject_invalid_username(self):
        """Test rejection of invalid username"""
        with patch('builtins.input', side_effect=['wronguser', 'testpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.login_user()
                output = fake_out.getvalue()

        self.assertIn("ERROR: INVALID CREDENTIALS!", output)
        self.assertEqual(self.system.session_count, 0)

    def test_reject_invalid_password(self):
        """Test rejection of invalid password"""
        with patch('builtins.input', side_effect=['testuser', 'wrongpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.login_user()
                output = fake_out.getvalue()

        self.assertIn("ERROR: INVALID CREDENTIALS!", output)
        self.assertEqual(self.system.session_count, 0)

    def test_reject_inactive_user(self):
        """Test rejection of inactive user (active flag = 0)"""
        # Set user to inactive
        self.system.user_table[0]['active'] = 0

        with patch('builtins.input', side_effect=['testuser', 'testpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.login_user()
                output = fake_out.getvalue()

        self.assertIn("ERROR: INVALID CREDENTIALS!", output)
        self.assertEqual(self.system.session_count, 0)

    def test_generate_valid_session_token(self):
        """Test that generated session token is in valid range (100000-999999)"""
        with patch('builtins.input', side_effect=['testuser', 'testpass']):
            self.system.login_user()

        token = self.system.session_table[0]['token']

        # Verify token is 6 digits in range 100000-999999
        self.assertGreaterEqual(token, 100000)
        self.assertLessEqual(token, 999999)

    def test_token_maps_to_correct_user(self):
        """Test that session token is correctly mapped to user"""
        with patch('builtins.input', side_effect=['testuser', 'testpass']):
            self.system.login_user()

        # Verify session username matches login username
        self.assertEqual(
            self.system.session_table[0]['username'],
            self.system.user_table[0]['username']
        )

    def test_multiple_sessions_same_user(self):
        """Test that same user can log in multiple times"""
        # Login 3 times with same user
        for i in range(3):
            with patch('builtins.input', side_effect=['testuser', 'testpass']):
                self.system.login_user()

        # Verify 3 sessions created
        self.assertEqual(self.system.session_count, 3)

        # Verify all sessions belong to same user
        for i in range(3):
            self.assertEqual(
                self.system.session_table[i]['username'],
                'testuser            '
            )

    def test_reject_when_session_table_full_50_sessions(self):
        """Test rejection when session table full (50 sessions)"""
        # Create 50 sessions
        for i in range(50):
            with patch('builtins.input', side_effect=['testuser', 'testpass']):
                self.system.login_user()

        # Verify 50 sessions created
        self.assertEqual(self.system.session_count, 50)

        # Try 51st login
        with patch('builtins.input', side_effect=['testuser', 'testpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.login_user()
                output = fake_out.getvalue()

        # Verify error message (credentials validated but session not created)
        self.assertIn("ERROR: SESSION TABLE FULL!", output)

        # Verify session count still 50
        self.assertEqual(self.system.session_count, 50)

    def test_password_padding_matters_for_login(self):
        """Test that password padding affects login (space-sensitive)"""
        # Register user with short password
        with patch('builtins.input', side_effect=['user2', 'pass']):
            self.system.register_user()

        # Try to login with unpadded password (should succeed with padding)
        with patch('builtins.input', side_effect=['user2', 'pass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.login_user()
                output = fake_out.getvalue()

        # Should succeed because both get padded to 20 chars
        self.assertIn("SUCCESS: LOGIN APPROVED", output)


class TestPasswordChange(unittest.TestCase):
    """Test suite for password change functionality"""

    def setUp(self):
        """Create system, register user, and login to get token"""
        self.system = UserManagementSystem()

        # Register user
        with patch('builtins.input', side_effect=['testuser', 'oldpass']):
            self.system.register_user()

        # Login to get session token
        with patch('builtins.input', side_effect=['testuser', 'oldpass']):
            with patch('sys.stdout', new=StringIO()):
                self.system.login_user()

        # Get the token that was generated
        self.token = self.system.session_table[0]['token']

    def test_change_password_with_valid_token_and_old_password(self):
        """Test successful password change"""
        token_str = str(self.token)

        with patch('builtins.input', side_effect=[token_str, 'oldpass', 'newpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.change_password()
                output = fake_out.getvalue()

        # Verify success message
        self.assertIn("SUCCESS: PASSWORD CHANGED!", output)

        # Verify password was actually changed
        self.assertEqual(self.system.user_table[0]['password'], 'newpass             ')

    def test_reject_invalid_token(self):
        """Test rejection of invalid session token"""
        with patch('builtins.input', side_effect=['999999', 'oldpass', 'newpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.change_password()
                output = fake_out.getvalue()

        # Verify error message
        self.assertIn("ERROR: INVALID SESSION TOKEN!", output)

        # Verify password unchanged
        self.assertEqual(self.system.user_table[0]['password'], 'oldpass             ')

    def test_reject_wrong_old_password(self):
        """Test rejection of wrong old password"""
        token_str = str(self.token)

        with patch('builtins.input', side_effect=[token_str, 'wrongpass', 'newpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.change_password()
                output = fake_out.getvalue()

        # Verify error message
        self.assertIn("ERROR: OLD PASSWORD INCORRECT!", output)

        # Verify password unchanged
        self.assertEqual(self.system.user_table[0]['password'], 'oldpass             ')

    def test_password_change_bug_line_210(self):
        """
        Test CRITICAL BUG preservation (COBOL line 210)

        Bug: Success flag set to 1 even when old password incorrect,
        causing loop to exit after first username match.
        """
        token_str = str(self.token)

        # Set up state to verify bug behavior
        with patch('builtins.input', side_effect=[token_str, 'wrongpass', 'newpass']):
            self.system.change_password()

        # Verify success flag was set to 1 (bug behavior)
        self.assertEqual(self.system.ws_success_flag, 1)

        # Verify password still unchanged (correct outcome despite flag)
        self.assertEqual(self.system.user_table[0]['password'], 'oldpass             ')

    def test_reject_inactive_session(self):
        """Test rejection of inactive session token"""
        token_str = str(self.token)

        # Set session to inactive
        self.system.session_table[0]['active'] = 0

        with patch('builtins.input', side_effect=[token_str, 'oldpass', 'newpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.change_password()
                output = fake_out.getvalue()

        self.assertIn("ERROR: INVALID SESSION TOKEN!", output)

    def test_change_to_same_password(self):
        """Test that changing to same password is allowed"""
        token_str = str(self.token)

        with patch('builtins.input', side_effect=[token_str, 'oldpass', 'oldpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.change_password()
                output = fake_out.getvalue()

        # Should succeed
        self.assertIn("SUCCESS: PASSWORD CHANGED!", output)

    def test_token_still_valid_after_password_change(self):
        """Test that session token remains valid after password change"""
        token_str = str(self.token)

        # Change password
        with patch('builtins.input', side_effect=[token_str, 'oldpass', 'newpass']):
            self.system.change_password()

        # Try to change again with same token but new old password
        with patch('builtins.input', side_effect=[token_str, 'newpass', 'newer']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.change_password()
                output = fake_out.getvalue()

        # Should succeed - token still valid
        self.assertIn("SUCCESS: PASSWORD CHANGED!", output)

    def test_token_collision_uses_first_match(self):
        """
        Test that with token collision, first matching active token is used
        (preserves COBOL bug - no collision detection)
        """
        # Create second user
        with patch('builtins.input', side_effect=['user2', 'pass2']):
            self.system.register_user()

        # Manually create second session with same token (collision)
        self.system.session_count += 1
        self.system.session_table[1]['token'] = self.token  # Same token!
        self.system.session_table[1]['username'] = 'user2               '
        self.system.session_table[1]['active'] = 1

        token_str = str(self.token)

        # Try to change password - should use FIRST matching session
        with patch('builtins.input', side_effect=[token_str, 'oldpass', 'newpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.change_password()
                output = fake_out.getvalue()

        # Should change first user's password, not second
        self.assertIn("SUCCESS: PASSWORD CHANGED!", output)
        self.assertEqual(self.system.user_table[0]['password'], 'newpass             ')
        self.assertEqual(self.system.user_table[1]['password'], 'pass2               ')


class TestStringPaddingBehavior(unittest.TestCase):
    """Test suite for COBOL string padding behavior"""

    def setUp(self):
        self.system = UserManagementSystem()

    def test_pad_string_function(self):
        """Test pad_string function directly"""
        # Short string
        self.assertEqual(self.system.pad_string('admin'), 'admin               ')
        self.assertEqual(len(self.system.pad_string('admin')), 20)

        # 1 character
        self.assertEqual(self.system.pad_string('a'), 'a                   ')

        # Empty string
        self.assertEqual(self.system.pad_string(''), ' ' * 20)

        # Exactly 20 characters
        self.assertEqual(self.system.pad_string('a' * 20), 'a' * 20)

        # Over 20 characters (truncate)
        self.assertEqual(self.system.pad_string('a' * 25), 'a' * 20)

    def test_username_comparison_with_padding(self):
        """Test that username comparison respects padding"""
        # Register 'user'
        with patch('builtins.input', side_effect=['user', 'pass']):
            self.system.register_user()

        # Verify stored as 20 chars
        self.assertEqual(self.system.user_table[0]['username'], 'user                ')

        # Try to register 'user' again (should detect duplicate)
        with patch('builtins.input', side_effect=['user', 'pass2']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()

        self.assertIn("ERROR: USERNAME ALREADY EXISTS!", output)


class TestMenuSystem(unittest.TestCase):
    """Test suite for menu system"""

    def setUp(self):
        self.system = UserManagementSystem()

    def test_menu_choice_1_register(self):
        """Test menu choice 1 calls register"""
        with patch('builtins.input', side_effect=['1', 'user', 'pass', '4']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.menu_loop()
                output = fake_out.getvalue()

        self.assertIn("--- USER REGISTRATION ---", output)
        self.assertEqual(self.system.user_count, 1)

    def test_menu_choice_4_exit(self):
        """Test menu choice 4 exits"""
        with patch('builtins.input', side_effect=['4']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.menu_loop()
                output = fake_out.getvalue()

        self.assertIn("SYSTEM SHUTDOWN...", output)

    def test_menu_invalid_choice_0(self):
        """Test menu choice 0 is invalid"""
        with patch('builtins.input', side_effect=['0', '4']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.menu_loop()
                output = fake_out.getvalue()

        self.assertIn("INVALID CHOICE. TRY AGAIN.", output)

    def test_menu_invalid_choice_5(self):
        """Test menu choice 5-9 is invalid"""
        with patch('builtins.input', side_effect=['5', '4']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.menu_loop()
                output = fake_out.getvalue()

        self.assertIn("INVALID CHOICE. TRY AGAIN.", output)

    def test_menu_input_truncation(self):
        """Test menu input '12' truncates to '1' (PIC 9 behavior)"""
        with patch('builtins.input', side_effect=['12', 'user', 'pass', '4']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.menu_loop()
                output = fake_out.getvalue()

        # Should call register (option 1)
        self.assertIn("--- USER REGISTRATION ---", output)


class TestTokenGeneration(unittest.TestCase):
    """Test suite for token generation"""

    def setUp(self):
        self.system = UserManagementSystem()

    def test_token_in_range_100000_to_999999(self):
        """Test that all generated tokens are in valid range"""
        tokens = []
        for _ in range(100):
            self.system.generate_token()
            tokens.append(self.system.ws_random_num)

        # All tokens should be in range
        for token in tokens:
            self.assertGreaterEqual(token, 100000)
            self.assertLessEqual(token, 999999)

    def test_token_is_integer(self):
        """Test that token is an integer (not float)"""
        self.system.generate_token()
        self.assertIsInstance(self.system.ws_random_num, int)

    def test_tokens_vary(self):
        """Test that tokens are different (not always same value)"""
        tokens = set()
        for _ in range(50):
            self.system.generate_token()
            tokens.add(self.system.ws_random_num)

        # Should have some variety (not all identical)
        # Allow for small chance of collision in 50 attempts
        self.assertGreater(len(tokens), 1)


class TestDataStructureInitialization(unittest.TestCase):
    """Test suite for data structure initialization"""

    def test_user_table_initialized_correctly(self):
        """Test user table has 100 slots, all initialized"""
        system = UserManagementSystem()

        # Check size
        self.assertEqual(len(system.user_table), 100)

        # Check all slots initialized
        for i in range(100):
            self.assertEqual(system.user_table[i]['username'], ' ' * 20)
            self.assertEqual(system.user_table[i]['password'], ' ' * 20)
            self.assertEqual(system.user_table[i]['active'], 0)

    def test_session_table_initialized_correctly(self):
        """Test session table has 50 slots, all initialized"""
        system = UserManagementSystem()

        # Check size
        self.assertEqual(len(system.session_table), 50)

        # Check all slots initialized
        for i in range(50):
            self.assertEqual(system.session_table[i]['token'], 0)
            self.assertEqual(system.session_table[i]['username'], ' ' * 20)
            self.assertEqual(system.session_table[i]['active'], 0)

    def test_counters_initialized_to_zero(self):
        """Test user_count and session_count start at 0"""
        system = UserManagementSystem()

        self.assertEqual(system.user_count, 0)
        self.assertEqual(system.session_count, 0)


class TestCompleteUserJourney(unittest.TestCase):
    """Integration tests for complete user journeys"""

    def test_scenario_1_complete_user_journey(self):
        """
        Test Scenario 1 from spec:
        Register -> Login -> Change Password -> Exit
        """
        system = UserManagementSystem()

        # 1. Register alice
        with patch('builtins.input', side_effect=['alice', 'pass123']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                system.register_user()
                output = fake_out.getvalue()
        self.assertIn("SUCCESS: USER REGISTERED!", output)

        # 2. Login alice
        with patch('builtins.input', side_effect=['alice', 'pass123']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                system.login_user()
                output = fake_out.getvalue()
        self.assertIn("SUCCESS: LOGIN APPROVED", output)
        token = system.session_table[0]['token']

        # 3. Change password
        with patch('builtins.input', side_effect=[str(token), 'pass123', 'newpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                system.change_password()
                output = fake_out.getvalue()
        self.assertIn("SUCCESS: PASSWORD CHANGED!", output)

        # 4. Verify can login with new password
        with patch('builtins.input', side_effect=['alice', 'newpass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                system.login_user()
                output = fake_out.getvalue()
        self.assertIn("SUCCESS: LOGIN APPROVED", output)

        # 5. Verify cannot login with old password
        with patch('builtins.input', side_effect=['alice', 'pass123']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                system.login_user()
                output = fake_out.getvalue()
        self.assertIn("ERROR: INVALID CREDENTIALS!", output)

    def test_scenario_2_session_full(self):
        """
        Test Scenario 2 from spec:
        Register -> Login 50 times -> 51st login fails
        """
        system = UserManagementSystem()

        # Register user
        with patch('builtins.input', side_effect=['user', 'pass']):
            system.register_user()

        # Login 50 times
        for i in range(50):
            with patch('builtins.input', side_effect=['user', 'pass']):
                system.login_user()

        self.assertEqual(system.session_count, 50)

        # 51st login should fail with session table full
        with patch('builtins.input', side_effect=['user', 'pass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                system.login_user()
                output = fake_out.getvalue()

        self.assertIn("ERROR: SESSION TABLE FULL!", output)
        self.assertEqual(system.session_count, 50)  # No new session

    def test_scenario_3_database_full(self):
        """
        Test Scenario 3 from spec:
        Register 100 users -> 101st fails
        """
        system = UserManagementSystem()

        # Register 100 users
        for i in range(100):
            with patch('builtins.input', side_effect=[f'user{i}', f'pass{i}']):
                system.register_user()

        self.assertEqual(system.user_count, 100)

        # 101st registration should fail
        with patch('builtins.input', side_effect=['user101', 'pass101']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                system.register_user()
                output = fake_out.getvalue()

        self.assertIn("ERROR: USER DATABASE FULL!", output)
        self.assertEqual(system.user_count, 100)


class TestErrorMessages(unittest.TestCase):
    """Test exact error message formatting"""

    def setUp(self):
        self.system = UserManagementSystem()

    def test_exact_error_messages(self):
        """Verify exact error message text matches spec"""
        # Register duplicate
        with patch('builtins.input', side_effect=['user', 'pass']):
            self.system.register_user()
        with patch('builtins.input', side_effect=['user', 'pass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()
        self.assertIn("ERROR: USERNAME ALREADY EXISTS!", output)

        # Invalid credentials
        with patch('builtins.input', side_effect=['wrong', 'wrong']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.login_user()
                output = fake_out.getvalue()
        self.assertIn("ERROR: INVALID CREDENTIALS!", output)

        # Invalid token
        with patch('builtins.input', side_effect=['999999', 'old', 'new']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.change_password()
                output = fake_out.getvalue()
        self.assertIn("ERROR: INVALID SESSION TOKEN!", output)

    def test_exact_success_messages(self):
        """Verify exact success message text matches spec"""
        # Register success
        with patch('builtins.input', side_effect=['user', 'pass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.register_user()
                output = fake_out.getvalue()
        self.assertIn("SUCCESS: USER REGISTERED!", output)

        # Login success
        with patch('builtins.input', side_effect=['user', 'pass']):
            with patch('sys.stdout', new=StringIO()) as fake_out:
                self.system.login_user()
                output = fake_out.getvalue()
        self.assertIn("SUCCESS: LOGIN APPROVED", output)
        self.assertIn("YOUR SESSION TOKEN:", output)


def run_test_suite():
    """Run all tests and display results"""
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add all test classes
    suite.addTests(loader.loadTestsFromTestCase(TestUserRegistration))
    suite.addTests(loader.loadTestsFromTestCase(TestUserLogin))
    suite.addTests(loader.loadTestsFromTestCase(TestPasswordChange))
    suite.addTests(loader.loadTestsFromTestCase(TestStringPaddingBehavior))
    suite.addTests(loader.loadTestsFromTestCase(TestMenuSystem))
    suite.addTests(loader.loadTestsFromTestCase(TestTokenGeneration))
    suite.addTests(loader.loadTestsFromTestCase(TestDataStructureInitialization))
    suite.addTests(loader.loadTestsFromTestCase(TestCompleteUserJourney))
    suite.addTests(loader.loadTestsFromTestCase(TestErrorMessages))

    # Run tests with verbose output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    # Print summary
    print("\n" + "=" * 70)
    print("TEST SUMMARY")
    print("=" * 70)
    print(f"Tests Run: {result.testsRun}")
    print(f"Successes: {result.testsRun - len(result.failures) - len(result.errors)}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print("=" * 70)

    # Return exit code
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(run_test_suite())
