#!/usr/bin/env python3
"""
USER MANAGEMENT SYSTEM - Python Migration from COBOL
Faithful conversion of legacy_app.cob preserving all behaviors, bugs, and quirks.

Original: USERMGMT (COBOL, 1987-03-15)
Migrated: 2025-10-08
Author: LEGACY-SYSTEMS-DEPT

IMPORTANT: This migration preserves all original behaviors including:
- Plaintext password storage
- Predictable token generation
- Password change bug (line 210 in COBOL)
- No session expiration
- 20-character space-padded strings
- 1-indexed COBOL behavior converted to 0-indexed Python
"""

import random


class UserManagementSystem:
    """
    User Management System - Faithful COBOL Migration

    Data Structures:
    - USER_TABLE: 100 fixed slots (username, password, active flag)
    - SESSION_TABLE: 50 fixed slots (token, username, active flag)

    All strings are 20-character space-padded for exact COBOL behavior.
    """

    def __init__(self):
        """Initialize system - mirrors INITIALIZE-SYSTEM procedure"""
        # User table - 100 slots (COBOL: indexed 1-100, Python: 0-99)
        self.user_table = []
        for _ in range(100):
            self.user_table.append({
                'username': ' ' * 20,      # PIC X(20) - 20 spaces
                'password': ' ' * 20,      # PIC X(20) - 20 spaces
                'active': 0                # PIC 9 VALUE 0
            })

        # Session table - 50 slots (COBOL: indexed 1-50, Python: 0-49)
        self.session_table = []
        for _ in range(50):
            self.session_table.append({
                'token': 0,                # PIC 9(6) - 6 digits
                'username': ' ' * 20,      # PIC X(20) - 20 spaces
                'active': 0                # PIC 9 VALUE 0
            })

        # Counters (PIC 999 - can hold 0-999)
        self.user_count = 0          # Current users (max 100)
        self.session_count = 0       # Current sessions (max 50)

        # Working storage variables
        self.ws_username = ''
        self.ws_password = ''
        self.ws_old_password = ''
        self.ws_new_password = ''
        self.ws_token = 0
        self.ws_menu_choice = 0

        # Status flags
        self.ws_user_found = 0
        self.ws_success_flag = 0
        self.ws_random_num = 0

        # Temp variables
        self.ws_temp_user = ''

    def pad_string(self, s):
        """
        Pad string to exactly 20 characters (COBOL PIC X(20) behavior)

        - Truncates if longer than 20
        - Right-pads with spaces if shorter
        - Returns exactly 20 characters
        """
        if len(s) > 20:
            return s[:20]
        return s.ljust(20, ' ')

    def display_welcome(self):
        """DISPLAY-WELCOME procedure"""
        print("========================================")
        print("   USER MANAGEMENT SYSTEM v1.0         ")
        print("   LEGACY MAINFRAME APPLICATION        ")
        print("========================================")

    def menu_loop(self):
        """
        MENU-LOOP procedure

        Loops until user selects option 4 (EXIT)
        Mimics COBOL: PERFORM MENU-LOOP UNTIL WS-MENU-CHOICE = 4
        """
        while self.ws_menu_choice != 4:
            print(" ")
            print("MAIN MENU:")
            print("1. REGISTER NEW USER")
            print("2. LOGIN")
            print("3. CHANGE PASSWORD")
            print("4. EXIT")

            # DISPLAY "ENTER CHOICE (1-4): " WITH NO ADVANCING
            choice_input = input("ENTER CHOICE (1-4): ")

            # PIC 9 - single digit, truncate to first character
            # Non-numeric input would cause COBOL error, Python handles gracefully
            try:
                if choice_input:
                    self.ws_menu_choice = int(choice_input[0])
                else:
                    self.ws_menu_choice = 0
            except ValueError:
                self.ws_menu_choice = 0

            # EVALUATE (COBOL switch statement)
            if self.ws_menu_choice == 1:
                self.register_user()
            elif self.ws_menu_choice == 2:
                self.login_user()
            elif self.ws_menu_choice == 3:
                self.change_password()
            elif self.ws_menu_choice == 4:
                print("SYSTEM SHUTDOWN...")
            else:
                print("INVALID CHOICE. TRY AGAIN.")

    def register_user(self):
        """
        REGISTER-USER procedure

        Registers new user with username/password.
        Checks for duplicates, enforces 100-user limit.
        """
        print("--- USER REGISTRATION ---")

        # DISPLAY "ENTER USERNAME: " WITH NO ADVANCING
        username_input = input("ENTER USERNAME: ")
        self.ws_username = self.pad_string(username_input)

        # DISPLAY "ENTER PASSWORD: " WITH NO ADVANCING
        password_input = input("ENTER PASSWORD: ")
        self.ws_password = self.pad_string(password_input)

        # CHECK IF USER ALREADY EXISTS
        # COBOL: PERFORM VARYING USER-IDX FROM 1 BY 1 UNTIL USER-IDX > USER-COUNT OR WS-USER-FOUND = 1
        # Note: COBOL searches 1 to USER-COUNT (not full array)
        # Python: Convert to 0-indexed (0 to user_count-1)
        self.ws_user_found = 0

        for user_idx in range(self.user_count):  # 0 to user_count-1
            if self.user_table[user_idx]['username'] == self.ws_username:
                self.ws_user_found = 1
                break  # Early exit on match

        if self.ws_user_found == 1:
            print("ERROR: USERNAME ALREADY EXISTS!")
        else:
            if self.user_count < 100:
                # COBOL: ADD 1 TO USER-COUNT, SET USER-IDX TO USER-COUNT
                # In Python 0-indexed: new user goes at index user_count
                self.user_count += 1
                user_idx = self.user_count - 1  # Convert to 0-based index

                self.user_table[user_idx]['username'] = self.ws_username
                self.user_table[user_idx]['password'] = self.ws_password
                self.user_table[user_idx]['active'] = 1

                print("SUCCESS: USER REGISTERED!")
            else:
                print("ERROR: USER DATABASE FULL!")

    def login_user(self):
        """
        LOGIN-USER procedure

        Authenticates user and creates session with token.
        Validates username AND password AND active flag.
        Generates 6-digit token, enforces 50-session limit.
        """
        print("--- USER LOGIN ---")

        username_input = input("ENTER USERNAME: ")
        self.ws_username = self.pad_string(username_input)

        password_input = input("ENTER PASSWORD: ")
        self.ws_password = self.pad_string(password_input)

        # VERIFY CREDENTIALS
        # COBOL: Loop through 1 to USER-COUNT, check username AND password AND active
        self.ws_user_found = 0

        for user_idx in range(self.user_count):
            # All three conditions must be true
            if (self.user_table[user_idx]['username'] == self.ws_username and
                self.user_table[user_idx]['password'] == self.ws_password and
                self.user_table[user_idx]['active'] == 1):
                self.ws_user_found = 1
                break  # Early exit

        if self.ws_user_found == 1:
            # PERFORM GENERATE-TOKEN
            self.generate_token()

            # Check session capacity AFTER validating credentials
            # This preserves COBOL bug: credentials validated but session may fail
            if self.session_count < 50:
                self.session_count += 1
                sess_idx = self.session_count - 1  # Convert to 0-based

                self.session_table[sess_idx]['token'] = self.ws_random_num
                self.session_table[sess_idx]['username'] = self.ws_username
                self.session_table[sess_idx]['active'] = 1

                print("SUCCESS: LOGIN APPROVED")
                # COBOL: DISPLAY "YOUR SESSION TOKEN: " SESSION-TOKEN(SESS-IDX)
                print(f"YOUR SESSION TOKEN: {self.session_table[sess_idx]['token']}")
            else:
                print("ERROR: SESSION TABLE FULL!")
        else:
            print("ERROR: INVALID CREDENTIALS!")

    def change_password(self):
        """
        CHANGE-PASSWORD procedure

        Changes user password using session token.
        Two-phase validation: token -> username -> old password.

        CRITICAL BUG PRESERVATION (COBOL line 210):
        Sets success flag to 1 even when old password is incorrect,
        causing loop to exit after first username match.
        """
        print("--- CHANGE PASSWORD ---")

        # Accept session token (PIC 9(6) - 6 digits)
        token_input = input("ENTER SESSION TOKEN: ")
        try:
            self.ws_token = int(token_input)
        except ValueError:
            self.ws_token = 0

        old_pwd_input = input("ENTER OLD PASSWORD: ")
        self.ws_old_password = self.pad_string(old_pwd_input)

        new_pwd_input = input("ENTER NEW PASSWORD: ")
        self.ws_new_password = self.pad_string(new_pwd_input)

        # VALIDATE SESSION TOKEN
        # COBOL: MOVE 0 TO WS-USER-FOUND, MOVE SPACES TO WS-TEMP-USER
        self.ws_user_found = 0
        self.ws_temp_user = ' ' * 20

        for sess_idx in range(self.session_count):
            if (self.session_table[sess_idx]['token'] == self.ws_token and
                self.session_table[sess_idx]['active'] == 1):
                self.ws_temp_user = self.session_table[sess_idx]['username']
                self.ws_user_found = 1
                break  # Early exit

        if self.ws_user_found == 0:
            print("ERROR: INVALID SESSION TOKEN!")
        else:
            # FIND USER AND VERIFY OLD PASSWORD
            # COBOL: MOVE 0 TO WS-SUCCESS-FLAG
            self.ws_success_flag = 0

            for user_idx in range(self.user_count):
                if self.user_table[user_idx]['username'] == self.ws_temp_user:
                    if self.user_table[user_idx]['password'] == self.ws_old_password:
                        # Password matches - update it
                        self.user_table[user_idx]['password'] = self.ws_new_password
                        self.ws_success_flag = 1
                        print("SUCCESS: PASSWORD CHANGED!")
                    else:
                        # Password doesn't match
                        print("ERROR: OLD PASSWORD INCORRECT!")
                        # CRITICAL BUG (COBOL line 210): Set flag to 1 anyway!
                        self.ws_success_flag = 1

                    # COBOL: Loop exits when WS-SUCCESS-FLAG = 1
                    break

    def generate_token(self):
        """
        GENERATE-TOKEN procedure

        Generates pseudo-random 6-digit token (100000-999999).
        COBOL: COMPUTE WS-RANDOM-NUM = FUNCTION RANDOM * 900000 + 100000

        NOT cryptographically secure (preserves COBOL behavior).
        No collision detection (preserves COBOL bug).
        """
        # COBOL FUNCTION RANDOM returns 0.0 to 1.0
        # Python random.random() has same range
        # No explicit seeding (preserves COBOL behavior)
        self.ws_random_num = int(random.random() * 900000 + 100000)

    def main_routine(self):
        """
        MAIN-ROUTINE procedure

        Entry point: initialize -> welcome -> menu loop -> exit
        COBOL: PERFORM INITIALIZE-SYSTEM, PERFORM DISPLAY-WELCOME,
               PERFORM MENU-LOOP UNTIL WS-MENU-CHOICE = 4, STOP RUN
        """
        # Initialization happens in __init__
        self.display_welcome()
        self.menu_loop()
        # STOP RUN - program exits


def main():
    """
    Program entry point
    Creates system instance and runs main routine.
    """
    system = UserManagementSystem()
    system.main_routine()


if __name__ == "__main__":
    main()
