#!/usr/bin/env python3

import subprocess
import sys
from pathlib import Path

def run_test(program_path, input_data, expected_output):
    """
    Run the program with given input and check if output matches expected output.
    
    Args:
        program_path (str): Path to the executable program
        input_data (str): Input to send to stdin
        expected_output (str): Expected output from stdout
    
    Returns:
        bool: True if test passed, False otherwise
    """
    try:
        # Run the program and communicate with it
        process = subprocess.run(
            [program_path],
            input=input_data,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=5  # Safety timeout in seconds
        )
        
        # Check for errors
        if process.stderr:
            print(f"Program error:\n{process.stderr}", file=sys.stderr)
            return False
        
        # Compare output
        actual_output = process.stdout.strip()
        if actual_output == expected_output.strip():
            return True
        else:
            print("\nTest failed!")
            print("Expected output:")
            print(expected_output)
            print("Actual output:")
            print(actual_output)
            return False
            
    except subprocess.TimeoutExpired:
        print("Test timed out (possible infinite loop)", file=sys.stderr)
        return False
    except Exception as e:
        print(f"Error running test: {str(e)}", file=sys.stderr)
        return False

input1 =\
"""
5 8
?.?...??
?????.??
......?.
??.?.?.?
?????..?
"""
output1 =\
"""
YES
X.......
..X.....
......X.
.X......
...X....
"""

input2 =\
"""
3 3
???
???
???
"""

output2 = 'NO'

input3 =\
"""
1 1
?
"""

output3 =\
"""
YES
X
"""

input4 =\
"""
1 2
??
"""

output4 =\
"""
YES
X.
"""

input5 =\
"""
12 1
?
?
?
?
?
?
?
?
?
?
?
?
"""

output5 = 'NO'


input6 =\
"""
5 8
..?...??
??.??.??
........
??.?.?.?
?????..?
"""
output6 = 'NO'

# input6 =\
# """
# 12
# ********** *
# *        * *
# * ****** * *
# * *    * * *
# * * ** * * *
# * * ** * * *
# * * ** * * *
# * * **** * *
# * *      * *
# * ******** *
# *          *
# ************
# """

# output6 = '0'

# input7 =\
# """
# 12
# ********** *
# *        * *
# * ****** * *
# * *    * * *
# * * **** * *
# * * **** * *
# * * *  * * *
# * * **** * *
# * *      * *
# * ******** *
# *          *
# ************
# """

# output7 = '1'


# input8 =\
# """
# 12
# ********** *
# *        * *
# * ****** * *
# * *    * * *
# * * ** * * *
# * * ** * * *
# * * ** * * *
# * * **** * *
# * *     ** *
# * ******** *
# * *      * *
# ************
# """

# output8 = '3'


# Test cases - modify these to match your program's requirements
TEST_CASES = [
    {
        'input': input1,
        'expected_output': output1,
        'description': 'Example 1'
    },
    {
        'input': input2,
        'expected_output': output2,
        'description': 'Example 2'
    },
    {
        'input': input3,
        'expected_output': output3,
        'description': ''
    },
    {
        'input': input4,
        'expected_output': output4,
        'description': ''
    },
    {
        'input': input5,
        'expected_output': output5,
        'description': ''
    },
    {
        'input': input6,
        'expected_output': output6,
        'description': ''
    },
    # {
    #     'input': input7,
    #     'expected_output': output7,
    #     'description': ''
    # },
    # {
    #     'input': input8,
    #     'expected_output': output8,
    #     'description': ''
    # }
]

def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <path_to_executable>", file=sys.stderr)
        sys.exit(1)
    
    program_path = Path(sys.argv[1]).absolute()
    if not program_path.exists():
        print(f"Error: Program not found at {program_path}", file=sys.stderr)
        sys.exit(1)
    
    print(f"Testing program: {program_path}")
    print(f"Running {len(TEST_CASES)} test cases...\n")
    
    passed = 0
    for i, test_case in enumerate(TEST_CASES, 1):
        print(f"Test {i}: {test_case['description']}... ", end='', flush=True)
        if run_test(program_path, test_case['input'], test_case['expected_output']):
            print("PASSED")
            passed += 1
        else:
            print("FAILED")
    
    print(f"\nTest results: {passed} passed, {len(TEST_CASES)-passed} failed")
    if passed == len(TEST_CASES):
        print("All tests passed successfully!")
        sys.exit(0)
    else:
        sys.exit(1)

if __name__ == "__main__":
    main()
