#!/usr/bin/env python3

import subprocess
import sys
from pathlib import Path

TIMEOUT=1

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
            timeout=TIMEOUT
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
5 5
1 2
2 3
3 1
2 1
3 4
"""
output1 =\
"""
0
1
2
3
-1
"""

input2 =\
"""
5 6
1 2
1 3
2 4
3 4
4 5
3 5
"""

output2 =\
"""
0
1
1
2
2
"""

input3 =\
"""
4 2
1 2
2 3
"""

output3 =\
"""
0
1
2
-1
"""

input4 =\
"""
3 3
1 2
2 3
3 1
"""

output4 =\
"""
0
1
2
"""

input5 =\
"""
200000 1
1 200000
"""

output5 = "0\n" + ("-1\n" * 199998) + "1\n"  

input6 =\
"""
2 1
1 2
"""

output6 =\
"""
0
1
"""

input7 =\
"""
2 0
"""

output7 =\
"""
0
-1
"""

input8 =\
"""
6 8  
1 2  
1 3  
2 4  
3 4  
4 5  
5 6  
2 5  
3 6
"""

output8 =\
"""
0
1
1
2
2
2  
"""

input9 = "200000 199999\n"
for i in range(1, 200000):
    input9 += "%d %d\n" % (i, i+1)

output9 = ""
for i in range(0, 200000):
    output9 += "%d\n" % i

input10=\
"""
1 1
1 1
"""

output10 = "0"

# Test cases - modify these to match your program's requirements
TEST_CASES = [
    {
        'input': input1,
        'expected_output': output1,
        'description': 'Example'
    },
    {
        'input': input2,
        'expected_output': output2,
        'description': 'Simple graph'
    },
    {
        'input': input3,
        'expected_output': output3,
        'description': 'Simple with unreachables'
    },
    {
        'input': input4,
        'expected_output': output4,
        'description': 'Cycle'
    },
    {
        'input': input5,
        'expected_output': output5,
        'description': 'Big graph with single path'
    },
    {
        'input': input6,
        'expected_output': output6,
        'description': 'Minimal with one edge'
    },
    {
        'input': input7,
        'expected_output': output7,
        'description': 'Minimal without edge'
    },
    {
        'input': input8,
        'expected_output': output8,
        'description': 'With bottleneck'
    },
    {
        'input': input9,
        'expected_output': output9,
        'description': 'Big graph with long path'
    },
    {
        'input': input10,
        'expected_output': output10,
        'description': 'Minimal pseudo-graph'
    }   
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
