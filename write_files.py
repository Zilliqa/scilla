#!/usr/bin/env python3

import sys
import os

def main():
    # Read the input as binary data from stdin
    input_data = sys.stdin.buffer.read()

    # Split the input into a path and data
    try:
        path, data = input_data.split(b' ', 1)
    except ValueError:
        print("Invalid input format. Please provide a valid absolute path and data.")
        sys.exit(1)

    # Ensure the path is absolute
    if not os.path.isabs(path.decode()):
        print("Please provide an absolute path.")
        sys.exit(1)

    # Write binary data to the specified file
    try:
        with open(path.decode(), 'wb') as file:
            file.write(data)
        print(f"Data written to {path.decode()}")
    except Exception as e:
        print(f"Error writing data to {path.decode()}: {str(e)}")
        sys.exit(1)

if __name__ == "__main__":
    main()
