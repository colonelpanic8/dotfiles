#! /usr/bin/env python
import sys

def score_parentheses(input_string, index=0):
    if index >= len(input_string):
        return (0, index)
    if input_string[index] == '(':
        index += 1
    else:
        raise Exception("Invalid parentheses")

    children_score, index = score_children(input_string, index)

    if input_string[index] == ')':
        index += 1
    else:
        raise Exception("Invalid parentheses")

    return (children_score * 2 if children_score > 0 else 1, index)

def score_children(input_string, index=0):
    input_length = len(input_string)
    children_score = 0
    while index < input_length and input_string[index] == '(':
        child_score, index = score_parentheses(input_string, index)
        children_score += child_score
    return (children_score, index)


if __name__ == '__main__':
    print (score_children(sys.argv[1]))
