#! /usr/bin/env python
import itertools

def textJustify(input_string, justification_length):
    partitioning = partition_paragraph(input_string, justification_length)
    return "\n".join(itertools.chain(
        (justify_line(line_partition, justification_length)
         for line_partition in partitioning[:-1]),
        [" ".join(partitioning[-1])]
    ))

def justify_line(line_words, justification_length):
    if len(line_words) == 1:
        return line_words[0]
    total_length = sum(len(word) for word in line_words)
    word_count = len(line_words)
    number_of_word_boundaries = word_count - 1
    spaces_to_add = justification_length - total_length
    base_spaces = spaces_to_add // number_of_word_boundaries
    extra_spaces = spaces_to_add % number_of_word_boundaries

    output_string = ""
    for i, word in enumerate(line_words):
        output_string += word
        if i >= len(line_words) - 1:
            break
        space_count = base_spaces
        if i < extra_spaces:
            space_count += 1
        spaces = " " * space_count
        output_string += spaces

    return output_string

def partition_paragraph(input_string, justification_length):
    min_line_length = 0
    partitioning = []
    current = []
    for word in input_string.split():
        word_length = len(word)
        length_with_word = min_line_length + word_length
        if justification_length < length_with_word:
            partitioning.append(current)
            current = []
            min_line_length = 0
            length_with_word = word_length

        current.append(word)
        min_line_length = length_with_word + 1

    if current:
        partitioning.append(current)

    return partitioning

if __name__ == '__main__':
    sample = "Coursera provides universal access to the world's best education, partnering with to universities and organizations to offer courses online."
    print(textJustify(sample, 10))
