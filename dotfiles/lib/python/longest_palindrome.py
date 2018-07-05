#!/usr/bin/env python
import sys
from cached_property import cached_property


class PalindromeSubstringFinder(object):

    def __init__(self, input_string):
        self.input_string = input_string
        self.input_length = len(input_string)
        self.position_count = (len(input_string) * 2) - 1
        self._palindrome_lengths = [0] * self.position_count
        self._total_comparisons = 0
        self._max_palindrome_index = 0

    @property
    def longest_palindrome(self):
        index = self._max_palindrome_index // 2
        half_length = self.longest_palindrome_length // 2
        offset =  self.longest_palindrome_length % 2
        left_index = index - half_length + 1 - offset
        right_index = index + half_length + 1
        return self.input_string[left_index:right_index]

    @property
    def longest_palindrome_length(self):
        return self._palindrome_lengths[self._max_palindrome_index]

    def print_current_state(self, position):
        print ("".join(map(str, self._palindrome_lengths)))
        print (" ".join(self.input_string))
        print ("{0}^".format(" " * position))

    def palindrome_lengths(self):
        if self.input_string is None or self.input_length < 1:
            return []
        max_reach = -1
        for position in range(self.position_count):
            starting_offset = self._palindrome_lengths[position] // 2
            index = position // 2
            right_offset = position % 2

            palindrome_length, max_index = self.get_length_from_indices(
                index - starting_offset,
                index + starting_offset + right_offset,
            )

            self._palindrome_lengths[position] = palindrome_length

            if palindrome_length > self._palindrome_lengths[self._max_palindrome_index]:
                self._max_palindrome_index = position

            if max_reach < max_index:
                max_reach = max_index
                self.copy_palindrome_lengths(position, palindrome_length)

            self.print_current_state(position)

        return self._palindrome_lengths

    def copy_palindrome_lengths(self, position, palindrome_length):
        # b a a b a a b
        # 1004007004001
        for offset in range(1, palindrome_length - 1):
            self._palindrome_lengths[position + offset] = (
                min(
                    self._palindrome_lengths[position - offset],
                    palindrome_length - offset
                )
            )

    def get_length_from_indices(self, left_index, right_index):
        while (left_index >= 0 and right_index < self.input_length and
               self.input_string[left_index] == self.input_string[right_index]):
            self._total_comparisons += 1
            left_index -= 1
            right_index += 1
        self._total_comparisons += 1
        # We'll always go one set of indices PAST the point that we should have,
        # so right_index - left_index + 1 becomes the below:
        return right_index - left_index - 1, right_index - 1


if __name__ == '__main__':
    target = sys.argv[1]
    finder = PalindromeSubstringFinder(target)
    finder.palindrome_lengths()
    print(finder._total_comparisons)
    print(finder._max_palindrome_index)
    print(finder.longest_palindrome)
