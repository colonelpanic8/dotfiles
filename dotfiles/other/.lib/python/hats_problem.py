import random


class HatsProblem(object):

    def __init__(self, size):
        self.size = size

    def build_hats(self):
        return [self.hat() for _ in range(self.size)]

    def hat(self):
        return random.randint(0, self.size - 1)

    def go(self):
        hats = self.build_hats()
        guesses = [
            self.calculate_guess_modulus(self.sum_of_all_but_i(i, hats), i)
            for i in range(self.size)
        ]
        return zip(hats, guesses)

    def calculate_guess_modulus(self, current, desired):
        return ((desired - current) + self.size) % self.size

    def sum_of_all_but_i(self, i, hats):
        return sum(hat for index, hat in enumerate(hats) if index != i)
