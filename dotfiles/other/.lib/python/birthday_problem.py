import random
import math

class BirthdayProblem(object):

    def __init__(self):
        pass


def birthday_problem(problem_size=365):
    birthdays = set()
    while True:
        new_birthday = random.randint(1, problem_size)
        if new_birthday in birthdays:
            return len(birthdays) + 1
        birthdays.add(new_birthday)


def theoretical_average(problem_size):
    probabilities = []
    contributions = []
    for n in range(1, problem_size):
        probability = (float(n-1) / problem_size) * falling_factorial_over_exponentiation(problem_size, n-1)
        contribution = n * probability
        probabilities.append(probability)
        contributions.append(contribution)
    return sum(contributions)


def falling_factorial(n, k):
    product = 1
    while k > 0:
        product *= n
        n -= 1
        k -= 1
    return product



def falling_factorial_over_exponentiation(n, k):
    orig = n
    product = float(1)
    while k > 0:
        product *= n
        product = product/orig
        n -= 1
        k -= 1
    return product


def run_birthday_problem_n_times(times_to_run, problem_size=365):
    return [birthday_problem(problem_size) for i in range(int(times_to_run))]


def number_of_people_to_times_occured(runs):
    number_of_people_to_times_occured = {}
    for run in runs:
        number_of_people_to_times_occured[run] = number_of_people_to_times_occured.get(run, 0) + 1


if __name__ == '__main__':
    times_to_run = 131072
    while times_to_run <= 131072:
        for problem_size in range(4000, 5000, 100):
            average = sum(run_birthday_problem_n_times(times_to_run, problem_size=problem_size))/float(times_to_run)
            print "problem size {3} ran {0} times, average was {1}, theoretical average is {2}".format(
                times_to_run,
                average,
                theoretical_average(problem_size),
                problem_size
            )
            print math.fabs(average - theoretical_average(problem_size))
        times_to_run *= 2