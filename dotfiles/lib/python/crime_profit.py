class CountCrimes(object):

    def __init__(self, profits, groups, profit_needed, group_count):
        self.profits = profits
        self.groups = groups
        self.crime_count = len(profits)
        self.profit_needed = profit_needed
        self.group_count = count
        self.reset_cache()

    def process_crime(self, profit, num_required):
        for gangster_count in range(self.group_count, -1, -1):
            for profit_amount in range(self.profit_needed, -1, -1):
                new_gangster_count = gangster_count + num_required
                new_profit = profit_amount + profit
                if new_profit > self.profit_needed:
                    new_profit = self.profit_needed
                new_count = self.cache[gangster_count][profit_amount]
                if new_count > 0 and new_gangster_count <= self.group_count:
                    self.cache[new_gangster_count][new_profit] += new_count

    def reset_cache(self):
        self.cache = [[0 for _ in range(profit_needed + 1)]
                      for _ in range(group_count + 1)]
        self.cache[0][0] = 1

    def process_crimes(self):
        for profit, num_required in zip(self.profits, self.groups):
            self.process_crime(profit, num_required)

    def get_count(self):
        self.reset_cache()
        self.process_crimes()
        return self.count_ways()

    def count_ways(self):
        return sum(self.cache[i][self.profit_needed]
                   for i in range(self.group_count + 1))


if __name__ == '__main__':
    print(CountCrimes().get_count())
