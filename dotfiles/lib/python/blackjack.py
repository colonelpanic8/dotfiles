import random
import enum
import itertools


class Suit(enum.Enum):
    CLUBS = 0
    DIAMONDS = 1
    HEARTS = 2
    SPADES = 3


class Deck(object):

    @classmethod
    def random_deck(cls):
        return cls(generate_cards())

    def __init__(self, cards):
        self._cards = cards
        self.top = 0

    def pop_top(self):
        if self.top >= 52:
            raise Exception()
        card = get_card(self._cards[self.top])
        self.top += 1
        return card


def get_card(card_number):
    return (Suit(card_number // 13), card_number % 13)


def random_permutation_of_size(n):
    remaining = list(range(n))
    for i in range(n-1, -1, -1):
        yield remaining.pop(random.randint(0, i))


def random_permutation(the_list):
    for index in random_permutation_of_size(len(the_list)):
        yield the_list[index]


def generate_cards(num_cards=52):
    return list(random_permutation(range(num_cards)))


def card_value(card_number):
    if card_number >= 10:
        return 10
    return card_number + 1


def card_string(card_number):
    if card_number == 12:
        return 'K'
    elif card_number == 11:
        return 'Q'
    elif card_number == 10:
        return 'J'
    return str(card_number + 1)


def get_hand_value(hand):
    number_of_aces = 0
    total_value = 0
    for _, card_number in hand:
        if card_number == 0:
            number_of_aces += 1
        else:
            total_value += card_value(card_number)
    while total_value < 10 - (number_of_aces - 1):
        total_value += 11
        number_of_aces -= 1
    total_value += number_aces
    return total_value


class Blackjack(object):

    def __init__(self, deck=None):
        self._deck = deck or Deck.random_deck()
        self.initialize_game()

    def initialize_game(self):
        self.dealer_hand = [self._deck.pop_top() for _ in range(2)]
        self.player_hand = [self._deck.pop_top() for _ in range(2)]

    def hit(self):
        self.player_hand.append(self._deck.pop_top())

    def run_dealer(self):
        while get_hand_value(self.dealer_hand) < 17:
            self.dealer_hand.append(self._deck.pop_top())


class UserHandler(object):

    def __init__(self, game=None):
        self._game = game or Blackjack()

    def print_game_state(self):
        print(self.game_string())

    def game_string(self):
        return "\n".join([
            self.dealer_string(), self.hand_string(self._game.player_hand),
        ])

    def dealer_string(self):
        return "X {0}".format(self.hand_string(self._game.dealer_hand[1:]))

    def hand_string(self, cards):
        return " ".join(card_string(card[1]) for card in cards)


if __name__ == '__main__':
    UserHandler().print_game_state()
    the_cards = UserHandler()._game._deck._cards
    print (the_cards)
    print(set(the_cards) == set(range(52)))
