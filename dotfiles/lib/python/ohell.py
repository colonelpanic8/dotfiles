import collections
import random
import pprint

do_print = False

def maybe_print(*args):
    if do_print:
        print(*args)


def falling_factorial(n, k):
    current = n
    product = 1
    for _ in range(k):
        product *= current
        current -= 1


def one_hand_with_lead_odds(card, num_players, is_trump=False, revealed_card=6, revealed_in_suit=False):
    num_stronger_cards = 14 - card
    if is_trump:
        if card < revealed_card:
            num_stronger_cards -= 1
    elif is_trump is None:
        if revealed_in_suit:
            if card < revealed_card:
                num_stronger_cards -= 1
    else:
        num_stronger_cards += 12

    odds_no_stronger_card_out = 1.0

    num_cards_remaining = 50
    num_weaker_cards_remaining = num_cards_remaining - num_stronger_cards

    for i in range(num_players-1):
        odds_no_stronger_card_out *= num_weaker_cards_remaining/num_cards_remaining
        num_weaker_cards_remaining -= 1
        num_cards_remaining -= 1

    return odds_no_stronger_card_out


def expected_value_of_one_bid(*args, **kwargs):
    win_prob = one_hand_with_lead_odds(*args, **kwargs)
    one_ev = win_prob + ((1 - win_prob) * -1)
    zero_ev = -win_prob

    return (one_ev - zero_ev, win_prob, one_ev, zero_ev)


def odds_for_number_of_players(num_players, is_trump=False):
    for card in range(2, 15):
        print(card)
        print(expected_value_of_one_bid(card, num_players, is_trump=is_trump))


def random_permutation_of_size(n):
    remaining = list(range(n))
    for i in range(n-1, -1, -1):
        yield remaining.pop(random.randint(0, i))


def random_permutation(the_list):
    for index in random_permutation_of_size(len(the_list)):
        yield the_list[index]


deck_of_cards = [
    (number, suit)
    for number in range(2, 15)
    for suit in range(4)
]


def compare_cards(card1, card2, trump_suit, led_suit):
    card1value, card1suit = card1
    card2value, card2suit = card2
    if card1suit == card2suit and card1suit in (trump_suit, led_suit):
        return card1value - card2value

    if card1suit == trump_suit:
        return 1

    if card2suit == trump_suit:
        return -1

    if card1suit == led_suit:
        return 1

    if card2suit == led_suit:
        return -1

    return 0


lost_hands = collections.defaultdict(int)
won_hands = collections.defaultdict(int)


performance_by_player_card = collections.defaultdict(
    lambda: collections.defaultdict(lambda: collections.defaultdict(lambda: collections.defaultdict(int)))
)


def play_hand(player_order, player_to_strategy, player_scores, score_first_only=True):
    bids = {}
    shuffled_deck = list(random_permutation(deck_of_cards))
    trump_card = shuffled_deck[len(player_order)]
    trump_suit = None if trump_card[0] in [11, 12, 13] else trump_card[1]
    led_suit = shuffled_deck[0][1]
    highest_card = (0, led_suit)
    winning_player = player_order[0]
    maybe_print("Trump is {}, {} leads".format(trump_suit, player_order[0]))
    if (trump_suit is not None and shuffled_deck[0][1] != trump_suit and
        shuffled_deck[0][0] <= 8 and shuffled_deck[0][0] > 5):
        # import ipdb; ipdb.set_trace()
        pass
    for player, card in zip(player_order, shuffled_deck):
        if compare_cards(card, highest_card, trump_suit, led_suit) > 0:
            highest_card = card
            winning_player = player
        bids[player] = player_to_strategy[player](card, trump_card, bids, len(player_order))
        maybe_print("{} got {} and bid {}".format(player, card, bids[player]))
    maybe_print("{} won the hand with {}".format(winning_player, highest_card))

    for player in player_order:
        bid = bids[player]
        score = 0
        if winning_player == player:
            if bid == 1:
                score = 1
                won_hands[player] += 1
            else:
                score = -1
                lost_hands[player] += 1
        else:
            if bid != 0:
                score = -1
                lost_hands[player] += 1

        player_scores[player] += score

        trump_of_card = None if trump_suit is None else shuffled_deck[0][1] == trump_suit
        performance_dict = performance_by_player_card[player][trump_of_card][shuffled_deck[0][0]]
        performance_dict[score] += 1
        performance_dict["total"] += score

        if score_first_only:
            break


def optimal_strategy(card, trump_card, bids, number_of_players):
    trump_suit = None if trump_card[0] in [11, 12, 13] else trump_card[1]
    card_is_trump = card[1] == trump_card[1]
    if trump_suit == None:
        card_is_trump = None

    if len(bids) == 0:
        odds = expected_value_of_one_bid(
            card[0], number_of_players, is_trump=card_is_trump,
            revealed_card=trump_card[0], revealed_in_suit=card[1] == trump_card[1]
        )
        return 1 if odds[0] > 0 else 0

    if card_is_trump:
        return handle_non_first_bid(card, trump_card, bids, number_of_players)
    else:
        return 0


def handle_non_first_bid(card, trump_card, bids, number_of_players):
    count_greater = 14 - card[0]
    count_smaller = card[0] - 2

    if trump_card[0] < card[0]:
        count_smaller -= 1
    else:
        count_greater -= 1

    odds_of_random_trump_smaller = float(count_smaller) / (count_smaller + count_greater)
    bid_sum = sum(bids.values())

    if bid_sum == 0:
        return 1
    elif bid_sum == 1 and odds_of_random_trump_smaller > (float(1)/3):
        return 1
    elif bid_sum == 2 and count_greater <= 3:
        return 1
    elif count_greater <= 1:
        return 1

    return 0


def mom_strat(card, trump_card, bids, number_of_players):
    trump_suit = None if trump_card[0] in [11, 12, 13] else trump_card[1]
    card_is_trump = card[1] == trump_card[1]
    if trump_suit == None:
        card_is_trump = None

    if len(bids) > 0:
        if card_is_trump:
            return handle_non_first_bid(card, trump_card, bids, number_of_players)
        return 0

    if trump_suit == None:
        if card[0] >= 5:
            return 1

    if card_is_trump:
        return 1

    if card[0] >= 13:
        return 1

    return 0


player_strategies = {
    "optimal": optimal_strategy,
    "mom1": mom_strat,
    "mom2": mom_strat,
    "mom3": mom_strat,
}


def simulate_hands(strats, number_of_hands):
    player_order = list(player_strategies.keys())
    scores = collections.defaultdict(int)
    for _ in range(number_of_hands):
        play_hand(player_order, strats, scores)
        player_order = player_order[-1:] + player_order[:-1]

    print(scores)
    return scores


odds_for_number_of_players(4, is_trump=False)
print("score")
scores = simulate_hands(player_strategies, 100000)
highest_score = -10000000
winner = None
for player, score in scores.items():
    if score > highest_score:
        highest_score = score
        winner = player
print("{} won with {}".format(winner, highest_score))
print("won")
print(won_hands)
print("lost")
print(lost_hands)

# for player, by_trump in performance_by_player_card.items():
#     print(player)
#     for trump_type, card_to_score in by_trump.items():
#         print("{}: {}".format(trump_type, sum(card_to_score.values())))

pprint.pprint(performance_by_player_card["optimal"][None])
pprint.pprint(performance_by_player_card["mom1"][None])
